/// KASINO — MMCC port — the Gameplay screen.
///
/// This is where the experiment lives. In the ImSim project the whole game is a
/// soup of module-level mutable `AppState` fields driven imperatively from a
/// per-frame `Process` loop. Here the *exact same* domain layer (Kasino.Domain,
/// shared verbatim) is driven from an immutable MMCC model:
///
///   • Model    — `Gameplay`: the full match state + an explicit phase.
///   • Message  — pure transitions of that model (the phase machine).
///   • Command  — world side-effects (audio, publishing the quit event).
///   • Content  — a declarative description of the board, derived from the model.
///
/// The phase machine that the ImSim version expresses as scattered timer
/// mutations inside `Process` is, here, a single total function over `Phase`
/// handled in `TimeUpdate`. That is the denotational vs operational contrast the
/// engine author was pointing at, made concrete in one game.
namespace Kasino
open System
open System.Numerics
open Prime
open Nu
open Kasino.Domain

// ─── Phase (mirrors the ImSim GameScreen.Phase) ───────────────────────
[<Struct>]
type GamePhase =
    | Shuffling
    | Dealing
    | WaitingForHuman
    | ComputerThinking
    | AnimatingPlay
    | ChoosingCaptureOption
    | RoundOver
    | GameOver

// ─── Capture preview (table-card tinting while a hand card is selected) ─
type CapturePreview =
    | NoCapture
    | SingleCapture of definite: Card list
    | MultipleCaptures of definite: Card list * possible: Card list

// ─── Animation specs (interpolated by Content from PhaseTicks) ─────────
// The denotational counterpart to the ImSim project's per-frame mutation: the
// model records WHAT moves and where; Content is a pure function of the clock.
type CardAnim =
    { Card: Card; FromX: float32; FromY: float32; ToX: float32; ToY: float32 }

type CollectAnim =
    { Cards: (Card * float32 * float32) list   // card + its source (x, y) on the table
      ToX: float32; ToY: float32 }

// ─── Deal animation: a sequence of steps, each sliding N backs from the deck ──
[<Struct>]
type DealTarget =
    | DealToTable
    | DealToSeat of int

type DealStep =
    { Target: DealTarget; Count: int; ToX: float32; ToY: float32 }

// ─── The MMCC model: a whole Kasino match ─────────────────────────────
type Gameplay =
    { Active: bool                                  // false = empty/unselected
      Config: GameEngine.GameConfig
      State: GameEngine.GameState
      Phase: GamePhase
      PhaseTicks: int64                             // engine updates since entering Phase
      RoundNumber: int
      CumulativeScores: Map<string, int>
      Back: string                                  // chosen card-back asset name
      SelectedCardIndex: int option
      CapturePreview: CapturePreview
      CaptureOptions: Rules.CaptureOption list
      CapturePage: int                              // current page of the capture modal (it paginates)
      LastPlayMessage: string
      ShowRecentPlays: bool                         // "Last moves" popup listing the other seats' recent plays
      LastChat: string
      LastEval: AI.PlayEvaluation option
      ScoreBreakdowns: (Player * Scoring.ScoreBreakdown) list
      Carry: Scoring.CarryOver             // undistributed most-cards/most-spades pot from tied rounds
      DealerOffset: int                    // per-game random dealer shift (starter = engine rotation + offset)
      PlayAnim: CardAnim option            // card sliding hand → table
      CollectAnim: CollectAnim option      // captured cards sliding table → player
      DealSteps: DealStep list             // current deal's slide sequence
      DragIndex: int option                // hand card currently being dragged (drag & drop)
      DragPos: single * single             // pointer position (world coords) while dragging
      TableDragCard: Card option           // table card being nudged (drag to reposition)
      TableOffsets: Map<Card, single * single> } // per-card manual nudges within the table area

    /// The engine synchronizes a screen's Content at registration, before any
    /// match exists, so the empty state carries two placeholder seats: the
    /// Content indexes Players (shown seat, current player) unconditionally.
    static member private emptyState : GameEngine.GameState =
        let placeholder = { Name = ""; Type = Computer; Hand = []; CapturedCards = []; Sweeps = 0 }
        { Players = [ placeholder; placeholder ]
          Table = []
          Deck = []
          CurrentPlayerIndex = 0
          DealRound = 0
          TotalDeals = 0
          LastCapturer = None
          RecentPlays = []
          Variant = StandardKasino
          SweepsFrozen = false }

    /// Unutilized model (screen not selected).
    static member empty =
        { Active = false
          Config = ({ Variant = StandardKasino; Seats = GameEngine.SeatCount.ofIntOrDefault 2; HumanCount = 1
                      Seed = None; TargetScore = 16; Settings = Settings.defaultSettings } : GameEngine.GameConfig)
          State = Gameplay.emptyState
          Phase = Shuffling
          PhaseTicks = 0L
          RoundNumber = 1
          CumulativeScores = Map.empty
          Back = "back1"
          SelectedCardIndex = None
          CapturePreview = NoCapture
          CaptureOptions = []
          CapturePage = 0
          LastPlayMessage = ""
          ShowRecentPlays = false
          LastChat = ""
          LastEval = None
          ScoreBreakdowns = []
          Carry = Scoring.CarryOver.zero
          DealerOffset = 0
          PlayAnim = None
          CollectAnim = None
          DealSteps = []
          DragIndex = None
          DragPos = (0.0f, 0.0f)
          TableDragCard = None
          TableOffsets = Map.empty }

    /// Initial model for a fresh match with the given configuration.
    static member start (config: GameEngine.GameConfig) =
        let rng = Random ()
        let back =
            if config.Settings.RandomCardBacks then $"back{rng.Next 3 + 1}"
            else "back1"
        let players = GameEngine.createPlayers config
        // The dealer is randomized per game: the offset shifts the engine's
        // round-by-round starter rotation by a per-game random amount.
        let dealerOffset = rng.Next(List.length players)
        let state = GameEngine.newRound config rng players 1
        let state = { state with CurrentPlayerIndex = (state.CurrentPlayerIndex + dealerOffset) % List.length players }
        let state = GameEngine.dealRound state true
        { Gameplay.empty with
            Active = true
            Config = config
            State = { state with DealRound = 1 }
            Phase = Shuffling
            RoundNumber = 1
            CumulativeScores = players |> List.map (fun p -> p.Name, 0) |> Map.ofList
            Back = back
            DealerOffset = dealerOffset
            LastPlayMessage = "Round 1 - Deal 1"
            ShowRecentPlays = false }

// ─── Messages (pure model transitions) and Commands (side-effects) ────
type GameplayMessage =
    | TimeUpdate
    | SelectCard of int
    | PlaySelectedCard
    /// Standard only: place a capture-capable card without capturing
    | PlaceSelectedCard
    | ChooseCapture of int
    /// dismiss the capture modal without playing
    | CancelCapture
    /// advance the paginated capture modal
    | CapturePageNext
    | KeyPressed of KeyboardKey
    | PointerDown            // left mouse down — grab a hand card (drag & drop)
    /// mouse moved with button held — move the dragged card
    | PointerDrag
    /// left mouse up — drop on the table to play, else just select
    | PointerUp
    /// toggle the "Last moves" popup (what the other seats did since your play)
    | ToggleRecentPlays
    | Ignore
    interface Message

type GameplayCommand =
    /// publish QuitEvent → top-level game returns to menu
    | RequestQuit
    /// publish HelpEvent → open rules, return to the game
    | RequestHelp
    /// publish ShowScoresEvent → top-level game shows scores
    | ShowScoresCmd
    /// audio side-effects
    | PlayCaptureSound
    | PlaySweepSound
    | PlayPlaceSound
    interface Command

// ─── Screen API extension ─────────────────────────────────────────────
[<AutoOpen>]
module GameplayExtensions =
    type Screen with
        member this.GetGameplay world = this.GetModelGeneric<Gameplay> world
        member this.SetGameplay value world = this.SetModelGeneric<Gameplay> value world
        member this.Gameplay = this.ModelGeneric<Gameplay> ()
        member this.QuitEvent = Events.QuitEvent --> this
        member this.ShowScoresEvent = Events.ShowScoresEvent --> this
        member this.HelpEvent = Events.HelpEvent --> this

// ─── Phase-machine timing (engine updates @ 60/s) ─────────────────────
[<RequireQualifiedAccess>]
module private Ticks =
    [<Literal>]
    let shuffle = 36L         // 0.6s
    [<Literal>]
    let think = 48L           // 0.8s
    let slide = 15L           // 0.25s — played card slides hand → table
    [<Literal>]
    let collectStart = 15L    // collect begins as the slide ends
    [<Literal>]
    let collectDur = 21L      // 0.35s — captured cards slide table → player
    [<Literal>]
    let anim = 60L            // 1.0s total AnimatingPlay (slide + collect + brief hold)
    [<Literal>]
    let dealStep = 11L        // 0.18s per deal step (one slide of N backs)

    /// Ease-out interpolation factor for elapsed/duration.
    let eased (elapsed: int64) (duration: int64) =
        let t = min 1.0f (float32 elapsed / float32 duration)
        1.0f - (1.0f - t) * (1.0f - t)

// ─── Pure transition helpers (public so the top-level game can resume a round) ─
module GameplayLogic =

    /// Capture preview for the hand card at index i against the current table.
    let previewFor (gp: Gameplay) (i: int) =
        let seat = gp.State.CurrentPlayerIndex
        let hand = gp.State.Players[seat].Hand
        if i < 0 || i >= List.length hand then (NoCapture, [])
        else
            let card = hand[i]
            let options = Rules.findCaptureOptions card gp.State.Table
            match options with
            | [] -> (NoCapture, [])
            | [ single ] -> (SingleCapture single.Captured, options)
            | many ->
                let sets = many |> List.map (fun o -> Set.ofList o.Captured)
                let definite = Set.intersectMany sets |> Set.toList
                let union = Set.unionMany sets
                let possible = Set.difference union (Set.ofList definite) |> Set.toList
                (MultipleCaptures (definite, possible), options)

    /// Human-readable summary of a just-completed turn.
    let describeTurn (preState: GameEngine.GameState) (tr: GameEngine.TurnResult) =
        let name = preState.Players[preState.CurrentPlayerIndex].Name
        match tr.PlayResult with
        | Capture (hc, captured, sweep) ->
            let tail = if sweep then " — SWEEP!" else ""
            $"{name} played {Cards.display hc}, captured {List.length captured} card(s){tail}"
        | Place hc ->
            $"{name} placed {Cards.display hc}"

    /// Optional table-talk for a computer turn.
    let chatFor (gp: Gameplay) (tr: GameEngine.TurnResult) =
        if not gp.Config.Settings.ChatEnabled then ""
        else
            let mood =
                match tr.PlayResult with
                | Capture (_, _, true) -> Chat.Sweep
                | Capture _ -> Chat.Capture
                | Place _ -> Chat.Place
            Chat.pick (int gp.State.CurrentPlayerIndex + gp.RoundNumber) mood

    /// Score the just-finished round and advance to RoundOver/GameOver.
    let enterRoundOver (gp: Gameplay) =
        let finalState = GameEngine.endRound gp.State
        // Tied most-cards/most-spades points ride the carry-over pot into the
        // next round; an outright winner collects the whole pot.
        let breakdowns, carryOut = Scoring.calculateScoresCarry gp.Carry finalState.Players
        let cumulative =
            breakdowns
            |> List.fold (fun acc (p, s) ->
                let prev = Map.tryFind p.Name acc |> Option.defaultValue 0
                Map.add p.Name (prev + s.Total) acc)
                gp.CumulativeScores
        let gameOver = cumulative |> Map.exists (fun _ v -> v >= gp.Config.TargetScore)
        { gp with
            State = finalState
            ScoreBreakdowns = breakdowns
            CumulativeScores = cumulative
            Carry = carryOut
            Phase = (if gameOver then GameOver else RoundOver)
            PhaseTicks = 0L
            SelectedCardIndex = None
            CapturePreview = NoCapture
            LastChat = ""
            LastPlayMessage =
                // The last capturer takes whatever remained on the table
                // (endRound above) — say so, since the cards leave silently.
                if gameOver then "Game over!"
                else
                    match gp.State.LastCapturer, gp.State.Table with
                    | Some idx, (_ :: _ as rest) ->
                        $"{gp.State.Players[idx].Name} takes the rest of the table ({List.length rest} cards)"
                    | _ -> $"Round {gp.RoundNumber} complete" }

    /// Build the deal-animation step sequence. The first deal also lays 4 cards
    /// on the table; every deal gives each seat 2 cards, twice.
    let buildDealSteps (state: GameEngine.GameState) (isFirst: bool) =
        let bottomSeat = 0   // fixed viewpoint: seat 0 is the bottom seat, also in watch mode
        let seatDest seat =
            match Ly.seatOf state.Players.Length bottomSeat seat with
            | Ly.SeatBottom -> (0.0f, Ly.handY)
            | Ly.SeatTop -> (0.0f, Ly.topOppY)
            | Ly.SeatLeft -> (Ly.sideLeftX, 0.0f)
            | Ly.SeatRight -> (Ly.sideRightX, 0.0f)
        // Deal like at a real table: two passes of 2 cards to each player —
        // starting with the player next to the dealer (the wave's first to
        // act, CurrentPlayerIndex) and proceeding clockwise, dealer last —
        // and on the first deal each pass ends with 2 cards to the table.
        [ for _ in 1 .. 2 do
            for k in 0 .. state.Players.Length - 1 do
                let seat = (state.CurrentPlayerIndex + k) % state.Players.Length
                let (dx, dy) = seatDest seat
                { Target = DealToSeat seat; Count = 2; ToX = dx; ToY = dy }
            if isFirst then
                { Target = DealToTable; Count = 2; ToX = 0.0f; ToY = Ly.tableY } ]

    /// Decide what happens at the start of a turn: deal the next deal, finish the
    /// round, or hand control to whoever is up next.
    let enterTurn (gp: Gameplay) =
        let st = gp.State
        if GameEngine.allHandsEmpty st then
            if List.isEmpty st.Deck then enterRoundOver gp
            else
                let st2 = GameEngine.dealRound st false
                let st2 = { st2 with DealRound = st.DealRound + 1 }
                { gp with
                    State = st2
                    Phase = Dealing
                    PhaseTicks = 0L
                    DealSteps = buildDealSteps st2 false
                    LastPlayMessage = $"Round {gp.RoundNumber} - Deal {st2.DealRound}" }
        else
            match st.Players[st.CurrentPlayerIndex].Type with
            | Human ->
                { gp with Phase = WaitingForHuman; PhaseTicks = 0L
                          SelectedCardIndex = None; CapturePreview = NoCapture }
            | Computer ->
                { gp with Phase = ComputerThinking; PhaseTicks = 0L }

    /// Apply a resolved turn, computing the slide/collect animation specs, and
    /// enter the play animation.
    let applyTurn (gp: Gameplay) (tr: GameEngine.TurnResult) =
        let seat = gp.State.CurrentPlayerIndex
        let mover = gp.State.Players[seat]
        let preHand = mover.Hand
        let preTable = gp.State.Table
        let playedCard = tr.Evaluation.HandCard
        let scatter = gp.Config.Settings.DefaultScatter
        // The viewpoint is fixed (seat 0 at the bottom, also in watch mode),
        // so each player's animations anchor to its own seat.
        let tableSeat = Ly.seatOf gp.State.Players.Length 0 seat
        // where the played card slides FROM
        let fromX, fromY =
            match tableSeat with
            | Ly.SeatBottom ->
                let idx = preHand |> List.tryFindIndex ((=) playedCard) |> Option.defaultValue 0
                Ly.handPos (List.length preHand) idx
            | Ly.SeatLeft | Ly.SeatRight ->
                let idx = preHand |> List.tryFindIndex ((=) playedCard) |> Option.defaultValue 0
                let step = Ly.cardH / 3.0f
                let top = float32 (List.length preHand - 1) * step / 2.0f
                let x = if tableSeat = Ly.SeatLeft then Ly.sideLeftX else Ly.sideRightX
                (x, top - float32 idx * step)
            | Ly.SeatTop -> (0.0f, Ly.topOppY)
        // play + collect specs depend on whether this was a capture or a placement
        let playAnim, collectAnim =
            match tr.PlayResult with
            | Place placed ->
                let toX, toY = Ly.tableCardPosFor scatter tr.NewState.Table placed
                Some { Card = playedCard; FromX = fromX; FromY = fromY; ToX = toX; ToY = toY }, None
            | Capture (_, captured, _) ->
                let play = Some { Card = playedCard; FromX = fromX; FromY = fromY; ToX = 0.0f; ToY = Ly.tableY }
                let destX, destY =
                    match tableSeat with
                    | Ly.SeatBottom -> 0.0f, Ly.handY - 40.0f
                    | Ly.SeatTop    -> 0.0f, Ly.topOppY + 40.0f
                    | Ly.SeatLeft   -> Ly.sideLeftX - 40.0f, 0.0f
                    | Ly.SeatRight  -> Ly.sideRightX + 40.0f, 0.0f
                let cards =
                    captured |> List.map (fun c ->
                        let cx, cy = Ly.tableCardPosFor scatter preTable c
                        // honour any manual nudge so the card slides from where it sat
                        let ox, oy = gp.TableOffsets |> Map.tryFind c |> Option.defaultValue (0.0f, 0.0f)
                        (c, cx + ox, cy + oy))
                play, Some { Cards = cards; ToX = destX; ToY = destY }
        { gp with
            State = tr.NewState
            LastEval = Some tr.Evaluation
            LastPlayMessage = describeTurn gp.State tr
            LastChat = chatFor gp tr
            Phase = AnimatingPlay
            PhaseTicks = 0L
            SelectedCardIndex = None
            CapturePreview = NoCapture
            CaptureOptions = []
            PlayAnim = playAnim
            CollectAnim = collectAnim }

    /// Deal and begin the next round of the match.
    let startNextRound (gp: Gameplay) =
        let rng = Random ()
        let roundNo = gp.RoundNumber + 1
        let state = GameEngine.newRound gp.Config rng gp.State.Players roundNo
        // 10-point freeze: sweeps stop scoring once anyone has 10+ points.
        let state = { state with SweepsFrozen = gp.CumulativeScores |> Map.exists (fun _ s -> s >= 10) }
        let state = { state with CurrentPlayerIndex = (state.CurrentPlayerIndex + gp.DealerOffset) % List.length state.Players }
        let state = GameEngine.dealRound state true
        { gp with
            State = { state with DealRound = 1 }
            RoundNumber = roundNo
            Phase = Shuffling
            PhaseTicks = 0L
            ScoreBreakdowns = []
            SelectedCardIndex = None
            CapturePreview = NoCapture
            LastChat = ""
            TableDragCard = None
            TableOffsets = Map.empty
            LastPlayMessage = $"Round {roundNo} - Deal 1" }

// ─── Dispatcher ───────────────────────────────────────────────────────
type GameplayDispatcher () =
    inherit ScreenDispatcher<Gameplay, GameplayMessage, GameplayCommand> (Gameplay.empty)

    override this.GetFallbackModel (_, _, _) =
        // The real model is installed by the top-level game when a match starts
        // (see KasinoGame's StartGame command); the fallback is just the empty one.
        Gameplay.empty

    override this.Definitions (_, _) =
        // NB: no DeselectingEvent reset — the model must persist across the
        // detour to the Scores screen so the match can resume on "Next Round".
        [Screen.TimeUpdateEvent => TimeUpdate
         Game.MouseLeftDownEvent => PointerDown
         Game.MouseDragEvent => PointerDrag
         Game.MouseLeftUpEvent => PointerUp
         Game.KeyboardKeyDownEvent =|> fun evt ->
            if not evt.Data.Repeated then KeyPressed evt.Data.KeyboardKey else Ignore]

    override this.Message (gameplay, message, _, world) =
        // ── drag & drop helpers (hit-testing in world coords) ──
        let players = gameplay.State.Players
        let humanSeatOpt = players |> List.tryFindIndex (fun p -> p.Type = Human)
        // index of the resting hand card under the pointer, if any
        let handHitTest (p: Vector2) =
            match humanSeatOpt with
            | Some seat ->
                let count = List.length players[seat].Hand
                seq { 0 .. count - 1 }
                |> Seq.tryFind (fun i ->
                    let hx, hy = Ly.handPos count i
                    abs (p.X - hx) <= Ly.cardW / 2.0f && abs (p.Y - hy) <= Ly.cardH / 2.0f)
            | None -> None
        let overTable (p: Vector2) =
            abs p.X <= Ly.tableW / 2.0f && abs (p.Y - Ly.tableY) <= Ly.tableH / 2.0f
        // the topmost table card under the pointer (only while the table is static),
        // accounting for any manual nudge already applied to it
        let tableStatic =
            match gameplay.Phase with
            | WaitingForHuman | ComputerThinking | ChoosingCaptureOption -> true
            | _ -> false
        let tableHitTest (p: Vector2) =
            if not tableStatic then None
            else
                let scatter = gameplay.Config.Settings.DefaultScatter
                gameplay.State.Table
                |> List.indexed
                |> List.rev   // last drawn is on top
                |> List.tryPick (fun (_, card) ->
                    let bx, by = Ly.tableCardPosFor scatter gameplay.State.Table card
                    let ox, oy = gameplay.TableOffsets |> Map.tryFind card |> Option.defaultValue (0.0f, 0.0f)
                    if abs (p.X - (bx + ox)) <= Ly.cardW / 2.0f && abs (p.Y - (by + oy)) <= Ly.cardH / 2.0f
                    then Some card else None)
        // clamp a table card's centre so it stays within the central table space
        let clampToTable (cx: single) (cy: single) =
            let maxX = Ly.tableW / 2.0f - Ly.cardW / 2.0f
            let dy = Ly.tableH / 2.0f - Ly.cardH / 2.0f
            (min maxX (max -maxX cx), min (Ly.tableY + dy) (max (Ly.tableY - dy) cy))
        // play the human's hand card at index i (mirrors PlaySelectedCard), clearing any drag
        let playHandIndex i =
            let preview, options = GameplayLogic.previewFor gameplay i
            match options with
            | _ :: _ :: _ ->
                // overlapping captures — let the player pick in the modal
                just { gameplay with SelectedCardIndex = Some i; CapturePreview = preview; CaptureOptions = options
                                     CapturePage = 0; Phase = ChoosingCaptureOption; PhaseTicks = 0L; DragIndex = None }
            | opts ->
                let chosen = match opts with [ o ] -> Some o | _ -> None
                let tr = GameEngine.playHumanTurn gameplay.State i chosen
                let gp = { GameplayLogic.applyTurn gameplay tr with DragIndex = None }
                match tr.PlayResult with
                | Capture (_, _, true) -> withSignal PlaySweepSound gp
                | Capture _ -> withSignal PlayCaptureSound gp
                | Place _ -> withSignal PlayPlaceSound gp
        match message with
        | TimeUpdate ->
            if not gameplay.Active then just gameplay
            else
                let gp = { gameplay with PhaseTicks = gameplay.PhaseTicks + world.GameDelta.Updates }
                match gp.Phase with
                | Shuffling ->
                    if gp.PhaseTicks >= Ticks.shuffle then
                        just { gp with Phase = Dealing; PhaseTicks = 0L; DealSteps = GameplayLogic.buildDealSteps gp.State true }
                    else just gp
                | Dealing ->
                    let total = int64 (List.length gp.DealSteps) * Ticks.dealStep
                    if gp.PhaseTicks >= total then just (GameplayLogic.enterTurn gp)
                    else just gp
                | ComputerThinking ->
                    if gp.PhaseTicks >= Ticks.think then
                        let style = GameEngine.computerStyle gp.Config gp.State.CurrentPlayerIndex
                        let tr = GameEngine.playComputerTurnStyled style gp.State
                        let gp2 = GameplayLogic.applyTurn gp tr
                        match tr.PlayResult with
                        | Capture (_, _, true) -> withSignal PlaySweepSound gp2
                        | Capture _ -> withSignal PlayCaptureSound gp2
                        | Place _ -> withSignal PlayPlaceSound gp2
                    else just gp
                | AnimatingPlay ->
                    if gp.PhaseTicks >= Ticks.anim then
                        let gp2 = GameplayLogic.enterTurn gp
                        match gp2.Phase with
                        | RoundOver | GameOver -> withSignal ShowScoresCmd gp2   // → top-level game shows Scores
                        | _ -> just gp2
                    else just gp
                | WaitingForHuman | ChoosingCaptureOption | RoundOver | GameOver ->
                    just gp

        | SelectCard i ->
            if gameplay.Phase = WaitingForHuman then
                let preview, options = GameplayLogic.previewFor gameplay i
                just { gameplay with SelectedCardIndex = Some i; CapturePreview = preview; CaptureOptions = options }
            else just gameplay

        | PlaySelectedCard ->
            match gameplay.Phase, gameplay.SelectedCardIndex with
            | WaitingForHuman, Some i ->
                match gameplay.CaptureOptions with
                | _ :: _ :: _ ->
                    // genuine choice between overlapping captures → let the player pick
                    just { gameplay with Phase = ChoosingCaptureOption; PhaseTicks = 0L; CapturePage = 0 }
                | options ->
                    let chosen = match options with [ o ] -> Some o | _ -> None
                    let tr = GameEngine.playHumanTurn gameplay.State i chosen
                    let gp = GameplayLogic.applyTurn gameplay tr
                    match tr.PlayResult with
                    | Capture (_, _, true) -> withSignal PlaySweepSound gp
                    | Capture _ -> withSignal PlayCaptureSound gp
                    | Place _ -> withSignal PlayPlaceSound gp
            | _ -> just gameplay

        | PlaceSelectedCard ->
            // Standard Kasino: capturing is optional, so a capture-capable card
            // may be placed instead — from the pre-play state or the modal.
            match gameplay.Phase, gameplay.SelectedCardIndex with
            | (WaitingForHuman | ChoosingCaptureOption), Some i when gameplay.Config.Variant = StandardKasino ->
                let tr = GameEngine.playHumanPlaceTurn gameplay.State i
                let gp = { GameplayLogic.applyTurn gameplay tr with DragIndex = None }
                withSignal PlayPlaceSound gp
            | _ -> just gameplay

        | ChooseCapture idx ->
            match gameplay.SelectedCardIndex with
            | Some i when idx >= 0 && idx < List.length gameplay.CaptureOptions ->
                let opt = gameplay.CaptureOptions[idx]
                let tr = GameEngine.playHumanTurn gameplay.State i (Some opt)
                let gp = GameplayLogic.applyTurn gameplay tr
                match tr.PlayResult with
                | Capture (_, _, true) -> withSignal PlaySweepSound gp
                | _ -> withSignal PlayCaptureSound gp
            | _ -> just gameplay

        | CancelCapture ->
            // Strict rules: the touched card must be played — no cancelling out.
            if gameplay.Phase = ChoosingCaptureOption && not gameplay.Config.Settings.StrictRules then
                just { gameplay with Phase = WaitingForHuman; PhaseTicks = 0L
                                     SelectedCardIndex = None; CapturePreview = NoCapture
                                     CaptureOptions = []; CapturePage = 0 }
            else just gameplay

        | CapturePageNext ->
            just { gameplay with CapturePage = gameplay.CapturePage + 1 }

        | KeyPressed key ->
            if gameplay.Active && key = KeyboardKey.Escape then
                // In the capture modal Escape backs out of the choice; anywhere
                // else it quits to the menu. (Previously Escape mid-choice
                // abandoned the whole match.)
                if gameplay.Phase = ChoosingCaptureOption then
                    just { gameplay with Phase = WaitingForHuman; PhaseTicks = 0L
                                         SelectedCardIndex = None; CapturePreview = NoCapture
                                         CaptureOptions = []; CapturePage = 0 }
                else withSignal RequestQuit gameplay
            else just gameplay

        | PointerDown ->
            let p = World.getMousePosition2dWorld false world
            // any click outside the "?" button (210,165 / 28x22) closes the
            // recent-moves panel; the button's own click toggles it
            let onRecentBtn = abs (p.X - 210.0f) <= 14.0f && abs (p.Y - 165.0f) <= 11.0f
            let gameplay = if gameplay.ShowRecentPlays && not onRecentBtn then { gameplay with ShowRecentPlays = false } else gameplay
            match (if gameplay.Phase = WaitingForHuman then handHitTest p else None) with
            | Some i ->
                // grab a hand card: select it (showing the capture preview) and begin a play-drag
                let preview, options = GameplayLogic.previewFor gameplay i
                just { gameplay with SelectedCardIndex = Some i; CapturePreview = preview; CaptureOptions = options
                                     DragIndex = Some i; DragPos = (p.X, p.Y) }
            | None ->
                // otherwise grab a TABLE card to nudge it into a clearer spot
                match tableHitTest p with
                | Some card -> just { gameplay with TableDragCard = Some card; DragPos = (p.X, p.Y) }
                | None -> just gameplay

        | PointerDrag ->
            let p = World.getMousePosition2dWorld false world
            match gameplay.DragIndex, gameplay.TableDragCard with
            | Some _, _ ->
                // hand card follows the cursor
                just { gameplay with DragPos = (p.X, p.Y) }
            | None, Some card ->
                // nudge the table card by the pointer delta, clamped to the table space
                let lx, ly = gameplay.DragPos
                let scatter = gameplay.Config.Settings.DefaultScatter
                let bx, by = Ly.tableCardPosFor scatter gameplay.State.Table card
                let ox, oy = gameplay.TableOffsets |> Map.tryFind card |> Option.defaultValue (0.0f, 0.0f)
                let cx, cy = clampToTable (bx + ox + (p.X - lx)) (by + oy + (p.Y - ly))
                just { gameplay with DragPos = (p.X, p.Y); TableOffsets = Map.add card (cx - bx, cy - by) gameplay.TableOffsets }
            | None, None -> just gameplay

        | PointerUp ->
            // drop a hand card over the table → play; otherwise just end the drag
            let ended = { gameplay with TableDragCard = None }
            match gameplay.Phase, gameplay.DragIndex with
            | WaitingForHuman, Some i ->
                let p = World.getMousePosition2dWorld false world
                if overTable p then playHandIndex i
                else just { ended with DragIndex = None }
            | _ -> just { ended with DragIndex = None }

        | ToggleRecentPlays -> just { gameplay with ShowRecentPlays = not gameplay.ShowRecentPlays }
        | Ignore -> just gameplay

    override this.Command (_, command, screen, world) =
        match command with
        | RequestQuit ->
            World.publish () screen.QuitEvent screen world
        | RequestHelp ->
            World.publish () screen.HelpEvent screen world
        | ShowScoresCmd ->
            World.publish () screen.ShowScoresEvent screen world
        // World.playSound distance panning volume sound. NOTE: this is the engine's
        // generic sound; dropping real card .wav files into Assets/Default and
        // referencing them here is the only step left for true card SFX.
        | PlaySweepSound ->
            World.playSound 0.0f 0.0f 1.0f Assets.Default.Sound world
        | PlayCaptureSound ->
            World.playSound 0.0f 0.0f 0.85f Assets.Default.Sound world
        | PlayPlaceSound ->
            World.playSound 0.0f 0.0f 0.5f Assets.Default.Sound world

    override this.Content (gameplay, _) =

        // deck style chosen on the menu for this match
        let style = gameplay.Config.Settings.CardStyle

        // ── derived presentation data ───────────────────────────────
        let st = gameplay.State
        let players = st.Players
        let humanSeatOpt = players |> List.tryFindIndex (fun p -> p.Type = Human)
        // Fixed viewpoint: in watch mode seat 0 occupies the bottom (its hand
        // rendered like a human's, minus interaction), and a spectated game
        // has nothing to hide so every hand is drawn face-up.
        let shownSeat = humanSeatOpt |> Option.defaultValue 0
        let interactive = gameplay.Phase = WaitingForHuman
        let elapsed = gameplay.PhaseTicks
        let animating = gameplay.Phase = AnimatingPlay
        // the played card is shown as the sliding AnimCard during the slide, so
        // its static copy on the table is hidden until the slide completes
        let hiddenCard =
            match animating, gameplay.PlayAnim with
            | true, Some a when elapsed < Ticks.slide -> Some a.Card
            | _ -> None
        let dealing = gameplay.Phase = Dealing
        let shuffling = gameplay.Phase = Shuffling
        // Progressive deal reveal: the game state is fully dealt before the
        // animation plays, so while dealing only the cards whose 2-card batch
        // has already landed are shown; the rest pop in step by step.
        let dealtSteps = if dealing then int (gameplay.PhaseTicks / Ticks.dealStep) else 0
        let dealVisible (target: DealTarget) (fullCount: int) =
            if not dealing then fullCount
            else
                let countFor sub =
                    sub |> List.filter (fun (s: DealStep) -> s.Target = target) |> List.sumBy (fun s -> s.Count)
                fullCount - countFor gameplay.DealSteps
                          + countFor (List.truncate dealtSteps gameplay.DealSteps)

        // capture-tint sets for the selected card
        let definiteSet, possibleSet =
            // Strict rules: capture candidates are not pre-highlighted.
            if gameplay.Config.Settings.StrictRules then Set.empty, Set.empty else
            match gameplay.CapturePreview with
            | NoCapture -> Set.empty, Set.empty
            | SingleCapture d -> Set.ofList d, Set.empty
            | MultipleCaptures (d, p) -> Set.ofList d, Set.ofList p
        let tintFor card =
            if Set.contains card definiteSet then Clr.tintGreen
            elif Set.contains card possibleSet then Clr.tintYellow
            else Clr.white

        // table cards laid out in centered rows that stay on-screen (the table
        // can accumulate many cards, so a single row would run off the edges).
        let scatter = gameplay.Config.Settings.DefaultScatter
        let tableContent =
            let count = dealVisible DealToTable (List.length st.Table)
            let visible = List.truncate count st.Table
            [ for i, card in List.indexed visible ->
                let bx, by = Ly.tableCardPosFor scatter visible card
                let ox, oy = gameplay.TableOffsets |> Map.tryFind card |> Option.defaultValue (0.0f, 0.0f)
                Content.staticSprite ("TableCard" + string i)
                    [Entity.Position := v3 (bx + ox) (by + oy) 0.0f
                     Entity.Size == v3 Ly.cardW Ly.cardH 0.0f
                     Entity.StaticImage := CardImg.cardAssetOf style card
                     Entity.Color := tintFor card
                     Entity.Visible := (Some card <> hiddenCard)
                     Entity.Elevation := (if gameplay.TableDragCard = Some card then 4.0f else 1.0f)] ]

        // the local human's hand (bottom). Rendered as plain sprites: selection and
        // play are driven by screen-level pointer hit-testing (see PointerDown/Up) so
        // that the same press can either select (click) or play (drag onto the table).
        // The card currently being dragged is hidden here and drawn at the cursor below.
        let handContent =
            let seat = shownSeat
            let hand = players[seat].Hand |> List.truncate (dealVisible (DealToSeat seat) (List.length players[seat].Hand))
            let count = List.length hand
            [ for i, card in List.indexed hand do
                    if gameplay.DragIndex <> Some i then
                        let hx, _ = Ly.handPos count i
                        let selected = gameplay.SelectedCardIndex = Some i
                        let lift = if selected then 18.0f else 0.0f
                        Content.staticSprite ("HandCard" + string i)
                            [Entity.Position := v3 hx (Ly.handY + lift) 0.0f
                             Entity.Size == v3 Ly.cardW Ly.cardH 0.0f
                             Entity.StaticImage := CardImg.cardAssetOf style card
                             Entity.Elevation == 2.0f] ]

        // the dragged card follows the cursor (drawn above everything but the modals)
        let dragContent =
            match gameplay.DragIndex, humanSeatOpt with
            | Some i, Some seat when gameplay.Phase = WaitingForHuman ->
                match List.tryItem i players[seat].Hand with
                | Some card ->
                    let dx, dy = gameplay.DragPos
                    [ Content.staticSprite "DragCard"
                        [Entity.Position := v3 dx dy 0.0f
                         Entity.Size == v3 Ly.cardW Ly.cardH 0.0f
                         Entity.StaticImage := CardImg.cardAssetOf style card
                         Entity.Elevation == 6.0f] ]
                | None -> []
            | _ -> []

        // opponents — their hands shown face-down (like the desktop Kasino): a fanned
        // row of card backs at the top for the first opponent, vertical stacks on the
        // left/right for additional opponents (3-4 player games), each with a label.
        let oppContent =
            // In a spectated (watch-mode) game the "backs" are drawn face-up.
            let cardImgFor (p: Player) (j: int) =
                if humanSeatOpt.IsSome then CardImg.handBackAssetOf style gameplay.Back else CardImg.cardAssetOf style p.Hand[j]
            let opponents = players |> List.indexed |> List.filter (fun (i, _) -> i <> shownSeat)
            [ for (i, p) in opponents do
                let handN = dealVisible (DealToSeat i) (List.length p.Hand)
                let labelCol = if st.CurrentPlayerIndex = i then Clr.gold else Clr.lightGray
                let summary = $"{p.Name}  —  hand {handN}, won {List.length p.CapturedCards}"
                match Ly.seatOf players.Length shownSeat i with
                | Ly.SeatTop | Ly.SeatBottom ->
                    // top opponent — fanned (slightly overlapping) backs, label beneath
                    let gap = -16.0f
                    let leftX = Ly.centerCardsX handN gap
                    for j in 0 .. handN - 1 do
                        let bx = leftX + Ly.cardW / 2.0f + float32 j * (Ly.cardW + gap)
                        yield Content.staticSprite ("OppTopCard" + string j)
                            [Entity.Position := v3 bx Ly.topOppY 0.0f
                             Entity.Size == v3 Ly.cardW Ly.cardH 0.0f
                             Entity.StaticImage := cardImgFor p j
                             Entity.Elevation == (1.0f + float32 j * 0.01f)]
                    yield Content.text "OppTopLabel"
                        [Entity.Position := v3 0.0f (Ly.topOppY - 36.0f) 0.0f
                         Entity.Size == v3 460.0f 14.0f 0.0f
                         Entity.Text := summary
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.TextColor := labelCol
                         Entity.FontSizing == Some 10.0f
                         Entity.Elevation == 2.0f]
                | sideSeat ->
                    // side opponents — vertical stack of backs hugging the screen edge
                    let side = if sideSeat = Ly.SeatLeft then 1 else 2
                    let x = if sideSeat = Ly.SeatLeft then Ly.sideLeftX else Ly.sideRightX
                    let step = Ly.cardH / 3.0f
                    let top = float32 (handN - 1) * step / 2.0f
                    for j in 0 .. handN - 1 do
                        yield Content.staticSprite ("OppSideCard" + string side + "_" + string j)
                            [Entity.Position := v3 x (top - float32 j * step) 0.0f
                             Entity.Size == v3 Ly.cardW Ly.cardH 0.0f
                             Entity.StaticImage := cardImgFor p j
                             Entity.Elevation == (1.0f + float32 j * 0.01f)]
                    yield Content.text ("OppSideLabel" + string side)
                        [Entity.Position := v3 x (top + 26.0f) 0.0f
                         Entity.Size == v3 130.0f 14.0f 0.0f
                         Entity.Text := $"{p.Name} ({handN})"
                         Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                         Entity.TextColor := labelCol
                         Entity.FontSizing == Some 9.0f
                         Entity.Elevation == 2.0f] ]

        // capture-choice modal — paginated (up to 64 options can exist, so an
        // unbounded column would run off-screen), with Cancel always available
        // and, in Standard Kasino, the option to decline the capture entirely.
        let captureModal =
            if gameplay.Phase <> ChoosingCaptureOption then []
            else
                let optionsPerPage = 5
                let optCount = List.length gameplay.CaptureOptions
                let pageCount = max 1 ((optCount + optionsPerPage - 1) / optionsPerPage)
                let page = ((gameplay.CapturePage % pageCount) + pageCount) % pageCount
                let pageStart = page * optionsPerPage
                let visible =
                    gameplay.CaptureOptions
                    |> List.indexed
                    |> List.skip pageStart
                    |> List.truncate optionsPerPage
                let allowPlace = gameplay.Config.Variant = StandardKasino
                [ Content.staticSprite "ModalBg"
                    [Entity.Position == v3 0.0f 0.0f 0.0f
                     Entity.Size == v3 640.0f 360.0f 0.0f
                     Entity.StaticImage == Assets.Default.White
                     Entity.Color == Clr.modalOverlay
                     Entity.Elevation == 6.0f]
                  Content.text "ModalPrompt"
                    [Entity.Position == v3 0.0f 90.0f 0.0f
                     Entity.Size == v3 460.0f 24.0f 0.0f
                     Entity.Text == "Choose which cards to capture:"
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.TextColor == Clr.white
                     Entity.FontSizing == Some 14.0f
                     Entity.Elevation == 7.0f]
                  for row, (i, opt) in List.indexed visible do
                    // Button carries no label of its own; the overlay texts
                    // below tint each card name by suit (red suits reddish,
                    // black suits gray) in fixed slots — Nu has no text
                    // measuring, hence the tabular layout.
                    let y = 56.0f - float32 row * 34.0f
                    Content.button ("CaptureOpt" + string i)
                        [Entity.Position := v3 0.0f y 0.0f
                         Entity.Size == v3 420.0f 28.0f 0.0f
                         Entity.Text == ""
                         Entity.Elevation == 7.0f
                         Entity.ClickEvent => ChooseCapture i]
                    Content.text ("CaptureOptPre" + string i)
                        [Entity.Position := v3 -180.0f y 0.0f
                         Entity.Size == v3 40.0f 24.0f 0.0f
                         Entity.Text := $"{i + 1})"
                         Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                         Entity.TextColor == Clr.white
                         Entity.FontSizing == Some 12.0f
                         Entity.Elevation == 7.5f]
                    for j, c in List.indexed (List.truncate 7 opt.Captured) do
                        let txt =
                            if j = 6 && opt.Captured.Length > 7 then "…"
                            else Cards.display c
                        let col =
                            if txt = "…" then Clr.white
                            else match c.Suit with Hearts | Diamonds -> Clr.cardRed | Spades | Clubs -> Clr.cardGray
                        Content.text ("CaptureOptCard" + string i + "_" + string j)
                            [Entity.Position := v3 (-134.0f + float32 j * 44.0f) y 0.0f
                             Entity.Size == v3 44.0f 24.0f 0.0f
                             Entity.Text := txt
                             Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                             Entity.TextColor := col
                             Entity.FontSizing == Some 12.0f
                             Entity.Elevation == 7.5f]
                    Content.text ("CaptureOptCount" + string i)
                        [Entity.Position := v3 178.0f y 0.0f
                         Entity.Size == v3 60.0f 24.0f 0.0f
                         Entity.Text := $"({opt.Captured.Length})"
                         Entity.Justification == Justified (JustifyRight, JustifyMiddle)
                         Entity.TextColor == Clr.white
                         Entity.FontSizing == Some 12.0f
                         Entity.Elevation == 7.5f]
                  if pageCount > 1 then
                    Content.button "CaptureMore"
                        [Entity.Position == v3 0.0f -114.0f 0.0f
                         Entity.Size == v3 420.0f 28.0f 0.0f
                         Entity.Text := $"More options ({page + 1}/{pageCount})"
                         Entity.Elevation == 7.0f
                         Entity.ClickEvent => CapturePageNext]
                  if allowPlace then
                    Content.button "CapturePlace"
                        [Entity.Position == v3 -105.0f -148.0f 0.0f
                         Entity.Size == v3 230.0f 28.0f 0.0f
                         Entity.Text == "Place instead"
                         Entity.Elevation == 7.0f
                         Entity.ClickEvent => PlaceSelectedCard]
                  // Strict rules: the touched card must be played — no Cancel.
                  if not gameplay.Config.Settings.StrictRules then
                    Content.button "CaptureCancel"
                        [Entity.Position == v3 120.0f -148.0f 0.0f
                         Entity.Size == v3 130.0f 28.0f 0.0f
                         Entity.Text == "Cancel"
                         Entity.Elevation == 7.0f
                         Entity.ClickEvent => CancelCapture] ]

        // (round-over / game-over now route to the dedicated Scores screen via
        // ShowScoresCmd — see the AnimatingPlay → enterTurn transition above)

        // animation sprites — pure interpolation of the model clock (PhaseTicks).
        // The played card slides hand → table over Ticks.slide; then any captured
        // cards slide from their table spots to the player over Ticks.collectDur.
        let animContent =
            if not animating then []
            else
                let playPart =
                    match gameplay.PlayAnim with
                    | Some a when elapsed < Ticks.slide ->
                        let e = Ticks.eased elapsed Ticks.slide
                        [ Content.staticSprite "AnimCard"
                            [Entity.Position := v3 (a.FromX + (a.ToX - a.FromX) * e) (a.FromY + (a.ToY - a.FromY) * e) 0.0f
                             Entity.Size == v3 Ly.cardW Ly.cardH 0.0f
                             Entity.StaticImage := CardImg.cardAssetOf style a.Card
                             Entity.Elevation == 5.0f] ]
                    | _ -> []
                let collectPart =
                    match gameplay.CollectAnim with
                    | Some c when elapsed >= Ticks.collectStart && elapsed < Ticks.collectStart + Ticks.collectDur ->
                        let e = Ticks.eased (elapsed - Ticks.collectStart) Ticks.collectDur
                        [ for ci, (card, fx, fy) in List.indexed c.Cards ->
                            Content.staticSprite ("ColC" + string ci)
                                [Entity.Position := v3 (fx + (c.ToX - fx) * e) (fy + (c.ToY - fy) * e) 0.0f
                                 Entity.Size == v3 Ly.cardW Ly.cardH 0.0f
                                 Entity.StaticImage := CardImg.cardAssetOf style card
                                 Entity.Elevation == (5.1f + float32 ci * 0.01f)] ]
                    | _ -> []
                playPart @ collectPart

        // deal animation — a deck stack plus the current step's backs sliding out
        let dealContent =
            if not dealing then []
            else
                let deckImg = CardImg.backAssetOf style gameplay.Back
                let deck =
                    [ for d in 0 .. 2 ->
                        Content.staticSprite ("Deck" + string d)
                            [Entity.Position == v3 0.0f (Ly.tableY + float32 d * 1.5f) 0.0f
                             Entity.Size == v3 Ly.cardW Ly.cardH 0.0f
                             Entity.StaticImage == deckImg
                             Entity.Elevation == (5.0f + float32 d * 0.005f)] ]
                let slide =
                    match List.tryItem (int (gameplay.PhaseTicks / Ticks.dealStep)) gameplay.DealSteps with
                    | Some step ->
                        let e = Ticks.eased (gameplay.PhaseTicks % Ticks.dealStep) Ticks.dealStep
                        [ for c in 0 .. step.Count - 1 ->
                            let spread = float32 (c - step.Count / 2) * 10.0f
                            Content.staticSprite ("DealC" + string c)
                                [Entity.Position := v3 ((step.ToX + spread) * e) (Ly.tableY + (step.ToY - Ly.tableY) * e) 0.0f
                                 Entity.Size == v3 Ly.cardW Ly.cardH 0.0f
                                 Entity.StaticImage == CardImg.handBackAssetOf style gameplay.Back
                                 Entity.Elevation == (5.2f + float32 c * 0.01f)] ]
                    | None -> []
                deck @ slide

        // shuffle visual — two halves of the deck riffling together toward center
        let shuffleContent =
            if not shuffling then []
            else
                let back = CardImg.handBackAssetOf style gameplay.Back
                let t = Ticks.eased gameplay.PhaseTicks Ticks.shuffle
                let sep = 55.0f * (1.0f - t)
                let wiggle = float32 (gameplay.PhaseTicks % 8L) - 4.0f
                [ for k in 0 .. 5 ->
                    let leftHalf = k % 2 = 0
                    let side = if leftHalf then -1.0f else 1.0f
                    let nudge = if leftHalf then -wiggle else wiggle
                    Content.staticSprite ("Shuf" + string k)
                        [Entity.Position := v3 (side * sep + nudge) (Ly.tableY + float32 (k / 2) * 2.0f) 0.0f
                         Entity.Size == v3 Ly.cardW Ly.cardH 0.0f
                         Entity.StaticImage == back
                         Entity.Elevation == (5.0f + float32 k * 0.01f)] ]

        // ── assemble the scene ──────────────────────────────────────
        [Content.group Simulants.GameplayScene.Name []
            [if gameplay.Active then
                // background (absolute, so it stays put if the 2D eye is ever
                // panned/zoomed for board animations)
                Content.staticSprite "Bg"
                    [Entity.Absolute == true
                     Entity.Position == v3 0.0f 0.0f 0.0f
                     Entity.Size == v3 640.0f 360.0f 0.0f
                     Entity.StaticImage == Assets.Default.White
                     Entity.Color == Clr.screenBg
                     Entity.Elevation == -1.0f]

                yield! oppContent
                // table and hand truncate themselves to the dealt-so-far counts
                if not shuffling then yield! tableContent
                if not shuffling then yield! handContent
                yield! dragContent
                yield! animContent
                yield! dealContent
                yield! shuffleContent

                // turn + status text
                // beside the hand (bottom-left), off the cards
                Content.text "TurnText"
                    [Entity.Position == v3 -215.0f Ly.handY 0.0f
                     Entity.Size == v3 200.0f 18.0f 0.0f
                     Entity.Text := $"{players[st.CurrentPlayerIndex].Name}'s turn"
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.TextColor == Clr.white
                     Entity.FontSizing == Some 12.0f
                     Entity.Elevation == 3.0f]
                Content.text "StatusText"
                    [Entity.Position == v3 0.0f Ly.statusY 0.0f
                     Entity.Size == v3 600.0f 18.0f 0.0f
                     Entity.Text := (if gameplay.LastChat <> "" then $"{gameplay.LastPlayMessage}    \"{gameplay.LastChat}\"" else gameplay.LastPlayMessage)
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.TextColor == Clr.lightGray
                     Entity.FontSizing == Some 11.0f
                     Entity.Elevation == 3.0f]

                // play button (only when a card is selected on the human's turn);
                // placed to the right of the hand so it never overlaps the status text
                if interactive && Option.isSome gameplay.SelectedCardIndex && gameplay.DragIndex |> Option.isNone then
                    Content.button "PlayBtn"
                        [Entity.Position == v3 210.0f Ly.handY 0.0f
                         Entity.Size == v3 120.0f 30.0f 0.0f
                         Entity.Text == "Play"
                         Entity.Elevation == 4.0f
                         Entity.ClickEvent => PlaySelectedCard]
                    // Standard Kasino: capturing is optional — offer to place the
                    // selected card instead when it could capture
                    if gameplay.Config.Variant = StandardKasino
                       && (match gameplay.CapturePreview with NoCapture -> false | SingleCapture _ | MultipleCaptures _ -> true) then
                        Content.button "PlaceBtn"
                            [Entity.Position == v3 210.0f (Ly.handY - 36.0f) 0.0f
                             Entity.Size == v3 120.0f 30.0f 0.0f
                             Entity.Text == "Place Instead"
                             Entity.FontSizing == Some 10.0f
                             Entity.Elevation == 4.0f
                             Entity.ClickEvent => PlaceSelectedCard]

                // always-available return-to-menu + help buttons (top-right)
                Content.button "MenuBtn"
                    [Entity.Position == v3 270.0f 165.0f 0.0f
                     Entity.Size == v3 90.0f 22.0f 0.0f
                     Entity.Text == "Menu"
                     Entity.Elevation == 5.0f
                     Entity.ClickEvent => RequestQuit]
                // "what did the others just do?" — popup over the top of the table
                Content.button "RecentBtn"
                    [Entity.Position == v3 210.0f 165.0f 0.0f
                     Entity.Size == v3 28.0f 22.0f 0.0f
                     Entity.Text == "?"
                     Entity.Elevation == 5.0f
                     Entity.ClickEvent => ToggleRecentPlays]
                if gameplay.ShowRecentPlays then
                    let recent = GameEngine.describeRecentPlays gameplay.State
                    Content.staticSprite "RecentBg"
                        [Entity.Position == v3 0.0f 40.0f 0.0f
                         Entity.Size := v3 540.0f (float32 recent.Length * 21.0f + 12.0f) 0.0f
                         Entity.StaticImage == Assets.Default.White
                         Entity.Color == color 0.0f 0.0f 0.0f 0.9f
                         Entity.Elevation == 6.0f]
                    for i, line in List.indexed recent do
                        Content.text ("RecentLine" + string i)
                            [Entity.Position := v3 0.0f (40.0f + float32 (recent.Length - 1) * 10.5f - float32 i * 21.0f) 0.0f
                             Entity.Size == v3 530.0f 21.0f 0.0f
                             Entity.Text := line
                             Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                             Entity.TextColor == Clr.gold
                             Entity.FontSizing == Some 14.0f
                             Entity.Elevation == 7.0f]
                Content.button "HelpBtn"
                    [Entity.Position == v3 170.0f 165.0f 0.0f
                     Entity.Size == v3 28.0f 22.0f 0.0f
                     Entity.Text == "i"
                     Entity.Elevation == 5.0f
                     Entity.ClickEvent => RequestHelp]

                yield! captureModal]]
