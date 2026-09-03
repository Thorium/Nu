/// KASINO — Finnish Card Game — Nu Engine Port
/// Replaces MonoGame UI (MenuScreen, GameScreen, ScoreScreen, RulesScreen,
/// KasinoGame, CardRenderer) with Nu engine equivalents using ImSim.
/// Domain files (Cards.fs, Combinations.fs, Rules.fs, Scoring.fs, AI.fs,
/// GameEngine.fs) are reused unchanged.
namespace Kasino
open System
open System.Numerics
open Prime
open Nu
open Kasino.Domain

// ─── Application Mode ─────────────────────────────────────────────────
[<Struct>]
type KasinoMode =
    | KasinoMenu
    | KasinoPlaying
    | KasinoScores
    | KasinoRules

// ─── Menu Step (mirrors MonoGame MenuScreen.MenuChoice) ───────────────
[<Struct>]
type MenuStep =
    | VariantSelect
    | PlayerCountSelect
    | HumanCountSelect

// ─── Game Phase (mirrors MonoGame GameScreen.Phase) ───────────────────
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

// ─── Card Animation ───────────────────────────────────────────────────
type CardAnimation =
    { AnimCard: Card
      FromX: float32; FromY: float32
      ToX: float32; ToY: float32
      Duration: float32 }

// ─── Deal Step (for deal animation) ───────────────────────────────────
type DealStep =
    { DealTargetLabel: string
      DealCardCount: int
      DealToX: float32; DealToY: float32
      DealIsFaceUp: bool }

// ─── Collect Animation: captured table cards slide toward the player ──
type CollectAnimation =
    { CollectCards: (Card * float32 * float32) list   // card, fromX, fromY
      CollectToX: float32; CollectToY: float32        // destination
      CollectStart: float32                            // when collect begins (after card slide)
      CollectDuration: float32 }                       // seconds for the slide

// ─── Capture Preview ──────────────────────────────────────────────────
type CapturePreview =
    | NoCapture
    | SingleCapture of definite: Card list
    | MultipleCaptures of definite: Card list * possible: Card list

// ─── Table Layout Mode ───────────────────────────────────────────────
[<Struct>]
type TableLayout =
    | StrictGrid
    | RandomScatter

// ─── Drag State for drag & drop ──────────────────────────────────────
type DragState =
    | NotDragging
    | Dragging of cardIndex: int * startPos: Vector2 * currentPos: Vector2
    /// nudging a table card (scatter mode)
    | DraggingTable of card: Card * grabOffsetX: float32 * grabOffsetY: float32

// ─── Game Property Extensions ─────────────────────────────────────────
[<AutoOpen>]
module KasinoExtensions =
    type Game with
        member this.GetKasinoMode world : KasinoMode = this.Get (nameof Game.KasinoMode) world
        member this.SetKasinoMode (value : KasinoMode) world = this.Set (nameof Game.KasinoMode) value world
        member this.KasinoMode = lens (nameof Game.KasinoMode) this this.GetKasinoMode this.SetKasinoMode

// ─── Module-level mutable state ───────────────────────────────────────
module AppState =

    // Menu state
    let mutable menuStep = VariantSelect
    let mutable menuVariant = StandardKasino
    let mutable menuPlayerCount = 2
    let mutable menuHumanCount = 1

    /// Player-configurable options. AI personalities + table-talk default ON
    /// here (the ported flavour features); toggled from the menu before a game.
    let mutable menuSettings : Settings.GameSettings =
        { Settings.defaultSettings with AiPersonalities = true; ChatEnabled = true }

    // Game state
    let mutable config : GameEngine.GameConfig =
        { Variant = StandardKasino; Seats = GameEngine.SeatCount.ofIntOrDefault 2; HumanCount = 1
          Seed = None; TargetScore = 16; Settings = menuSettings }
    let mutable gameState : GameEngine.GameState option = None
    let mutable phase = Dealing
    let mutable phaseTimer = 0.0f
    let mutable selectedCardIndex : int option = None
    let mutable hoveredCardIndex : int option = None
    let mutable capturePreview = NoCapture
    let mutable captureOptions : Rules.CaptureOption list = []
    let mutable captureCardIdx = 0
    /// Current page of the capture-option modal (6 options per page; up to 64
    /// options can exist, so the modal paginates).
    let mutable capturePage = 0
    let mutable lastPlayMessage = ""
    /// Latest table-talk line from a computer player (empty = none shown).
    let mutable lastChat = ""
    let mutable roundNumber = 1
    let mutable cumulativeScores : Map<string, int> = Map.empty
    /// Undistributed most-cards/most-spades pot from earlier tied rounds.
    let mutable carryOver = Scoring.CarryOver.zero
    /// Per-game random dealer shift: the first round's starter (the player
    /// next to the dealer) is drawn at game start instead of always seat 0.
    let mutable dealerOffset = 0
    let mutable rng = Random()
    let mutable lastEval : AI.PlayEvaluation option = None

    /// Number of available card-back designs (back1.png .. backN.png)
    [<Literal>]
    let backDesignCount = 3
    /// Asset name of the card back chosen for the current game (one of back1..backN)
    let mutable currentBack = "back1"

    // Layout & drag state
    let mutable tableLayout = RandomScatter
    let mutable dragState : DragState = NotDragging
    /// card -> (x, y, rotation)
    let mutable scatteredPositions : Map<Card, (float32 * float32 * float32)> = Map.empty

    // Animation state
    let mutable currentCardAnim : CardAnimation option = None
    let mutable currentCollectAnim : CollectAnimation option = None
    let mutable shuffleDuration = 0.6f
    let mutable cardSlideDuration = 0.25f
    let mutable collectSlideDuration = 0.35f
    [<Literal>]
    let dealStepDuration = 0.18f

    // Deal animation state
    let mutable dealSteps : DealStep list = []
    let mutable dealStepIndex = 0
    let mutable dealStepElapsed = 0.0f

    // Score state
    let mutable scoreBreakdowns : (Player * Scoring.ScoreBreakdown) list = []
    let mutable scoreIsGameOver = false

    // Rules state
    let mutable rulesPage = 0
    let mutable rulesReturnMode = KasinoMenu

    // Input guard: prevents Enter key from firing across multiple screens in the same frame
    let mutable enterConsumed = false

    [<Literal>]
    let computerDelay = 0.8f
    [<Literal>]
    let animDelay = 1.4f

    /// Reset menu to defaults
    let resetMenu () =
        menuStep <- VariantSelect
        menuVariant <- StandardKasino
        menuPlayerCount <- 2
        menuHumanCount <- 1

    /// Start a new game from current menu settings
    let startGame () =
        config <- { Variant = menuVariant; Seats = GameEngine.SeatCount.ofIntOrDefault menuPlayerCount
                    HumanCount = menuHumanCount; Seed = None; TargetScore = 16
                    Settings = menuSettings }
        rng <- Random()
        // Pick a random card-back design for this game (back1..backN), or back1
        // if random backs are disabled. Chosen once per game (whole deck).
        currentBack <-
            if menuSettings.RandomCardBacks then $"back{rng.Next(backDesignCount) + 1}"
            else "back1"
        tableLayout <- if menuSettings.DefaultScatter then RandomScatter else StrictGrid
        lastChat <- ""
        let players = GameEngine.createPlayers config
        cumulativeScores <- players |> List.map (fun p -> p.Name, 0) |> Map.ofList
        roundNumber <- 1
        carryOver <- Scoring.CarryOver.zero
        dealerOffset <- rng.Next(List.length players)
        let state = GameEngine.newRound config rng players 1
        // The dealer is randomized per game: dealerOffset shifts the engine's
        // round-by-round starter rotation by a per-game random amount.
        let state = { state with CurrentPlayerIndex = (state.CurrentPlayerIndex + dealerOffset) % List.length players }
        let state = GameEngine.dealRound state true
        gameState <- Some { state with DealRound = 1 }
        phase <- Shuffling
        phaseTimer <- 0.0f
        selectedCardIndex <- None
        hoveredCardIndex <- None
        capturePreview <- NoCapture
        lastPlayMessage <- "Round 1 - Deal 1"
        lastEval <- None
        dragState <- NotDragging
        scatteredPositions <- Map.empty
        currentCardAnim <- None
        currentCollectAnim <- None
        dealSteps <- []
        dealStepIndex <- 0
        dealStepElapsed <- 0.0f

    /// Start a new round (continuing a game)
    let startNextRound () =
        match gameState with
        | None -> ()
        | Some gs ->
            roundNumber <- roundNumber + 1
            let players = gs.Players
            let state = GameEngine.newRound config rng players roundNumber
            // 10-point freeze: sweeps stop scoring once anyone has 10+ points.
            let state = { state with SweepsFrozen = cumulativeScores |> Map.exists (fun _ s -> s >= 10) }
            let state = { state with CurrentPlayerIndex = (state.CurrentPlayerIndex + dealerOffset) % List.length players }
            let state = GameEngine.dealRound state true
            gameState <- Some { state with DealRound = 1 }
            phase <- Shuffling
            phaseTimer <- 0.0f
            selectedCardIndex <- None
            hoveredCardIndex <- None
            capturePreview <- NoCapture
            lastPlayMessage <- $"Round {roundNumber} - Deal 1"
            lastEval <- None
            dragState <- NotDragging
            scatteredPositions <- Map.empty
            currentCardAnim <- None
            currentCollectAnim <- None
            dealSteps <- []
            dealStepIndex <- 0
            dealStepElapsed <- 0.0f

    /// Enter score screen
    let enterScores () =
        match gameState with
        | None -> ()
        | Some gs ->
            let finalGs = GameEngine.endRound gs
            gameState <- Some finalGs
            // Tied most-cards/most-spades points ride the carry-over pot into
            // the next round; an outright winner collects the whole pot.
            let breakdowns, carryOut = Scoring.calculateScoresCarry carryOver finalGs.Players
            scoreBreakdowns <- breakdowns
            carryOver <- carryOut
            // Update cumulative
            cumulativeScores <-
                scoreBreakdowns
                |> List.fold (fun acc (p, s) ->
                    let prev = acc |> Map.tryFind p.Name |> Option.defaultValue 0
                    acc |> Map.add p.Name (prev + s.Total))
                    cumulativeScores
            // Check game over
            scoreIsGameOver <-
                cumulativeScores |> Map.exists (fun _ score -> score >= config.TargetScore)

// ─── Card Image Mapping ───────────────────────────────────────────────
module CardImg =

    let private suitPrefix = function
        | Spades   -> "sp"
        | Hearts   -> "he"
        | Diamonds -> "di"
        | Clubs    -> "cl"

    let private rankSuffix = function
        | Ace   -> "1"  | Two   -> "2"  | Three -> "3"
        | Four  -> "4"  | Five  -> "5"  | Six   -> "6"
        | Seven -> "7"  | Eight -> "8"  | Nine  -> "9"
        | Ten   -> "10" | Jack  -> "j"  | Queen -> "q"
        | King  -> "k"

    /// Get the Nu asset for a card face image
    let cardAsset (card: Card) : Image AssetTag =
        asset<Image> "Default" ($"{suitPrefix card.Suit}{rankSuffix card.Rank}")

    /// Deck image for the current game (randomly chosen scenic design with
    /// stacked edges baked in) — used for the deck pile and deck icon only.
    let backAsset () : Image AssetTag =
        asset<Image> "Default" AppState.currentBack

    /// Plain single-card back for face-down hand cards (not the deck image).
    let handBackAsset : Image AssetTag =
        asset<Image> "Default" "back"

    /// Table felt background image
    let tableBgAsset : Image AssetTag =
        asset<Image> "Default" "table_bg"

// ─── Layout Constants ─────────────────────────────────────────────────
// Nu virtual resolution is 640×360 → visible range ±320 (X) × ±180 (Y)
module Ly =
    // Card dimensions in world units
    [<Literal>]
    let cardW = 44.0f
    [<Literal>]
    let cardH = 57.0f
    [<Literal>]
    let cardGap = 5.0f
    [<Literal>]
    let tableGap = 4.0f

    // Y positions (center origin, Y up) — fitted to 640×360 viewport
    /// human hand at bottom (card bottom at -169)
    [<Literal>]
    let handY = -130.0f
    /// table center
    [<Literal>]
    let tableY = 10.0f
    /// top opponent (card top at 174)
    [<Literal>]
    let topOppY = 135.0f
    /// left side opponent (card edge at -315)
    [<Literal>]
    let sideLeftX = -285.0f
    /// right side opponent (card edge at 315)
    [<Literal>]
    let sideRightX = 285.0f

    /// Which edge of the table a player occupies, viewed from the bottom seat.
    type Seat =
        | SeatBottom
        | SeatLeft
        | SeatTop
        | SeatRight

    /// Seats advance clockwise (seen from above, like poker): the player after
    /// the bottom seat sits on the left, then the top, then the right — so the
    /// index turn order reads clockwise around the table. The lone opponent of
    /// a 2-player game stays at the top.
    let seatOf (playerCount: int) (bottomIdx: int) (idx: int) =
        match playerCount, (idx - bottomIdx + playerCount) % playerCount with
        | _, 0 -> SeatBottom
        | 2, _ -> SeatTop
        | 3, 1 -> SeatLeft
        | 3, _ -> SeatTop
        | _, 1 -> SeatLeft
        | _, 2 -> SeatTop
        | _, _ -> SeatRight

    // Table area dimensions (centered at 0, tableY)
    /// narrower to leave room for side hands
    [<Literal>]
    let tableW = 500.0f
    /// compressed to fit viewport
    [<Literal>]
    let tableH = 130.0f

    // Max entity slots
    /// max cards in hand (dealt 4 at a time)
    [<Literal>]
    let maxHand = 4
    /// max table cards (theoretical max)
    [<Literal>]
    let maxTable = 26
    /// max opponent cards shown
    [<Literal>]
    let maxOppHand = 4

    // Menu/UI positions
    [<Literal>]
    let titleY = 155.0f
    [<Literal>]
    let subtitleY = 125.0f
    [<Literal>]
    let menuBaseY = 55.0f
    [<Literal>]
    let btnW = 240.0f
    [<Literal>]
    let btnH = 28.0f
    [<Literal>]
    let btnGap = 36.0f

    // Status bar Y
    [<Literal>]
    let statusY = -95.0f
    [<Literal>]
    let turnTextY = -110.0f

    // Scoreboard position (top right)
    [<Literal>]
    let scoreX = 200.0f
    [<Literal>]
    let scoreTopY = 165.0f

    /// Center N cards horizontally, returns X of leftmost card
    let centerCardsX (count: int) (gap: float32) =
        let totalW = float32 count * (cardW + gap) - gap
        -totalW / 2.0f

// ─── Colors ───────────────────────────────────────────────────────────
module Clr =
    /// dark poker green (25,50,35)
    let screenBg = color 0.098f 0.196f 0.137f 1.0f
    /// poker-green felt (35,100,55)
    let tableBg = color 0.137f 0.392f 0.216f 1.0f
    let gold = color 1.0f 0.843f 0.0f 1.0f
    let white = color 1.0f 1.0f 1.0f 1.0f
    let gray = color 0.627f 0.627f 0.627f 1.0f
    let lightGray = color 0.827f 0.827f 0.827f 1.0f
    let green = color 0.0f 0.549f 0.0f 1.0f
    /// capture definite overlay (unused)
    let darkGreen = color 0.0f 0.275f 0.0f 0.353f
    /// capture possible overlay (unused)
    let darkYellow = color 0.275f 0.275f 0.0f 0.353f
    /// definite capture card tint
    let tintGreen = color 0.7f 1.0f 0.7f 1.0f
    /// possible capture card tint
    let tintYellow = color 1.0f 1.0f 0.65f 1.0f
    let limeGreen = color 0.196f 0.804f 0.196f 1.0f
    let yellow = color 1.0f 1.0f 0.0f 1.0f
    let lightSalmon = color 1.0f 0.627f 0.478f 1.0f
    let lightBlue = color 0.678f 0.847f 0.902f 1.0f
    let plum = color 0.867f 0.627f 0.867f 1.0f
    let lightGreen = color 0.565f 0.933f 0.565f 1.0f
    let btnGreen = color 0.157f 0.392f 0.157f 1.0f
    let btnBlue = color 0.157f 0.314f 0.471f 1.0f
    let btnRed = color 0.471f 0.157f 0.157f 1.0f
    /// red-suit tint for card names in text
    let cardRed = color 1.0f 0.53f 0.49f 1.0f
    /// black-suit tint for card names in text
    let cardGray = color 0.745f 0.745f 0.745f 1.0f
    let btnPurple = color 0.314f 0.235f 0.471f 1.0f
    let btnDark = color 0.235f 0.314f 0.235f 1.0f
    let modalOverlay = color 0.0f 0.0f 0.0f 0.627f

// ─── Helpers ──────────────────────────────────────────────────────────
module Helpers =

    /// Compute scattered positions for table cards in Nu world coordinates.
    /// Cards are placed center-outward: the first card lands near the middle
    /// of the table and each subsequent card tries progressively larger radii.
    /// Table area: centered at (0, Ly.tableY) with size Ly.tableW × Ly.tableH, Y-up.
    /// Returns Map<Card, (x, y, rotation)>.
    let computeScatteredPositions (table: Card list) (existing: Map<Card, (float32 * float32 * float32)>) =
        let centerX = 0.0f
        let centerY = Ly.tableY
        let maxRadiusX = Ly.tableW / 2.0f - Ly.cardW / 2.0f - 8.0f
        let maxRadiusY = Ly.tableH / 2.0f - Ly.cardH / 2.0f - 8.0f

        let mutable result = existing

        // Remove cards no longer on table
        let tableSet = Set.ofList table
        result <- result |> Map.filter (fun card _ -> Set.contains card tableSet)

        // Add positions for new cards — center-outward placement
        let newCards = table |> List.filter (fun c -> not (Map.containsKey c result))
        let existingCount = Map.count result

        for cardIdx in 0 .. List.length newCards - 1 do
            let card = newCards[cardIdx]
            let seed = hash (card.Suit, card.Rank)
            let rng = Random(seed)
            let orderIdx = existingCount + cardIdx
            let spreadFraction = float32 orderIdx / float32 (max 1 (List.length table - 1))
            let radiusFrac = 0.15f + 0.85f * spreadFraction

            let mutable attempts = 0
            let mutable placed = false
            let mutable bestX = centerX
            let mutable bestY = centerY
            let mutable bestRot = 0.0f

            while not placed && attempts < 60 do
                let angle = float32 (rng.NextDouble()) * MathF.PI * 2.0f
                let jitter = 0.7f + float32 (rng.NextDouble()) * 0.6f
                let rFrac = radiusFrac * jitter |> min 1.0f
                let rx = centerX + cos angle * maxRadiusX * rFrac
                let ry = centerY + sin angle * maxRadiusY * rFrac
                // Clamp inside area
                let x = max (centerX - maxRadiusX) (min (centerX + maxRadiusX) rx)
                let y = max (centerY - maxRadiusY) (min (centerY + maxRadiusY) ry)
                let rot = (float32 (rng.NextDouble()) - 0.5f) * 0.35f

                let overlaps =
                    result |> Map.exists (fun _ (ox, oy, _) ->
                        abs(x - ox) < Ly.cardW * 0.85f && abs(y - oy) < Ly.cardH * 0.85f)

                if not overlaps then
                    bestX <- x; bestY <- y; bestRot <- rot; placed <- true
                else
                    bestX <- x; bestY <- y; bestRot <- rot

                attempts <- attempts + 1

            result <- result |> Map.add card (bestX, bestY, bestRot)

        result

    /// Topmost scattered table card whose rect contains (mx, my) — for drag-to-reposition.
    let tableCardAtScatter (table: Card list) (mx: float32) (my: float32) =
        table
        |> List.rev                                  // later in the list draws on top
        |> List.tryPick (fun card ->
            match Map.tryFind card AppState.scatteredPositions with
            | Some (sx, sy, _) ->
                if abs (mx - sx) <= Ly.cardW / 2.0f && abs (my - sy) <= Ly.cardH / 2.0f then Some card else None
            | None -> None)

    /// Clamp a scattered card's centre so the whole card stays within the table area.
    let clampScatterCenter (cx: float32) (cy: float32) =
        let maxX = Ly.tableW / 2.0f - Ly.cardW / 2.0f
        let dy = Ly.tableH / 2.0f - Ly.cardH / 2.0f
        (min maxX (max -maxX cx), min (Ly.tableY + dy) (max (Ly.tableY - dy) cy))

    /// Format a play result as a human-readable message
    let formatPlayResult (playerName: string) (result: PlayResult) =
        match result with
        | Capture(hc, captured, sweep) ->
            let capturedStr = captured |> List.map Cards.display |> String.concat " "
            let sweepStr = if sweep then " SWEEP!" else ""
            $"{playerName} plays {Cards.display hc} -> captures {capturedStr}{sweepStr}"
        | Place hc ->
            $"{playerName} places {Cards.display hc}"

    /// Compute capture preview for a selected hand card
    let computePreview (card: Card) (table: Card list) : CapturePreview =
        let options = Rules.findCaptureOptions card table
        match options with
        | [] -> NoCapture
        | [ single ] -> SingleCapture single.Captured
        | multiple ->
            let allSets = multiple |> List.map (fun o -> Set.ofList o.Captured)
            let definite = allSets |> List.reduce Set.intersect |> Set.toList
            let anyCapture = allSets |> List.reduce Set.union |> Set.toList
            let possible = anyCapture |> List.filter (fun c -> not (List.contains c definite))
            MultipleCaptures(definite, possible)

    /// Advance the turn: deal or transition to next player
    [<TailCall>]
    let rec advanceTurn () =
        match AppState.gameState with
        | None -> ()
        | Some gs ->
            if GameEngine.allHandsEmpty gs then
                if gs.DealRound < gs.TotalDeals then
                    let nextDeal = gs.DealRound + 1
                    let newGs = GameEngine.dealRound gs false
                    AppState.gameState <- Some { newGs with DealRound = nextDeal }
                    AppState.lastPlayMessage <- $"Round {AppState.roundNumber} - Deal {nextDeal}"
                    AppState.phase <- Shuffling
                    AppState.phaseTimer <- 0.0f
                else
                    // End of round: the last capturer takes whatever remains
                    // on the table (endRound applies it in enterScores) — say
                    // so, since the cards leave without a play animation.
                    (match gs.LastCapturer, gs.Table with
                     | Some idx, (_ :: _ as rest) ->
                         AppState.lastPlayMessage <-
                             $"{gs.Players[idx].Name} takes the rest of the table ({List.length rest} cards)"
                     | _ -> ())
                    AppState.phase <- RoundOver
            else
                let currentPlayer = gs.Players[gs.CurrentPlayerIndex]
                if List.isEmpty currentPlayer.Hand then
                    let newGs = { gs with CurrentPlayerIndex = (gs.CurrentPlayerIndex + 1) % gs.Players.Length }
                    AppState.gameState <- Some newGs
                    advanceTurn ()
                else
                    match currentPlayer.Type with
                    | Human ->
                        AppState.phase <- WaitingForHuman
                        AppState.selectedCardIndex <- None
                        AppState.hoveredCardIndex <- None
                        AppState.capturePreview <- NoCapture
                    | Computer ->
                        AppState.phase <- ComputerThinking
                        AppState.phaseTimer <- 0.0f

    /// Display order for the strict grid: cards arranged by table value
    /// (aces first, kings last), suits keeping ties stable. The game state's
    /// own order is untouched — this is presentation only.
    let gridOrder (table: Card list) =
        table |> List.sortBy (fun c -> Cards.tableValue c.Rank, c.Suit)

    /// Balanced grid geometry: up to 7 cards in one row, more split into
    /// balanced rows (9 = 5+4), each row individually centered. Returns the
    /// center position of display index idx among count cards.
    let gridPos (count: int) (idx: int) =
        let rows = if count <= 7 then 1 else (count + 6) / 7
        let cols = (count + rows - 1) / rows
        let row = idx / cols
        let col = idx % cols
        let rowCount = min cols (count - row * cols)   // last row may be short
        let x = Ly.centerCardsX rowCount Ly.tableGap + float32 col * (Ly.cardW + Ly.tableGap) + Ly.cardW / 2.0f
        let y = Ly.tableY + float32 (rows - 1 - row * 2) * (Ly.cardH + Ly.tableGap) / 2.0f
        (x, y)

    /// Build a collect animation from a play result.
    /// Captured cards slide from their table positions toward the player's area.
    let buildCollectAnimation (playResult: PlayResult) (seat: Ly.Seat) (table: Card list) =
        match playResult with
        | Capture(_, captured, _) when not (List.isEmpty captured) ->
            let destX, destY =
                match seat with
                | Ly.SeatBottom -> 0.0f, Ly.handY - 40.0f
                | Ly.SeatTop    -> 0.0f, Ly.topOppY + 40.0f
                | Ly.SeatLeft   -> Ly.sideLeftX - 40.0f, 0.0f
                | Ly.SeatRight  -> Ly.sideRightX + 40.0f, 0.0f
            let cards =
                captured |> List.map (fun card ->
                    match Map.tryFind card AppState.scatteredPositions with
                    | Some(sx, sy, _) -> (card, sx, sy)
                    | None ->
                        // Fallback: grid position (value-sorted display order)
                        let idx = gridOrder table |> List.tryFindIndex ((=) card) |> Option.defaultValue 0
                        let x, y = gridPos (List.length table) idx
                        (card, x, y))
            Some { CollectCards = cards; CollectToX = destX; CollectToY = destY
                   CollectStart = AppState.cardSlideDuration; CollectDuration = AppState.collectSlideDuration }
        | _ -> None

    /// Build deal animation steps for Nu.
    /// First deal: 4 cards to table, then (2 per player) × 2 rounds.
    /// Subsequent deals: (2 per player) × 2 rounds.
    let buildDealSteps (gs: GameEngine.GameState) (isFirstDeal: bool) =
        let playerCount = gs.Players.Length
        let bottomIdx = 0   // fixed viewpoint: seat 0 is the bottom seat, also in watch mode

        let playerDest (idx: int) =
            match Ly.seatOf playerCount bottomIdx idx with
            | Ly.SeatBottom -> (0.0f, Ly.handY)
            | Ly.SeatTop -> (0.0f, Ly.topOppY)
            | Ly.SeatLeft -> (Ly.sideLeftX, 0.0f)
            | Ly.SeatRight -> (Ly.sideRightX, 0.0f)

        // Deal like at a real table: two passes of 2 cards to each player —
        // starting with the player next to the dealer (the wave's first to
        // act, CurrentPlayerIndex) and proceeding clockwise, dealer last —
        // and on the first deal each pass ends with 2 cards to the table.
        [for _ in 1 .. 2 do
            for k in 0 .. playerCount - 1 do
                let pIdx = (gs.CurrentPlayerIndex + k) % playerCount
                let (px, py) = playerDest pIdx
                { DealTargetLabel = gs.Players[pIdx].Name; DealCardCount = 2
                  DealToX = px; DealToY = py; DealIsFaceUp = (pIdx = bottomIdx) }
            if isFirstDeal then
                { DealTargetLabel = "table"; DealCardCount = 2
                  DealToX = 0.0f; DealToY = Ly.tableY; DealIsFaceUp = false }]

    /// Process a human play (single option or no captures)
    let processHumanPlay (cardIndex: int) =
        match AppState.gameState with
        | None -> ()
        | Some gs ->
            let player = gs.Players[gs.CurrentPlayerIndex]
            let card = player.Hand[cardIndex]
            let options = Rules.findCaptureOptions card gs.Table
            match options with
            | _ :: _ :: _ ->
                AppState.phase <- ChoosingCaptureOption
                AppState.captureOptions <- options
                AppState.captureCardIdx <- cardIndex
                AppState.capturePage <- 0
                AppState.selectedCardIndex <- None
            | _ ->
                // Build card animation: from hand position to scatter/center position
                let handSize = List.length player.Hand
                let startX = Ly.centerCardsX handSize Ly.cardGap
                let fromX = startX + float32 cardIndex * (Ly.cardW + Ly.cardGap) + Ly.cardW / 2.0f
                let fromY = Ly.handY
                let turnResult = GameEngine.playHumanTurn gs cardIndex None
                AppState.currentCollectAnim <- buildCollectAnimation turnResult.PlayResult Ly.SeatBottom gs.Table
                // Compute animation target: scatter position for Place, table center for Capture
                let toX, toY =
                    match turnResult.PlayResult, AppState.tableLayout with
                    | Place _, RandomScatter ->
                        let newPos = computeScatteredPositions turnResult.NewState.Table AppState.scatteredPositions
                        AppState.scatteredPositions <- newPos
                        match Map.tryFind card newPos with
                        | Some(sx, sy, _) -> (sx, sy)
                        | None -> (0.0f, Ly.tableY)
                    | _ -> (0.0f, Ly.tableY)
                AppState.currentCardAnim <- Some
                    { AnimCard = card
                      FromX = fromX; FromY = fromY
                      ToX = toX; ToY = toY
                      Duration = AppState.cardSlideDuration }
                let msg = formatPlayResult player.Name turnResult.PlayResult
                AppState.gameState <- Some turnResult.NewState
                AppState.lastPlayMessage <- msg
                AppState.lastChat <- ""
                AppState.selectedCardIndex <- None
                // Clear the capture-preview tint (only recomputed while WaitingForHuman)
                AppState.capturePreview <- NoCapture
                AppState.lastEval <- Some turnResult.Evaluation
                AppState.phase <- AnimatingPlay
                AppState.phaseTimer <- 0.0f

    /// Process a chosen capture option
    let processCapture (cardIdx: int) (chosen: Rules.CaptureOption) =
        match AppState.gameState with
        | None -> ()
        | Some gs ->
            let player = gs.Players[gs.CurrentPlayerIndex]
            let card = player.Hand[cardIdx]
            let handSize = List.length player.Hand
            let startX = Ly.centerCardsX handSize Ly.cardGap
            let fromX = startX + float32 cardIdx * (Ly.cardW + Ly.cardGap) + Ly.cardW / 2.0f
            let fromY = Ly.handY
            let turnResult = GameEngine.playHumanTurn gs cardIdx (Some chosen)
            AppState.currentCollectAnim <- buildCollectAnimation turnResult.PlayResult Ly.SeatBottom gs.Table
            // Captures always animate to table center (cards get collected away)
            AppState.currentCardAnim <- Some
                { AnimCard = card
                  FromX = fromX; FromY = fromY
                  ToX = 0.0f; ToY = Ly.tableY
                  Duration = AppState.cardSlideDuration }
            let msg = formatPlayResult player.Name turnResult.PlayResult
            AppState.gameState <- Some turnResult.NewState
            AppState.lastPlayMessage <- msg
            AppState.lastChat <- ""
            AppState.selectedCardIndex <- None
            // Clear the capture-preview tint (only recomputed while WaitingForHuman)
            AppState.capturePreview <- NoCapture
            AppState.lastEval <- Some turnResult.Evaluation
            AppState.phase <- AnimatingPlay
            AppState.phaseTimer <- 0.0f

    /// Place a capture-capable card without capturing (Standard Kasino only,
    /// where capturing is optional).
    let processHumanPlace (cardIndex: int) =
        match AppState.gameState with
        | None -> ()
        | Some gs ->
            let player = gs.Players[gs.CurrentPlayerIndex]
            let card = player.Hand[cardIndex]
            let handSize = List.length player.Hand
            let startX = Ly.centerCardsX handSize Ly.cardGap
            let fromX = startX + float32 cardIndex * (Ly.cardW + Ly.cardGap) + Ly.cardW / 2.0f
            let fromY = Ly.handY
            let turnResult = GameEngine.playHumanPlaceTurn gs cardIndex
            AppState.currentCollectAnim <- buildCollectAnimation turnResult.PlayResult Ly.SeatBottom gs.Table
            let toX, toY =
                match turnResult.PlayResult, AppState.tableLayout with
                | Place _, RandomScatter ->
                    let newPos = computeScatteredPositions turnResult.NewState.Table AppState.scatteredPositions
                    AppState.scatteredPositions <- newPos
                    match Map.tryFind card newPos with
                    | Some(sx, sy, _) -> (sx, sy)
                    | None -> (0.0f, Ly.tableY)
                | _ -> (0.0f, Ly.tableY)
            AppState.currentCardAnim <- Some
                { AnimCard = card
                  FromX = fromX; FromY = fromY
                  ToX = toX; ToY = toY
                  Duration = AppState.cardSlideDuration }
            let msg = formatPlayResult player.Name turnResult.PlayResult
            AppState.gameState <- Some turnResult.NewState
            AppState.lastPlayMessage <- msg
            AppState.lastChat <- ""
            AppState.selectedCardIndex <- None
            // Clear the capture-preview tint (only recomputed while WaitingForHuman)
            AppState.capturePreview <- NoCapture
            AppState.lastEval <- Some turnResult.Evaluation
            AppState.phase <- AnimatingPlay
            AppState.phaseTimer <- 0.0f

// ─── Rules page content ───────────────────────────────────────────────
module RulesContent =

    // A tutorial page is either a block of text, or a VISUAL page that
    // shows real card images so new / visual players can see how
    // capturing and scoring actually work.
    type CardSpot = { Card: Card; X: float32; Y: float32 }
    type Caption  = { Text: string; X: float32; Y: float32; Col: Color; Size: float32; Center: bool; W: float32 }

    type Page =
        | TextPage of title: string * lines: string[]
        | VisualPage of title: string * cards: CardSpot list * caps: Caption list

    // ── Visual-page layout helpers (Nu virtual res ±320 × ±180) ──
    [<Literal>]
    let private tcw = 38.0f      // tutorial card width
    [<Literal>]
    let private tch = 49.0f      // tutorial card height

    /// Centered header/footer line at y.
    let private hdr y col text : Caption =
        { Text = text; X = 0.0f; Y = y; Col = col; Size = 12.0f; Center = true; W = 580.0f }

    /// A centered row of cards, each with a short caption beneath it.
    let private mkRow (centerY: float32) (labelCol: Color) (items: (Card * string) list) =
        let gap = 44.0f
        let n = items.Length
        let totalW = float32 n * tcw + float32 (max 0 (n - 1)) * gap
        let startX = -totalW / 2.0f + tcw / 2.0f
        let labelY = centerY - tch / 2.0f - 9.0f
        items
        |> List.mapi (fun i (c, lbl) ->
            let x = startX + float32 i * (tcw + gap)
            { Card = c; X = x; Y = centerY },
            { Text = lbl; X = x; Y = labelY; Col = labelCol; Size = 11.0f; Center = true; W = 130.0f })
        |> List.unzip

    /// A group of cards laid left-to-right starting at (x, y), with a
    /// left-justified label to the right of the cards.
    let private mkGroup (x: float32) (y: float32) (labelCol: Color) (cards: Card list) (label: string) =
        let gap = 6.0f
        let spots = cards |> List.mapi (fun i c -> { Card = c; X = x + float32 i * (tcw + gap); Y = y })
        let lastX = x + float32 (cards.Length - 1) * (tcw + gap)
        let cap = { Text = label; X = lastX + tcw / 2.0f + 12.0f; Y = y; Col = labelCol; Size = 11.0f; Center = false; W = 250.0f }
        spots, cap

    let private cardValuesVisual () =
        let cards1, caps1 =
            mkRow 35.0f Clr.white
                [ { Suit = Clubs;  Rank = Four },  "worth 4"
                  { Suit = Hearts; Rank = Seven }, "worth 7"
                  { Suit = Spades; Rank = King },  "worth 13" ]
        let cards2, caps2 =
            mkRow -75.0f Clr.lightGreen
                [ { Suit = Diamonds; Rank = Ace }, "Ace = 14"
                  { Suit = Spades;   Rank = Two }, "2 of spades = 15"
                  { Suit = Diamonds; Rank = Ten }, "10 of diam. = 16" ]
        let heads =
            [ hdr 108.0f Clr.white    "Every card has a TABLE value and a HAND value."
              hdr 90.0f  Clr.lightGray "Add table values; spend HAND value to capture."
              hdr 68.0f  Clr.gold      "Normal cards: value = face value"
              hdr -38.0f Clr.gold      "Three special cards have EXTRA capture power:" ]
        cards1 @ cards2, heads @ caps1 @ caps2

    let private capture9Visual () =
        let gx = -170.0f
        let s0, c0 = mkGroup gx 88.0f  Clr.gold        [ { Suit = Hearts; Rank = Nine } ]                                  "<- the 9 you play"
        let s1, c1 = mkGroup gx 38.0f  Clr.limeGreen   [ { Suit = Clubs;  Rank = Nine } ]                                  "a 9       = 9   captured"
        let s2, c2 = mkGroup gx -16.0f Clr.limeGreen   [ { Suit = Spades; Rank = Three }; { Suit = Diamonds; Rank = Six } ] "3 + 6 = 9   captured"
        let s3, c3 = mkGroup gx -70.0f Clr.lightSalmon [ { Suit = Hearts; Rank = Eight } ]                                 "8 is not 9  ->  stays"
        let heads =
            [ hdr 118.0f  Clr.white "Play a 9: capture any cards that ADD UP to 9."
              hdr -118.0f Clr.white "One 9 grabs BOTH 9-groups at once (3 cards)." ]
        s0 @ s1 @ s2 @ s3, heads @ [ c0; c1; c2; c3 ]

    let private takeOrLeaveVisual () =
        let s0, c0 = mkGroup -70.0f 70.0f Clr.gold [ { Suit = Hearts; Rank = Nine }; { Suit = Clubs; Rank = Nine } ] "hand 9 + table 9"
        let heads =
            [ hdr 110.0f  Clr.white       "Your 9 CAN capture the table 9. Must you?"
              hdr 10.0f   Clr.limeGreen   "STANDARD: capturing is OPTIONAL."
              hdr -8.0f   Clr.lightGray   "Take it, or just place a card instead."
              hdr -50.0f  Clr.lightSalmon "LAISTO: capturing is FORCED."
              hdr -68.0f  Clr.lightGray   "If a capture is possible, you MUST take it."
              hdr -104.0f Clr.gray        "(In Laisto a forced take hurts you.)" ]
        s0, c0 :: heads

    let private scoringVisual () =
        let gx = -150.0f
        let s0, c0 = mkGroup gx 95.0f  Clr.gold  [ { Suit = Diamonds; Rank = Ten } ] "10 of diamonds = 2 points"
        let s1, c1 = mkGroup gx 42.0f  Clr.gold  [ { Suit = Spades;   Rank = Two } ] "2 of spades = 1 point"
        let s2, c2 = mkGroup gx -11.0f Clr.white
                        [ { Suit = Spades;   Rank = Ace }; { Suit = Hearts; Rank = Ace }
                          { Suit = Diamonds; Rank = Ace }; { Suit = Clubs;  Rank = Ace } ] "each Ace = 1 pt (4 total)"
        let s3, c3 = mkGroup gx -64.0f Clr.white
                        [ { Suit = Spades; Rank = Four }; { Suit = Spades; Rank = Seven }; { Suit = Spades; Rank = Nine } ] "most Spades = 2 points"
        let heads =
            [ hdr 122.0f  Clr.white     "Most points come from specials & majorities:"
              hdr -110.0f Clr.lightGray "Most cards = 1 pt      Each sweep = 1 pt" ]
        s0 @ s1 @ s2 @ s3, heads @ [ c0; c1; c2; c3 ]

    let pages : Page[] =
        let vp title (f: unit -> CardSpot list * Caption list) =
            let cards, caps = f ()
            VisualPage (title, cards, caps)
        [| TextPage ("Game Overview",
            [| "Kasino is a classic Finnish card game for 2-4 players."
               "The goal is to capture cards from the table by"
               "matching values from your hand."
               ""
               "Each round, players are dealt cards in waves of 4."
               "On your turn you MUST play one card from your hand:"
               "  - If it can capture table cards, you may take them"
               "    (optional in Standard, forced in Laisto)."
               "  - Otherwise your card is placed on the table."
               ""
               "After all cards are played, scores are tallied."
               "First player to reach 16 cumulative points wins!" |])
           TextPage ("Card Values",
            [| "Cards have TWO different value systems:"
               ""
               "TABLE VALUE (for summing on the table):"
               "  Ace=1, 2-10=face, J=11, Q=12, K=13"
               ""
               "HAND VALUE (capture power from hand):"
               "  Most cards use table value, but 3 are special:"
               "  Ace = 14  (captures combos summing to 14)"
               "  Spade 2 = 15  (captures combos summing to 15)"
               "  Diamond 10 = 16  (captures combos sum to 16)"
               ""
               "Kings can only be captured by Kings (value 13)." |])
           vp "Card Values at a Glance" cardValuesVisual
           TextPage ("Capturing Cards",
            [| "When you play a card, ALL non-overlapping subsets"
               "of table cards that sum to your hand value must"
               "be captured simultaneously."
               ""
               "Example: Play a 7, table has 3,4,2,5,7"
               "  Subsets: {7}, {3,4}, {2,5} - no overlap"
               "  => Capture ALL 5 cards at once!"
               ""
               "If subsets OVERLAP, you choose which to take."
               ""
               "CAPTURE PREVIEW when selecting a card:"
               "  Green = definitely captured (in all options)"
               "  Yellow = in some options only (choice needed)" |])
           vp "Capturing with a 9" capture9Visual
           vp "Take or Leave" takeOrLeaveVisual
           TextPage ("Sweeps & Round End",
            [| "SWEEP: Capturing ALL remaining table cards"
               "earns bonus points."
               ""
               "ROUND END: All deals exhausted, hands empty."
               "  - Last capturer takes remaining table cards"
               "    (NOT a sweep)."
               "  - Scores tallied for the round."
               ""
               "DEALING STRUCTURE (52 cards):"
               "  4 cards to table at start."
               "  Rest dealt in waves of 4 per player:"
               "  2 players: 6 waves, 3: 4 waves, 4: 3 waves" |])
           TextPage ("Scoring",
            [| "SCORING (per round):"
               ""
               "  Most cards captured .... 1 point"
               "  Most spades captured ... 2 points"
               "  Each Ace captured ...... 1 point (max 4)"
               "  10 of Diamonds ......... 2 points"
               "  2 of Spades ............ 1 point"
               "  Each Sweep ............. 1 point"
               ""
               "TIE: Tied most-cards/spades points carry over as a"
               "  pot to the next outright winner of the category."
               "SWEEPS: Minimum sweep count subtracted from all."
               "  Once anyone has 10+ points, sweeps score nothing."
               "TARGET: First to 16 cumulative points wins." |])
           vp "Scoring Cards" scoringVisual
           TextPage ("Laistokasino",
            [| "LAISTOKASINO (Misa-Kasino):"
               ""
               "Same rules, but goal is REVERSED:"
               "Minimize your points! First to 16 LOSES."
               ""
               "STRATEGY:"
               "  - Avoid capturing Aces and special cards."
               "  - Don't accumulate too many cards or spades."
               "  - Sweeps hurt you!"
               "  - Sometimes placing is better than capturing."
               "  - Force opponents to sweep by leaving few"
               "    cards on the table." |]) |]

    let totalPages = pages.Length

    let pageTitle page =
        match pages.[page] with
        | TextPage (t, _) -> t
        | VisualPage (t, _, _) -> t

// ─── Game Dispatcher ──────────────────────────────────────────────────
type KasinoDispatcher () =
    inherit GameDispatcherImSim ()

    static member Properties =
        [define Game.KasinoMode KasinoMenu]

    /// Declare a screen with our standard dark-green background
    static member private BeginMode (name, active, world) =
        World.beginScreen name active Vanilla [] world |> ignore
        World.beginGroup "Main" [] world
        World.doStaticSprite "ScreenBg"
            [Entity.Position .= v3 0.0f 0.0f 0.0f
             Entity.Size .= v3 800.0f 600.0f 0.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Clr.screenBg
             Entity.Elevation .= -1.0f] world |> ignore

    static member private EndMode world =
        World.endGroup world
        World.endScreen world

    override this.Process (game, world) =

        let mode = game.GetKasinoMode world
        AppState.enterConsumed <- false

        // ── Menu Screen ──
        KasinoDispatcher.BeginMode ("Menu", (mode = KasinoMenu), world)
        KasinoDispatcher.RenderMenu (game, world)
        KasinoDispatcher.EndMode world

        // ── Game Screen ──
        KasinoDispatcher.BeginMode ("Game", (mode = KasinoPlaying), world)
        KasinoDispatcher.RenderGame (game, world)
        KasinoDispatcher.EndMode world

        // ── Scores Screen ──
        KasinoDispatcher.BeginMode ("Scores", (mode = KasinoScores), world)
        KasinoDispatcher.RenderScores (game, world)
        KasinoDispatcher.EndMode world

        // ── Rules Screen ──
        KasinoDispatcher.BeginMode ("Rules", (mode = KasinoRules), world)
        KasinoDispatcher.RenderRules (game, world)
        KasinoDispatcher.EndMode world

        // when not in editor, handle the close-window button or Alt+F4
        if world.Unaccompanied then
            if  World.doSubscriptionAny "Exit" game.ExitRequestEvent world ||
                World.isKeyboardAltDown world && World.isKeyboardKeyDown KeyboardKey.F4 world then
                World.exit world

        // Toggle fullscreen on F11
        if World.isKeyboardKeyPressed KeyboardKey.F11 world then
            World.tryToggleWindowFullScreen world |> ignore

    // ═══════════════════════════════════════════════════════════════
    //  MENU SCREEN
    // ═══════════════════════════════════════════════════════════════
    static member RenderMenu (game : Game, world : World) =

        // Title
        World.doText "Title"
            [Entity.Position .= v3 0.0f Ly.titleY 0.0f
             Entity.Size .= v3 300.0f 40.0f 0.0f
             Entity.Text .= "KASINO"
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.gold
             Entity.FontSizing .= Some 30.0f
             Entity.Elevation .= 1.0f] world

        World.doText "Subtitle"
            [Entity.Position .= v3 0.0f Ly.subtitleY 0.0f
             Entity.Size .= v3 300.0f 24.0f 0.0f
             Entity.Text .= "Finnish Card Game"
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.white
             Entity.FontSizing .= Some 14.0f
             Entity.Elevation .= 1.0f] world

        // Decorative fan of the four aces (held-in-hand shape), filling the empty
        // band between the selection buttons and the "How to Play" button.
        let acesFan = [ Spades, 0.30f; Hearts, 0.10f; Diamonds, -0.10f; Clubs, -0.30f ]
        for i, (suit, rot) in List.indexed acesFan do
            let off = float32 i - 1.5f                           // -1.5, -0.5, 0.5, 1.5
            World.doStaticSprite ("MenuAce" + string i)
                [Entity.Position .= v3 (off * 44.0f) (-100.0f - abs off * 7.0f) 0.0f
                 Entity.Size .= v3 48.0f 60.0f 0.0f
                 Entity.StaticImage .= CardImg.cardAsset { Suit = suit; Rank = Ace }
                 Entity.Rotation .= Quaternion.CreateFromAxisAngle (Vector3.UnitZ, rot)
                 Entity.Elevation .= 0.5f] world |> ignore

        // Prompt text (always declared, content changes per step)
        let promptText =
            match AppState.menuStep with
            | VariantSelect -> "Choose game variant:"
            | PlayerCountSelect ->
                let vName = match AppState.menuVariant with StandardKasino -> "Standard" | LaistoKasino -> "Laisto"
                $"Variant: {vName}  |  Number of players:"
            | HumanCountSelect ->
                let vName = match AppState.menuVariant with StandardKasino -> "Standard" | LaistoKasino -> "Laisto"
                $"Variant: {vName}  |  Players: {AppState.menuPlayerCount}  |  How many humans?"

        World.doText "MenuPrompt"
            [Entity.Position .= v3 0.0f (Ly.menuBaseY + 30.0f) 0.0f
             Entity.Size .= v3 520.0f 24.0f 0.0f
             Entity.Text @= promptText
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.lightGray
             Entity.FontSizing .= Some 12.0f
             Entity.Elevation .= 1.0f] world

        // ── Variant buttons (visible only on VariantSelect) ──
        let isVariant = AppState.menuStep = VariantSelect
        if World.doButton "BtnStandard"
            [Entity.Position .= v3 0.0f Ly.menuBaseY 0.0f
             Entity.Size .= v3 Ly.btnW Ly.btnH 0.0f
             Entity.Text .= "Standard Kasino (maximize)"
             Entity.Visible @= isVariant
             Entity.Elevation .= 1.0f] world then
            AppState.menuVariant <- StandardKasino
            AppState.menuStep <- PlayerCountSelect

        if World.doButton "BtnLaisto"
            [Entity.Position .= v3 0.0f (Ly.menuBaseY - Ly.btnGap) 0.0f
             Entity.Size .= v3 Ly.btnW Ly.btnH 0.0f
             Entity.Text .= "Laistokasino (minimize)"
             Entity.Visible @= isVariant
             Entity.Elevation .= 1.0f] world then
            AppState.menuVariant <- LaistoKasino
            AppState.menuStep <- PlayerCountSelect

        // ── Player count buttons (visible only on PlayerCountSelect) ──
        let isPlayerCount = AppState.menuStep = PlayerCountSelect
        if World.doButton "Btn2P"
            [Entity.Position .= v3 -130.0f Ly.menuBaseY 0.0f
             Entity.Size .= v3 120.0f Ly.btnH 0.0f
             Entity.Text .= "2 Players"
             Entity.Visible @= isPlayerCount
             Entity.Elevation .= 1.0f] world then
            AppState.menuPlayerCount <- 2
            AppState.menuStep <- HumanCountSelect

        if World.doButton "Btn3P"
            [Entity.Position .= v3 0.0f Ly.menuBaseY 0.0f
             Entity.Size .= v3 120.0f Ly.btnH 0.0f
             Entity.Text .= "3 Players"
             Entity.Visible @= isPlayerCount
             Entity.Elevation .= 1.0f] world then
            AppState.menuPlayerCount <- 3
            AppState.menuStep <- HumanCountSelect

        if World.doButton "Btn4P"
            [Entity.Position .= v3 130.0f Ly.menuBaseY 0.0f
             Entity.Size .= v3 120.0f Ly.btnH 0.0f
             Entity.Text .= "4 Players"
             Entity.Visible @= isPlayerCount
             Entity.Elevation .= 1.0f] world then
            AppState.menuPlayerCount <- 4
            AppState.menuStep <- HumanCountSelect

        // ── Human count buttons (visible only on HumanCountSelect) ──
        let isHumanCount = AppState.menuStep = HumanCountSelect
        if World.doButton "BtnAI"
            [Entity.Position .= v3 0.0f Ly.menuBaseY 0.0f
             Entity.Size .= v3 Ly.btnW Ly.btnH 0.0f
             Entity.Text .= "Watch AI Only"
             Entity.Visible @= isHumanCount
             Entity.Elevation .= 1.0f] world then
            AppState.menuHumanCount <- 0
            AppState.startGame ()
            game.SetKasinoMode KasinoPlaying world

        if World.doButton "BtnHuman"
            [Entity.Position .= v3 0.0f (Ly.menuBaseY - Ly.btnGap) 0.0f
             Entity.Size .= v3 Ly.btnW Ly.btnH 0.0f
             Entity.Text .= "Play Yourself"
             Entity.Visible @= isHumanCount
             Entity.Elevation .= 1.0f] world then
            AppState.menuHumanCount <- 1
            AppState.startGame ()
            game.SetKasinoMode KasinoPlaying world

        // ── Flavour toggles (visible only on HumanCountSelect) ──
        if World.doButton "BtnTogglePers"
            [Entity.Position .= v3 0.0f (Ly.menuBaseY - 2.0f * Ly.btnGap) 0.0f
             Entity.Size .= v3 Ly.btnW Ly.btnH 0.0f
             Entity.Text @= (if AppState.menuSettings.AiPersonalities then "AI Personalities: ON" else "AI Personalities: OFF")
             Entity.Visible @= isHumanCount
             Entity.Elevation .= 1.0f] world then
            AppState.menuSettings <- { AppState.menuSettings with AiPersonalities = not AppState.menuSettings.AiPersonalities }

        if World.doButton "BtnToggleChat"
            [Entity.Position .= v3 0.0f (Ly.menuBaseY - 3.0f * Ly.btnGap) 0.0f
             Entity.Size .= v3 Ly.btnW Ly.btnH 0.0f
             Entity.Text @= (if AppState.menuSettings.ChatEnabled then "Table Talk: ON" else "Table Talk: OFF")
             Entity.Visible @= isHumanCount
             Entity.Elevation .= 1.0f] world then
            AppState.menuSettings <- { AppState.menuSettings with ChatEnabled = not AppState.menuSettings.ChatEnabled }

        if World.doButton "BtnToggleStrict"
            [Entity.Position .= v3 0.0f (Ly.menuBaseY - 4.0f * Ly.btnGap) 0.0f
             Entity.Size .= v3 Ly.btnW Ly.btnH 0.0f
             Entity.Text @= (if AppState.menuSettings.StrictRules then "Strict Rules: ON" else "Strict Rules: OFF")
             Entity.Visible @= isHumanCount
             Entity.Elevation .= 1.0f] world then
            AppState.menuSettings <- { AppState.menuSettings with StrictRules = not AppState.menuSettings.StrictRules }

        // "How to Play" button on all menu steps
        if World.doButton "BtnRules"
            [Entity.Position .= v3 0.0f -160.0f 0.0f
             Entity.Size .= v3 160.0f Ly.btnH 0.0f
             Entity.Text .= "How to Play"
             Entity.Elevation .= 1.0f] world then
            AppState.rulesPage <- 0
            AppState.rulesReturnMode <- KasinoMenu
            game.SetKasinoMode KasinoRules world

        // Quit button (top-right corner; Esc also quits)
        if World.doButton "BtnQuit"
            [Entity.Position .= v3 250.0f 160.0f 0.0f
             Entity.Size .= v3 110.0f Ly.btnH 0.0f
             Entity.Text .= "Quit"
             Entity.Elevation .= 1.0f] world then
            if world.Unaccompanied then World.exit world

        // Escape to quit from menu
        if world.Advancing && World.isKeyboardKeyPressed KeyboardKey.Escape world && world.Unaccompanied then
            World.exit world

    // ═══════════════════════════════════════════════════════════════
    //  GAME SCREEN
    // ═══════════════════════════════════════════════════════════════
    static member RenderGame (game : Game, world : World) =
        match AppState.gameState with
        | None -> ()
        | Some gs ->

        let dt = world.GameDelta.SecondsF
        let isHuman = AppState.config.HumanCount > 0
        // The viewpoint is fixed: seat 0 sits at the bottom even in watch
        // mode, so seats never rotate mid-game. A spectated (watch-mode) game
        // has nothing to hide, so CPU hands are drawn face-up.
        let bottomIdx = 0
        let bottomPlayer = gs.Players[bottomIdx]

        // ── Progressive deal reveal ───────────────────────────
        // The game state is fully dealt before the animation plays, so while
        // the Dealing phase runs only the cards whose 2-card batch has already
        // landed are drawn; the rest pop in as their deal step completes.
        let dealVisible (label: string) (fullCount: int) =
            match AppState.phase with
            | Shuffling ->
                // The state is already dealt during the shuffle, but nothing
                // has visibly left the deck yet: hide the new hand cards (and,
                // on the round's first deal, the new table cards) so they don't
                // flash before the deal animation delivers them.
                if label = "table" && gs.DealRound > 1 then fullCount else 0
            | Dealing ->
                let countFor stepsSubset =
                    stepsSubset
                    |> List.filter (fun (s: DealStep) -> s.DealTargetLabel = label)
                    |> List.sumBy (fun s -> s.DealCardCount)
                fullCount - countFor AppState.dealSteps
                          + countFor (List.truncate AppState.dealStepIndex AppState.dealSteps)
            | _ -> fullCount

        let handSize = dealVisible bottomPlayer.Name (List.length bottomPlayer.Hand)

        // ── Table background ──────────────────────────────────
        World.doStaticSprite "TableBg"
            [Entity.Position .= v3 0.0f 10.0f 0.0f
             Entity.Size .= v3 Ly.tableW Ly.tableH 0.0f
             Entity.StaticImage .= CardImg.tableBgAsset
             Entity.Color .= Clr.white
             Entity.Elevation .= 0.0f] world |> ignore

        // ── Draw table cards ──────────────────────────────────
        let tableCount = dealVisible "table" (List.length gs.Table)
        let definiteSet, possibleSet =
            // Strict rules: capture candidates are not pre-highlighted.
            if AppState.config.Settings.StrictRules then Set.empty, Set.empty else
            match AppState.capturePreview with
            | NoCapture -> Set.empty, Set.empty
            | SingleCapture cards -> Set.ofList cards, Set.empty
            | MultipleCaptures(definite, possible) -> Set.ofList definite, Set.ofList possible

        // Update scattered positions if needed
        if world.Advancing then
            match AppState.tableLayout with
            | RandomScatter ->
                AppState.scatteredPositions <- Helpers.computeScatteredPositions gs.Table AppState.scatteredPositions
            | StrictGrid -> ()

        // Grid mode arranges cards by value; scatter positions are per-card.
        let displayTable =
            let visible = gs.Table |> List.truncate tableCount
            match AppState.tableLayout with
            | StrictGrid -> Helpers.gridOrder visible
            | RandomScatter -> visible

        // During AnimatingPlay, hide the card being animated from the table to prevent flicker
        let animatingCard =
            match AppState.currentCardAnim with
            | Some anim when AppState.phase = AnimatingPlay && AppState.phaseTimer < anim.Duration -> Some anim.AnimCard
            | _ -> None

        for i in 0 .. Ly.maxTable - 1 do
            let name = $"TC{i}"
            if i < tableCount then
                let card = displayTable[i]
                // Hide the card being animated (Place action flicker fix)
                let isAnimating = animatingCard = Some card

                let cx, cy, rot =
                    match AppState.tableLayout with
                    | StrictGrid ->
                        let x, y = Helpers.gridPos tableCount i
                        (x, y, 0.0f)
                    | RandomScatter ->
                        match Map.tryFind card AppState.scatteredPositions with
                        | Some (sx, sy, sr) -> (sx, sy, sr)
                        | None ->
                            // Fallback to grid
                            let x, y = Helpers.gridPos tableCount i
                            (x, y, 0.0f)

                let rotation = Quaternion.CreateFromAxisAngle(Vector3.UnitZ, rot)

                // Apply capture preview tint directly on the card
                let cardTint =
                    if Set.contains card definiteSet then Clr.tintGreen
                    elif Set.contains card possibleSet then Clr.tintYellow
                    else Clr.white

                // lift the card being nudged so it stays on top while dragged
                let cardElevation = match AppState.dragState with DraggingTable (dc, _, _) when dc = card -> 4.0f | _ -> 1.0f

                World.doStaticSprite name
                    [Entity.Position @= v3 cx cy 0.0f
                     Entity.Size @= v3 Ly.cardW Ly.cardH 0.0f
                     Entity.StaticImage @= CardImg.cardAsset card
                     Entity.Color @= cardTint
                     Entity.Rotation @= rotation
                     Entity.Visible @= (not isAnimating)
                     Entity.Elevation @= cardElevation] world |> ignore

                // Overlay slot (kept hidden — preview indicated by card tint)
                World.doStaticSprite $"TCO{i}"
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.5f] world |> ignore
            else
                World.doStaticSprite name
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.0f] world |> ignore
                World.doStaticSprite $"TCO{i}"
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.5f] world |> ignore

        // ── Draw opponent hands by clockwise seat ─────────────
        let seatPlayerCount = gs.Players.Length
        let oppAtSeat (seat: Ly.Seat) =
            gs.Players
            |> List.mapi (fun i p -> (i, p))
            |> List.tryFind (fun (i, _) -> i <> bottomIdx && Ly.seatOf seatPlayerCount bottomIdx i = seat)

        // Top opponent
        match oppAtSeat Ly.SeatTop with
        | Some (_, opp) ->
            let oppHandSize = dealVisible opp.Name (List.length opp.Hand)
            for i in 0 .. Ly.maxOppHand - 1 do
                let name = $"OT{i}"
                if i < oppHandSize then
                    let startX = Ly.centerCardsX oppHandSize Ly.cardGap
                    let x = startX + float32 i * (Ly.cardW + Ly.cardGap) + Ly.cardW / 2.0f
                    World.doStaticSprite name
                        [Entity.Position @= v3 x Ly.topOppY 0.0f
                         Entity.Size .= v3 Ly.cardW Ly.cardH 0.0f
                         Entity.StaticImage @= (if isHuman then CardImg.handBackAsset else CardImg.cardAsset opp.Hand[i])
                         Entity.Visible @= true
                         Entity.Elevation .= 1.0f] world |> ignore
                else
                    World.doStaticSprite name
                        [Entity.Visible @= false
                         Entity.Elevation .= 1.0f] world |> ignore

            World.doText "OppTopLabel"
                [Entity.Position .= v3 -40.0f (Ly.topOppY - 32.0f) 0.0f
                 Entity.Size .= v3 500.0f 20.0f 0.0f
                 Entity.Text @= $"{opp.Name}  Cards:{List.length opp.CapturedCards}  Sweeps:{opp.Sweeps}"
                 Entity.TextColor .= Clr.lightSalmon
                 Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                 Entity.FontSizing .= Some 13.0f
                 Entity.Elevation .= 2.0f] world
        | None ->
            for i in 0 .. Ly.maxOppHand - 1 do
                World.doStaticSprite $"OT{i}"
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.0f] world |> ignore
            World.doText "OppTopLabel"
                [Entity.Visible @= false
                 Entity.Elevation .= 2.0f] world

        // Side opponents (3-4 player games)
        match oppAtSeat Ly.SeatLeft with
        | Some (_, opp2) ->
            let opp2Hand = dealVisible opp2.Name (List.length opp2.Hand)
            for i in 0 .. Ly.maxOppHand - 1 do
                let name = $"OL{i}"
                if i < opp2Hand then
                    let y = float32 (opp2Hand - 1 - i * 2) * (Ly.cardH * 0.3f) / 2.0f
                    World.doStaticSprite name
                        [Entity.Position @= v3 Ly.sideLeftX y 0.0f
                         Entity.Size .= v3 Ly.cardW Ly.cardH 0.0f
                         Entity.StaticImage @= (if isHuman then CardImg.handBackAsset else CardImg.cardAsset opp2.Hand[i])
                         Entity.Visible @= true
                         Entity.Elevation .= 1.0f] world |> ignore
                else
                    World.doStaticSprite name
                        [Entity.Visible @= false
                         Entity.Elevation .= 1.0f] world |> ignore
            World.doText "OppLeftLabel"
                [Entity.Position .= v3 Ly.sideLeftX -80.0f 0.0f
                 Entity.Size .= v3 130.0f 20.0f 0.0f
                 Entity.Text @= opp2.Name
                 Entity.TextColor .= Clr.lightBlue
                 Entity.FontSizing .= Some 13.0f
                 Entity.Elevation .= 2.0f] world
        | None ->
            for i in 0 .. Ly.maxOppHand - 1 do
                World.doStaticSprite $"OL{i}"
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.0f] world |> ignore
            World.doText "OppLeftLabel"
                [Entity.Visible @= false
                 Entity.Elevation .= 2.0f] world

        match oppAtSeat Ly.SeatRight with
        | Some (_, opp3) ->
            let opp3Hand = dealVisible opp3.Name (List.length opp3.Hand)
            for i in 0 .. Ly.maxOppHand - 1 do
                let name = $"OR{i}"
                if i < opp3Hand then
                    let y = float32 (opp3Hand - 1 - i * 2) * (Ly.cardH * 0.3f) / 2.0f
                    World.doStaticSprite name
                        [Entity.Position @= v3 Ly.sideRightX y 0.0f
                         Entity.Size .= v3 Ly.cardW Ly.cardH 0.0f
                         Entity.StaticImage @= (if isHuman then CardImg.handBackAsset else CardImg.cardAsset opp3.Hand[i])
                         Entity.Visible @= true
                         Entity.Elevation .= 1.0f] world |> ignore
                else
                    World.doStaticSprite name
                        [Entity.Visible @= false
                         Entity.Elevation .= 1.0f] world |> ignore
            World.doText "OppRightLabel"
                [Entity.Position .= v3 Ly.sideRightX -80.0f 0.0f
                 Entity.Size .= v3 130.0f 20.0f 0.0f
                 Entity.Text @= opp3.Name
                 Entity.TextColor .= Clr.plum
                 Entity.FontSizing .= Some 13.0f
                 Entity.Elevation .= 2.0f] world
        | None ->
            for i in 0 .. Ly.maxOppHand - 1 do
                World.doStaticSprite $"OR{i}"
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.0f] world |> ignore
            World.doText "OppRightLabel"
                [Entity.Visible @= false
                 Entity.Elevation .= 2.0f] world

        // ── Draw human hand (bottom) ──────────────────────────
        let isDraggingIdx = match AppState.dragState with Dragging(idx, _, _) -> Some idx | NotDragging | DraggingTable _ -> None
        for i in 0 .. Ly.maxHand - 1 do
            let name = $"HC{i}"
            if i < handSize then
                // Hide card being dragged from hand position
                let beingDragged = isDraggingIdx = Some i
                let startX = Ly.centerCardsX handSize Ly.cardGap
                let x = startX + float32 i * (Ly.cardW + Ly.cardGap) + Ly.cardW / 2.0f
                let isSelected = AppState.selectedCardIndex = Some i
                let isHovered = AppState.hoveredCardIndex = Some i && not beingDragged
                let yOffset = if isSelected then 10.0f elif isHovered then 6.0f else 0.0f
                let card = bottomPlayer.Hand[i]
                let img = CardImg.cardAsset card   // bottom seat is always face-up

                World.doStaticSprite name
                    [Entity.Position @= v3 x (Ly.handY + yOffset) 0.0f
                     Entity.Size .= v3 Ly.cardW Ly.cardH 0.0f
                     Entity.StaticImage @= img
                     Entity.Visible @= (not beingDragged)
                     Entity.Elevation .= 2.0f] world |> ignore

                // Border slot (kept hidden — selection indicated by y-offset)
                World.doStaticSprite $"HCB{i}"
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.9f] world |> ignore
            else
                World.doStaticSprite name
                    [Entity.Visible @= false
                     Entity.Elevation .= 2.0f] world |> ignore
                World.doStaticSprite $"HCB{i}"
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.9f] world |> ignore

        // ── Draw dragged card at cursor position ──────────────
        let dragThreshold = 8.0f
        match AppState.dragState with
        | Dragging(idx, startPos, curPos) when idx < handSize ->
            let dx = abs(curPos.X - startPos.X)
            let dy = abs(curPos.Y - startPos.Y)
            if dx > dragThreshold || dy > dragThreshold then
                let card = bottomPlayer.Hand[idx]
                let dragImg =
                    if isHuman || bottomIdx = gs.CurrentPlayerIndex then
                        CardImg.cardAsset card
                    else
                        CardImg.handBackAsset
                World.doStaticSprite "DragCard"
                    [Entity.Position @= v3 curPos.X curPos.Y 0.0f
                     Entity.Size .= v3 Ly.cardW Ly.cardH 0.0f
                     Entity.StaticImage @= dragImg
                     Entity.Visible @= true
                     Entity.Elevation .= 8.0f] world |> ignore
                // Drag border slot (kept hidden)
                World.doStaticSprite "DragCardBorder"
                    [Entity.Visible @= false
                     Entity.Elevation .= 7.9f] world |> ignore
            else
                World.doStaticSprite "DragCard"
                    [Entity.Visible @= false
                     Entity.Elevation .= 8.0f] world |> ignore
                World.doStaticSprite "DragCardBorder"
                    [Entity.Visible @= false
                     Entity.Elevation .= 7.9f] world |> ignore
        | _ ->
            World.doStaticSprite "DragCard"
                [Entity.Visible @= false
                 Entity.Elevation .= 8.0f] world |> ignore
            World.doStaticSprite "DragCardBorder"
                [Entity.Visible @= false
                 Entity.Elevation .= 7.9f] world |> ignore

        // ── Shuffle animation (riffle shuffle) ──────────────
        let isShuffling = AppState.phase = Shuffling
        let shuffleT =
            if isShuffling then AppState.phaseTimer / AppState.shuffleDuration
            else 0.0f
        let numShuffleCards = 6
        let halfN = numShuffleCards / 2
        let separation = 80.0f  // max world-unit distance between halves
        for si in 0 .. numShuffleCards - 1 do
            let name = $"ShufC{si}"
            if isShuffling then
                let isLeft = si % 2 = 0
                let stackIdx = si / 2
                // Phase 1 (0..0.4): halves separate outward
                // Phase 2 (0.4..1.0): halves slide back together, interleaving
                let sepAmount, interleaveY =
                    if shuffleT < 0.4f then
                        let p = shuffleT / 0.4f
                        let eased = 1.0f - (1.0f - p) * (1.0f - p)
                        (separation * eased, 0.0f)
                    else
                        let p = (shuffleT - 0.4f) / 0.6f
                        let eased = 1.0f - (1.0f - p) * (1.0f - p)
                        let sep = separation * (1.0f - eased)
                        let vertShift = float32 (stackIdx - halfN / 2) * 2.5f * eased
                        (sep, vertShift)
                let xOff = if isLeft then -sepAmount else sepAmount
                let yStack = float32 (stackIdx - halfN / 2) * 1.5f
                World.doStaticSprite name
                    [Entity.Position @= v3 xOff (Ly.tableY + yStack + interleaveY) 0.0f
                     Entity.Size .= v3 Ly.cardW Ly.cardH 0.0f
                     Entity.StaticImage @= CardImg.handBackAsset
                     Entity.Rotation @= Quaternion.Identity
                     Entity.Visible @= true
                     Entity.Elevation @= (5.0f + float32 si * 0.01f)] world |> ignore
            else
                World.doStaticSprite name
                    [Entity.Visible @= false
                     Entity.Elevation .= 5.0f] world |> ignore

        // ── Deal animation (cards slide from deck to destinations) ──
        let isDealing = AppState.phase = Dealing && AppState.dealStepIndex < List.length AppState.dealSteps
        let maxDealCardSlots = 4  // max cards per step
        for dci in 0 .. maxDealCardSlots - 1 do
            let name = $"DealC{dci}"
            if isDealing then
                let step = AppState.dealSteps[AppState.dealStepIndex]
                if dci < step.DealCardCount then
                    let t = AppState.dealStepElapsed / AppState.dealStepDuration
                    let eased = 1.0f - (1.0f - t) * (1.0f - t)
                    let spread = float32 (dci - step.DealCardCount / 2) * 10.0f
                    let destX = step.DealToX + spread
                    let destY = step.DealToY
                    let x = 0.0f + (destX - 0.0f) * eased   // from deck center (0, tableY) to dest
                    let y = Ly.tableY + (destY - Ly.tableY) * eased
                    World.doStaticSprite name
                        [Entity.Position @= v3 x y 0.0f
                         Entity.Size .= v3 Ly.cardW Ly.cardH 0.0f
                         Entity.StaticImage @= CardImg.handBackAsset
                         Entity.Visible @= true
                         Entity.Elevation @= (5.2f + float32 dci * 0.01f)] world |> ignore
                else
                    World.doStaticSprite name
                        [Entity.Visible @= false
                         Entity.Elevation .= 5.2f] world |> ignore
            else
                World.doStaticSprite name
                    [Entity.Visible @= false
                     Entity.Elevation .= 5.2f] world |> ignore

        // Draw deck stack during deal animation
        let deckVisible = isDealing
        for dsi in 0 .. 2 do
            let name = $"DeckS{dsi}"
            if deckVisible then
                let offset = float32 dsi * 1.5f
                World.doStaticSprite name
                    [Entity.Position @= v3 0.0f (Ly.tableY + offset) 0.0f
                     Entity.Size .= v3 Ly.cardW Ly.cardH 0.0f
                     Entity.StaticImage @= CardImg.backAsset ()
                     Entity.Visible @= true
                     Entity.Elevation @= (5.0f + float32 dsi * 0.005f)] world |> ignore
            else
                World.doStaticSprite name
                    [Entity.Visible @= false
                     Entity.Elevation .= 5.0f] world |> ignore

        // ── Card movement animation (card slides from hand to table) ──
        match AppState.currentCardAnim with
        | Some anim when AppState.phase = AnimatingPlay && AppState.phaseTimer < anim.Duration ->
            let t = AppState.phaseTimer / anim.Duration
            // Ease-out: smooth deceleration
            let eased = 1.0f - (1.0f - t) * (1.0f - t)
            let ax = anim.FromX + (anim.ToX - anim.FromX) * eased
            let ay = anim.FromY + (anim.ToY - anim.FromY) * eased
            World.doStaticSprite "AnimCard"
                [Entity.Position @= v3 ax ay 0.0f
                 Entity.Size .= v3 Ly.cardW Ly.cardH 0.0f
                 Entity.StaticImage @= CardImg.cardAsset anim.AnimCard
                 Entity.Visible @= true
                 Entity.Elevation .= 5.0f] world |> ignore
        | _ ->
            World.doStaticSprite "AnimCard"
                [Entity.Visible @= false
                 Entity.Elevation .= 5.0f] world |> ignore

        // ── Collect animation: captured cards slide toward player ──
        let isCollecting =
            match AppState.currentCollectAnim with
            | Some col when AppState.phase = AnimatingPlay &&
                            AppState.phaseTimer >= col.CollectStart &&
                            AppState.phaseTimer < col.CollectStart + col.CollectDuration -> true
            | _ -> false

        let maxCollectSlots = 16
        for ci in 0 .. maxCollectSlots - 1 do
            let name = $"ColC{ci}"
            match AppState.currentCollectAnim with
            | Some col when isCollecting && ci < List.length col.CollectCards ->
                let (card, fx, fy) = col.CollectCards[ci]
                let t = (AppState.phaseTimer - col.CollectStart) / col.CollectDuration
                let eased = 1.0f - (1.0f - t) * (1.0f - t)
                let cx = fx + (col.CollectToX - fx) * eased
                let cy = fy + (col.CollectToY - fy) * eased
                World.doStaticSprite name
                    [Entity.Position @= v3 cx cy 0.0f
                     Entity.Size .= v3 Ly.cardW Ly.cardH 0.0f
                     Entity.StaticImage @= CardImg.cardAsset card
                     Entity.Visible @= true
                     Entity.Elevation @= (5.1f + float32 ci * 0.01f)] world |> ignore
            | _ ->
                World.doStaticSprite name
                    [Entity.Visible @= false
                     Entity.Elevation .= 5.1f] world |> ignore

        // Bottom player label
        World.doText "BottomLabel"
            [Entity.Position .= v3 -40.0f (Ly.handY - 32.0f) 0.0f
             Entity.Size .= v3 500.0f 20.0f 0.0f
             Entity.Text @= $"{bottomPlayer.Name}  Cards:{List.length bottomPlayer.CapturedCards}  Sweeps:{bottomPlayer.Sweeps}"
             Entity.TextColor .= Clr.lightGreen
             Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
             Entity.FontSizing .= Some 13.0f
             Entity.Elevation .= 2.0f] world

        // ── Status bar ────────────────────────────────────────
        World.doText "StatusMsg"
            [Entity.Position .= v3 0.0f Ly.statusY 0.0f
             Entity.Size .= v3 550.0f 20.0f 0.0f
             Entity.Text @= AppState.lastPlayMessage
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.white
             Entity.FontSizing .= Some 13.0f
             Entity.Elevation .= 3.0f] world

        // ── Table-talk line (computer banter) ─────────────────
        World.doText "ChatMsg"
            [Entity.Position .= v3 0.0f (Ly.statusY - 18.0f) 0.0f
             Entity.Size .= v3 550.0f 18.0f 0.0f
             Entity.Text @= AppState.lastChat
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.gold
             Entity.FontSizing .= Some 12.0f
             Entity.Elevation .= 3.0f] world

        let turnText =
            match AppState.phase with
            | WaitingForHuman when AppState.selectedCardIndex.IsSome -> ""
            | WaitingForHuman -> ""
            | ComputerThinking -> $"{gs.Players[gs.CurrentPlayerIndex].Name} thinking..."
            | ChoosingCaptureOption
            | AnimatingPlay -> ""
            | Shuffling -> "Shuffling..."
            | Dealing -> "Dealing..."
            | RoundOver -> "Round over! [Enter] continue"
            | GameOver -> "Game over!"

        World.doText "TurnText"
            [Entity.Position .= v3 0.0f Ly.turnTextY 0.0f
             Entity.Size .= v3 550.0f 20.0f 0.0f
             Entity.Text @= turnText
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.gold
             Entity.FontSizing .= Some 13.0f
             Entity.Elevation .= 3.0f] world

        // ── Scoreboard (top right) ────────────────────────────
        World.doText "ScoreHeader"
            [Entity.Position .= v3 Ly.scoreX Ly.scoreTopY 0.0f
             Entity.Size .= v3 120.0f 20.0f 0.0f
             Entity.Text .= "Scores:"
             Entity.TextColor .= Clr.gold
             Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
             Entity.FontSizing .= Some 13.0f
             Entity.Elevation .= 3.0f] world

        for i in 0 .. 3 do
            let name = $"ScP{i}"
            if i < gs.Players.Length then
                let p = gs.Players[i]
                let cumScore = AppState.cumulativeScores |> Map.tryFind p.Name |> Option.defaultValue 0
                World.doText name
                    [Entity.Position .= v3 Ly.scoreX (Ly.scoreTopY - 18.0f - float32 i * 16.0f) 0.0f
                     Entity.Size .= v3 130.0f 20.0f 0.0f
                     Entity.Text @= $"{p.Name}: {cumScore}"
                     Entity.TextColor .= Clr.white
                     Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                     Entity.FontSizing .= Some 12.0f
                     Entity.Visible @= true
                     Entity.Elevation .= 3.0f] world
            else
                World.doText name
                    [Entity.Visible @= false
                     Entity.Elevation .= 3.0f] world

        // Round / deal info with deck indicator
        let infoY = Ly.scoreTopY - 18.0f - float32 gs.Players.Length * 16.0f - 8.0f
        World.doText "RoundInfo"
            [Entity.Position .= v3 Ly.scoreX infoY 0.0f
             Entity.Size .= v3 150.0f 18.0f 0.0f
             Entity.Text @= $"R{AppState.roundNumber} Deal {gs.DealRound}/{gs.TotalDeals}"
             Entity.TextColor .= Clr.lightGray
             Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
             Entity.FontSizing .= Some 11.0f
             Entity.Elevation .= 3.0f] world

        // Small deck card-back with count
        World.doStaticSprite "DeckIcon"
            [Entity.Position .= v3 Ly.scoreX (infoY - 18.0f) 0.0f
             Entity.Size .= v3 (Ly.cardW * 0.5f) (Ly.cardH * 0.5f) 0.0f
             Entity.StaticImage .= CardImg.backAsset ()
             Entity.Visible .= true
             Entity.Elevation .= 3.0f] world |> ignore

        World.doText "DeckCount"
            [Entity.Position .= v3 (Ly.scoreX + 24.0f) (infoY - 18.0f) 0.0f
             Entity.Size .= v3 60.0f 18.0f 0.0f
             Entity.Text @= $"{List.length gs.Deck}"
             Entity.TextColor .= Clr.lightGray
             Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
             Entity.FontSizing .= Some 12.0f
             Entity.Elevation .= 3.0f] world

        // ── "?" Help button (top-left, always declared, hidden during modal) ──
        let helpVisible = AppState.phase <> ChoosingCaptureOption
        if World.doButton "BtnHelp"
            [Entity.Position .= v3 -305.0f 168.0f 0.0f
             Entity.Size .= v3 40.0f 24.0f 0.0f
             Entity.Text .= "?"
             Entity.Visible @= helpVisible
             Entity.Elevation .= 4.0f] world then
            if helpVisible then
                AppState.rulesPage <- 0
                AppState.rulesReturnMode <- KasinoPlaying
                game.SetKasinoMode KasinoRules world

        // ── Layout toggle button (top-left, next to help) ─────────────
        let layoutLabel = match AppState.tableLayout with StrictGrid -> "Scatter" | RandomScatter -> "Grid"
        if World.doButton "BtnLayout"
            [Entity.Position .= v3 -250.0f 168.0f 0.0f
             Entity.Size .= v3 60.0f 24.0f 0.0f
             Entity.Text @= layoutLabel
             Entity.Visible @= helpVisible
             Entity.Elevation .= 4.0f] world then
            if helpVisible then
                match AppState.tableLayout with
                | StrictGrid ->
                    AppState.tableLayout <- RandomScatter
                    AppState.scatteredPositions <- Helpers.computeScatteredPositions gs.Table Map.empty
                | RandomScatter ->
                    AppState.tableLayout <- StrictGrid
                    AppState.scatteredPositions <- Map.empty

        // ── "Menu" button (top-left, next to layout) ───────────
        if World.doButton "BtnMenu"
            [Entity.Position .= v3 -185.0f 168.0f 0.0f
             Entity.Size .= v3 60.0f 24.0f 0.0f
             Entity.Text .= "Menu"
             Entity.Visible @= helpVisible
             Entity.Elevation .= 4.0f] world then
            if helpVisible then
                AppState.resetMenu ()
                game.SetKasinoMode KasinoMenu world

        // ── "Play Card" button (always declared) ──────────────
        let notDragging = match AppState.dragState with NotDragging -> true | Dragging _ | DraggingTable _ -> false
        let btnPlayVisible = AppState.phase = WaitingForHuman && isHuman && AppState.selectedCardIndex.IsSome && notDragging
        let btnPlayLabel =
            if btnPlayVisible then
                match AppState.capturePreview with
                | NoCapture -> "Place on Table"
                | SingleCapture cards ->
                    // Strict rules hide how many cards the capture would take.
                    if AppState.config.Settings.StrictRules then "Capture Cards"
                    else $"Capture {cards.Length} Cards"
                | MultipleCaptures _ -> "Play (Choose)"
            else ""
        if World.doButton "BtnPlay"
            [Entity.Position .= v3 0.0f (Ly.handY + 45.0f) 0.0f
             Entity.Size .= v3 180.0f Ly.btnH 0.0f
             Entity.Text @= btnPlayLabel
             Entity.Visible @= btnPlayVisible
             Entity.Elevation .= 5.0f] world then
            if btnPlayVisible then
                match AppState.selectedCardIndex with
                | Some idx -> Helpers.processHumanPlay idx
                | None -> ()

        // ── "Place Instead" button — Standard Kasino only, where declining a
        // capture is legal. Shown beside Play when the selected card captures.
        let btnPlaceVisible =
            btnPlayVisible
            && gs.Variant = StandardKasino
            && (match AppState.capturePreview with NoCapture -> false | SingleCapture _ | MultipleCaptures _ -> true)
        if World.doButton "BtnPlaceInstead"
            [Entity.Position .= v3 165.0f (Ly.handY + 45.0f) 0.0f
             Entity.Size .= v3 130.0f Ly.btnH 0.0f
             Entity.Text .= "Place Instead"
             Entity.Visible @= btnPlaceVisible
             Entity.Elevation .= 5.0f] world then
            if btnPlaceVisible then
                match AppState.selectedCardIndex with
                | Some idx -> Helpers.processHumanPlace idx
                | None -> ()

        // ── "Continue" button (always declared, visible in RoundOver) ──
        let continueVisible = AppState.phase = RoundOver
        if World.doButton "BtnContinue"
            [Entity.Position .= v3 0.0f -20.0f 0.0f
             Entity.Size .= v3 160.0f Ly.btnH 0.0f
             Entity.Text .= "Continue"
             Entity.Visible @= continueVisible
             Entity.Elevation .= 5.0f] world then
            if continueVisible then
                AppState.enterScores ()
                game.SetKasinoMode KasinoScores world

        // ── Modal overlay & capture option buttons (always declared) ──
        let isModal = AppState.phase = ChoosingCaptureOption
        World.doStaticSprite "ModalBg"
            [Entity.Position .= v3 0.0f 0.0f 0.0f
             Entity.Size .= v3 900.0f 700.0f 0.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Clr.modalOverlay
             Entity.Visible @= isModal
             Entity.Elevation .= 6.0f] world |> ignore

        // The modal paginates: 6 option slots per page (up to 64 options can
        // exist), with More/Place/Cancel at fixed rows below the slots. All
        // dynamic values use @= — a `.=` computed from modal state would be
        // frozen at first-frame (non-modal) values forever.
        let optionsPerPage = 6
        let capturePageCount = max 1 ((AppState.captureOptions.Length + optionsPerPage - 1) / optionsPerPage)
        let capturePageClamped = ((AppState.capturePage % capturePageCount) + capturePageCount) % capturePageCount
        let capturePageStart = capturePageClamped * optionsPerPage

        World.doText "CaptureHeader"
            [Entity.Position .= v3 0.0f 150.0f 0.0f
             Entity.Size .= v3 400.0f 24.0f 0.0f
             Entity.Text @= (if isModal then "Choose which cards to capture:" else "")
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.gold
             Entity.Visible @= isModal
             Entity.Elevation .= 7.0f] world

        for i in 0 .. optionsPerPage - 1 do
            let name = $"BtnOpt{i}"
            let optIdx = capturePageStart + i
            let optVisible = isModal && optIdx < AppState.captureOptions.Length
            let y = 120.0f - float32 i * 34.0f
            if optVisible then
                let opt = AppState.captureOptions[optIdx]
                if World.doButton name
                    [Entity.Position .= v3 0.0f y 0.0f
                     Entity.Size .= v3 340.0f 28.0f 0.0f
                     Entity.Text @= ""
                     Entity.Visible @= true
                     Entity.Elevation .= 7.0f] world then
                    Helpers.processCapture AppState.captureCardIdx opt
            else
                World.doButton name
                    [Entity.Visible @= false
                     Entity.Elevation .= 7.0f] world |> ignore
            // Overlay label in fixed slots: white prefix/count, each card name
            // tinted by suit (red suits reddish, black suits gray) so the
            // options read at a glance. Nu has no text measuring, hence slots.
            let captured = if optVisible then AppState.captureOptions[optIdx].Captured else []
            World.doText $"OptPre{i}"
                [Entity.Position .= v3 -150.0f y 0.0f
                 Entity.Size .= v3 40.0f 24.0f 0.0f
                 Entity.Text @= (if optVisible then $"{i + 1}:" else "")
                 Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                 Entity.TextColor .= Clr.white
                 Entity.FontSizing .= Some 13.0f
                 Entity.Visible @= optVisible
                 Entity.Elevation .= 7.5f] world
            for j in 0 .. 5 do
                let cardTxt, cardCol =
                    if optVisible && j < captured.Length then
                        if j = 5 && captured.Length > 6 then ("…", Clr.white)
                        else
                            let c = captured[j]
                            (Cards.display c, (match c.Suit with Hearts | Diamonds -> Clr.cardRed | Spades | Clubs -> Clr.cardGray))
                    else ("", Clr.white)
                World.doText $"OptCard{i}_{j}"
                    [Entity.Position .= v3 (-109.0f + float32 j * 38.0f) y 0.0f
                     Entity.Size .= v3 38.0f 24.0f 0.0f
                     Entity.Text @= cardTxt
                     Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                     Entity.TextColor @= cardCol
                     Entity.FontSizing .= Some 13.0f
                     Entity.Visible @= (cardTxt <> "")
                     Entity.Elevation .= 7.5f] world
            World.doText $"OptCount{i}"
                [Entity.Position .= v3 140.0f y 0.0f
                 Entity.Size .= v3 60.0f 24.0f 0.0f
                 Entity.Text @= (if optVisible then $"({captured.Length})" else "")
                 Entity.Justification .= Justified (JustifyRight, JustifyMiddle)
                 Entity.TextColor .= Clr.white
                 Entity.FontSizing .= Some 13.0f
                 Entity.Visible @= optVisible
                 Entity.Elevation .= 7.5f] world

        let moreVisible = isModal && capturePageCount > 1
        if World.doButton "BtnOptMore"
            [Entity.Position .= v3 0.0f -90.0f 0.0f
             Entity.Size .= v3 340.0f 28.0f 0.0f
             Entity.Text @= (if moreVisible then $"More options ({capturePageClamped + 1}/{capturePageCount})" else "")
             Entity.Visible @= moreVisible
             Entity.Elevation .= 7.0f] world then
            if moreVisible then
                AppState.capturePage <- (capturePageClamped + 1) % capturePageCount

        // Standard Kasino: capturing is optional — the capture may be declined.
        let placeOptVisible = isModal && gs.Variant = StandardKasino
        if World.doButton "BtnOptPlace"
            [Entity.Position .= v3 0.0f -124.0f 0.0f
             Entity.Size .= v3 340.0f 28.0f 0.0f
             Entity.Text .= "Place on table instead"
             Entity.Visible @= placeOptVisible
             Entity.Elevation .= 7.0f] world then
            if placeOptVisible then
                Helpers.processHumanPlace AppState.captureCardIdx

        // Strict rules: the touched card must be played — no cancelling out.
        let cancelVisible = isModal && not AppState.config.Settings.StrictRules
        if World.doButton "BtnCancel"
            [Entity.Position .= v3 0.0f -158.0f 0.0f
             Entity.Size .= v3 140.0f 28.0f 0.0f
             Entity.Text .= "Cancel"
             Entity.Visible @= cancelVisible
             Entity.Elevation .= 7.0f] world then
            if cancelVisible then
                AppState.phase <- WaitingForHuman
                AppState.selectedCardIndex <- None
                AppState.capturePreview <- NoCapture

        // ── Phase-specific input handling (advancing only) ────
        if world.Advancing then
            match AppState.phase with
            | Shuffling | Dealing | ComputerThinking | AnimatingPlay when
                World.isKeyboardKeyPressed KeyboardKey.Escape world ->
                // Escape returns to the menu from phases with no Escape
                // handling of their own — in particular watch-AI-only games,
                // which never reach the human input branch.
                AppState.resetMenu ()
                game.SetKasinoMode KasinoMenu world

            | Dealing ->
                AppState.dealStepElapsed <- AppState.dealStepElapsed + dt
                if AppState.dealStepIndex >= List.length AppState.dealSteps then
                    Helpers.advanceTurn ()
                elif AppState.dealStepElapsed >= AppState.dealStepDuration then
                    AppState.dealStepIndex <- AppState.dealStepIndex + 1
                    AppState.dealStepElapsed <- 0.0f

            | WaitingForHuman when isHuman ->
                let mousePos = World.getMousePosition2dWorld false world
                let mx = mousePos.X
                let my = mousePos.Y
                let dragThreshold = 8.0f

                // Compute hover (only when not dragging)
                match AppState.dragState with
                | NotDragging ->
                    let newHovered =
                        [ 0 .. handSize - 1 ]
                        |> List.tryFindBack (fun i ->
                            let startX = Ly.centerCardsX handSize Ly.cardGap
                            let cx = startX + float32 i * (Ly.cardW + Ly.cardGap) + Ly.cardW / 2.0f
                            let cy = Ly.handY + (if AppState.selectedCardIndex = Some i then 15.0f else 0.0f)
                            mx >= cx - Ly.cardW / 2.0f && mx <= cx + Ly.cardW / 2.0f &&
                            my >= cy - Ly.cardH / 2.0f && my <= cy + Ly.cardH / 2.0f)
                    AppState.hoveredCardIndex <- newHovered
                | Dragging _ | DraggingTable _ ->
                    AppState.hoveredCardIndex <- None

                // Update capture preview for selected/dragged card
                let previewIdx =
                    match AppState.dragState with
                    | Dragging(idx, _, _) -> Some idx
                    | NotDragging | DraggingTable _ -> AppState.selectedCardIndex
                match previewIdx with
                | Some idx when idx < handSize ->
                    let card = bottomPlayer.Hand[idx]
                    AppState.capturePreview <- Helpers.computePreview card gs.Table
                | _ ->
                    if AppState.selectedCardIndex.IsNone then
                        AppState.capturePreview <- NoCapture

                // ── Drag & drop handling ──
                match AppState.dragState with
                | NotDragging ->
                    // ── Keyboard shortcuts: number keys select, Enter confirms, Escape deselects/menu ──
                    if World.isKeyboardKeyPressed KeyboardKey.Escape world then
                        if AppState.selectedCardIndex.IsSome then
                            AppState.selectedCardIndex <- None
                            AppState.capturePreview <- NoCapture
                        else
                            AppState.resetMenu ()
                            game.SetKasinoMode KasinoMenu world
                    elif World.isKeyboardKeyPressed KeyboardKey.Enter world then
                        match AppState.selectedCardIndex with
                        | Some idx -> Helpers.processHumanPlay idx
                        | None -> ()
                    elif World.isKeyboardKeyPressed KeyboardKey.Num1 world && handSize >= 1 then
                        if AppState.selectedCardIndex = Some 0 then Helpers.processHumanPlay 0
                        else AppState.selectedCardIndex <- Some 0
                    elif World.isKeyboardKeyPressed KeyboardKey.Num2 world && handSize >= 2 then
                        if AppState.selectedCardIndex = Some 1 then Helpers.processHumanPlay 1
                        else AppState.selectedCardIndex <- Some 1
                    elif World.isKeyboardKeyPressed KeyboardKey.Num3 world && handSize >= 3 then
                        if AppState.selectedCardIndex = Some 2 then Helpers.processHumanPlay 2
                        else AppState.selectedCardIndex <- Some 2
                    elif World.isKeyboardKeyPressed KeyboardKey.Num4 world && handSize >= 4 then
                        if AppState.selectedCardIndex = Some 3 then Helpers.processHumanPlay 3
                        else AppState.selectedCardIndex <- Some 3
                    elif World.isMouseButtonPressed MouseLeft world then
                        // Check if clicking on a hand card
                        let clickedIdx =
                            [ 0 .. handSize - 1 ]
                            |> List.tryFindBack (fun i ->
                                let startX = Ly.centerCardsX handSize Ly.cardGap
                                let cx = startX + float32 i * (Ly.cardW + Ly.cardGap) + Ly.cardW / 2.0f
                                let cy = Ly.handY + (if AppState.selectedCardIndex = Some i then 15.0f else 0.0f)
                                mx >= cx - Ly.cardW / 2.0f && mx <= cx + Ly.cardW / 2.0f &&
                                my >= cy - Ly.cardH / 2.0f && my <= cy + Ly.cardH / 2.0f)

                        match clickedIdx with
                        | Some idx when AppState.selectedCardIndex = Some idx ->
                            // Tap same card again -> confirm play
                            Helpers.processHumanPlay idx
                        | Some idx ->
                            // Start potential drag and select card
                            AppState.selectedCardIndex <- Some idx
                            let card = bottomPlayer.Hand[idx]
                            AppState.capturePreview <- Helpers.computePreview card gs.Table
                            AppState.dragState <- Dragging(idx, mousePos, mousePos)
                        | None ->
                            // Don't deselect if clicking on the Play or Place
                            // Instead button areas
                            let btnCenterY = Ly.handY + 45.0f
                            let onPlaceBtn =
                                btnPlaceVisible &&
                                mx >= 100.0f && mx <= 230.0f &&
                                abs (my - btnCenterY) <= Ly.btnH / 2.0f
                            let onPlayBtn =
                                (btnPlayVisible &&
                                 abs mx <= 90.0f &&
                                 abs (my - btnCenterY) <= Ly.btnH / 2.0f)
                                || onPlaceBtn
                            // Otherwise, grab a table card to nudge it (scatter mode only —
                            // the strict grid never overlaps, so nothing to untangle there).
                            let tableCardOpt =
                                if not onPlayBtn && AppState.tableLayout = RandomScatter
                                then Helpers.tableCardAtScatter gs.Table mx my
                                else None
                            match tableCardOpt with
                            | Some card ->
                                match Map.tryFind card AppState.scatteredPositions with
                                | Some (sx, sy, _) -> AppState.dragState <- DraggingTable(card, mx - sx, my - sy)
                                | None -> ()
                            | None ->
                                if not onPlayBtn then
                                    AppState.selectedCardIndex <- None
                                    AppState.capturePreview <- NoCapture

                | DraggingTable(card, gdx, gdy) ->
                    if World.isMouseButtonDown MouseLeft world then
                        // reposition the card under the cursor, kept inside the table area
                        let cx, cy = Helpers.clampScatterCenter (mx - gdx) (my - gdy)
                        let rot = match Map.tryFind card AppState.scatteredPositions with Some (_, _, r) -> r | None -> 0.0f
                        AppState.scatteredPositions <- Map.add card (cx, cy, rot) AppState.scatteredPositions
                    else
                        AppState.dragState <- NotDragging

                | Dragging(idx, startPos, _) ->
                    if World.isMouseButtonDown MouseLeft world then
                        // Continue dragging — update position
                        AppState.dragState <- Dragging(idx, startPos, mousePos)
                    else
                        // Released — check if it was a drag or click
                        let dx = abs(mx - startPos.X)
                        let dy = abs(my - startPos.Y)
                        if dx > dragThreshold || dy > dragThreshold then
                            // Real drag — check if dropped on table area
                            // Table area: centered at (0, Ly.tableY), size Ly.tableW x Ly.tableH
                            let halfW = Ly.tableW / 2.0f
                            let halfH = Ly.tableH / 2.0f
                            let inTable =
                                mx >= -halfW && mx <= halfW &&
                                my >= (Ly.tableY - halfH) && my <= (Ly.tableY + halfH)
                            if inTable then
                                AppState.dragState <- NotDragging
                                Helpers.processHumanPlay idx
                            else
                                // Dropped outside table — cancel drag, keep selected
                                AppState.dragState <- NotDragging
                                AppState.selectedCardIndex <- Some idx
                        else
                            // Just a click — select the card
                            AppState.dragState <- NotDragging
                            AppState.selectedCardIndex <- Some idx

            | WaitingForHuman ->
                // AI-only mode: auto-advance
                AppState.phase <- ComputerThinking
                AppState.phaseTimer <- 0.0f

            | Shuffling ->
                AppState.phaseTimer <- AppState.phaseTimer + dt
                if AppState.phaseTimer >= AppState.shuffleDuration then
                    // Start deal animation
                    let isFirst = gs.DealRound = 1
                    AppState.dealSteps <- Helpers.buildDealSteps gs isFirst
                    AppState.dealStepIndex <- 0
                    AppState.dealStepElapsed <- 0.0f
                    AppState.phase <- Dealing

            | ComputerThinking ->
                AppState.phaseTimer <- AppState.phaseTimer + dt
                if AppState.phaseTimer >= AppState.computerDelay then
                    let player = gs.Players[gs.CurrentPlayerIndex]
                    let style = GameEngine.computerStyle AppState.config gs.CurrentPlayerIndex
                    let turnResult = GameEngine.playComputerTurnStyled style gs
                    // The viewpoint is fixed (seat 0 at the bottom, also in
                    // watch mode), so animations anchor to the player's seat.
                    let seat = Ly.seatOf gs.Players.Length 0 gs.CurrentPlayerIndex
                    AppState.currentCollectAnim <- Helpers.buildCollectAnimation turnResult.PlayResult seat gs.Table
                    // Compute animation target: scatter position for Place, table center for Capture
                    let toX, toY =
                        match turnResult.PlayResult, AppState.tableLayout with
                        | Place placedCard, RandomScatter ->
                            let newPos = Helpers.computeScatteredPositions turnResult.NewState.Table AppState.scatteredPositions
                            AppState.scatteredPositions <- newPos
                            match Map.tryFind placedCard newPos with
                            | Some(sx, sy, _) -> (sx, sy)
                            | None -> (0.0f, Ly.tableY)
                        | _ -> (0.0f, Ly.tableY)
                    // Animate the card the AI actually played (from its real
                    // slot in the hand), not Hand[0] — anything else leaks a
                    // hidden card face-up.
                    let playedCard =
                        match turnResult.PlayResult with
                        | Capture(hc, _, _) | Place hc -> hc
                    if not (List.isEmpty player.Hand) then
                        let oppHandSize = List.length player.Hand
                        let cardIdx = player.Hand |> List.tryFindIndex ((=) playedCard) |> Option.defaultValue 0
                        let fromX, fromY =
                            match seat with
                            | Ly.SeatLeft ->
                                (Ly.sideLeftX, float32 (oppHandSize - 1 - cardIdx * 2) * (Ly.cardH * 0.3f) / 2.0f)
                            | Ly.SeatRight ->
                                (Ly.sideRightX, float32 (oppHandSize - 1 - cardIdx * 2) * (Ly.cardH * 0.3f) / 2.0f)
                            | seatTB ->
                                (Ly.centerCardsX oppHandSize Ly.cardGap + float32 cardIdx * (Ly.cardW + Ly.cardGap) + Ly.cardW / 2.0f,
                                 (if seatTB = Ly.SeatBottom then Ly.handY else Ly.topOppY))
                        AppState.currentCardAnim <- Some
                            { AnimCard = playedCard
                              FromX = fromX; FromY = fromY
                              ToX = toX; ToY = toY
                              Duration = AppState.cardSlideDuration }
                    else
                        AppState.currentCardAnim <- None
                    let msg = Helpers.formatPlayResult player.Name turnResult.PlayResult
                    AppState.gameState <- Some turnResult.NewState
                    AppState.lastPlayMessage <- msg
                    AppState.lastEval <- Some turnResult.Evaluation
                    // Table-talk: let the computer banter about what it just did.
                    if AppState.config.Settings.ChatEnabled then
                        let mood =
                            match turnResult.PlayResult with
                            | Capture(_, _, true)  -> Chat.Sweep
                            | Capture(_, _, false) -> Chat.Capture
                            | Place _              -> Chat.Place
                        let seed = AppState.roundNumber * 97 + List.length turnResult.NewState.Deck
                        AppState.lastChat <- $"{player.Name}: {Chat.pick seed mood}"
                    else
                        AppState.lastChat <- ""
                    AppState.phase <- AnimatingPlay
                    AppState.phaseTimer <- 0.0f

            | AnimatingPlay ->
                AppState.phaseTimer <- AppState.phaseTimer + dt
                if AppState.phaseTimer >= AppState.animDelay then
                    AppState.currentCardAnim <- None
                    AppState.currentCollectAnim <- None
                    Helpers.advanceTurn ()

            | ChoosingCaptureOption ->
                // Escape to cancel
                if World.isKeyboardKeyPressed KeyboardKey.Escape world then
                    AppState.phase <- WaitingForHuman
                    AppState.selectedCardIndex <- None
                    AppState.capturePreview <- NoCapture
                else
                    // Number keys choose from the visible page of the modal
                    let pickVisible n =
                        let optIdx = capturePageStart + n
                        if optIdx < AppState.captureOptions.Length then
                            Helpers.processCapture AppState.captureCardIdx AppState.captureOptions[optIdx]
                    if World.isKeyboardKeyPressed KeyboardKey.Num1 world then pickVisible 0
                    elif World.isKeyboardKeyPressed KeyboardKey.Num2 world then pickVisible 1
                    elif World.isKeyboardKeyPressed KeyboardKey.Num3 world then pickVisible 2
                    elif World.isKeyboardKeyPressed KeyboardKey.Num4 world then pickVisible 3

            | RoundOver ->
                if World.isKeyboardKeyPressed KeyboardKey.Enter world then
                    AppState.enterConsumed <- true
                    AppState.enterScores ()
                    game.SetKasinoMode KasinoScores world
                elif World.isKeyboardKeyPressed KeyboardKey.Escape world then
                    AppState.resetMenu ()
                    game.SetKasinoMode KasinoMenu world

            | GameOver -> ()

    // ═══════════════════════════════════════════════════════════════
    //  SCORE SCREEN
    // ═══════════════════════════════════════════════════════════════
    static member RenderScores (game : Game, world : World) =

        let title =
            if AppState.scoreIsGameOver then "Game Over!"
            else $"Round {AppState.roundNumber} Results"

        World.doText "ScTitle"
            [Entity.Position .= v3 0.0f 165.0f 0.0f
             Entity.Size .= v3 350.0f 30.0f 0.0f
             Entity.Text @= title
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.gold
             Entity.FontSizing .= Some 22.0f
             Entity.Elevation .= 1.0f] world

        let varName =
            match AppState.config.Variant with StandardKasino -> "Standard Kasino" | LaistoKasino -> "Laistokasino"
        World.doText "ScVariant"
            [Entity.Position .= v3 0.0f 145.0f 0.0f
             Entity.Size .= v3 300.0f 20.0f 0.0f
             Entity.Text @= varName
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.gray
             Entity.Elevation .= 1.0f] world

        // Category labels
        let categories =
            [| "Most cards (1pt)"; "Most spades (2pts)"; "Aces (1pt each)"
               "Diamond 10 (2pts)"; "Spade 2 (1pt)"; "Sweeps (1pt each)"
               "----------------"; "Round total"; ""; "Cumulative" |]

        let baseY = 115.0f
        let rowH = 18.0f

        for i in 0 .. categories.Length - 1 do
            World.doText $"ScCat{i}"
                [Entity.Position .= v3 -280.0f (baseY - float32 i * rowH) 0.0f
                 Entity.Size .= v3 200.0f 18.0f 0.0f
                 Entity.Text .= categories[i]
                 Entity.TextColor .= Clr.lightGray
                 Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                 Entity.FontSizing .= Some 13.0f
                 Entity.Elevation .= 1.0f] world

        // Player columns (up to 4)
        let numPlayers = AppState.scoreBreakdowns.Length
        let colW = 120.0f

        for col in 0 .. 3 do
            if col < numPlayers then
                let (player, breakdown) = AppState.scoreBreakdowns[col]
                let colX = -80.0f + float32 col * colW

                // Player name header
                World.doText $"ScPN{col}"
                    [Entity.Position .= v3 colX (baseY + rowH) 0.0f
                     Entity.Text @= player.Name
                     Entity.TextColor .= Clr.white
                     Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                     Entity.FontSizing .= Some 13.0f
                     Entity.Visible @= true
                     Entity.Elevation .= 1.0f] world

                let rows =
                    [| string breakdown.MostCards; string breakdown.MostSpades
                       string breakdown.Aces; string breakdown.DiamondTen
                       string breakdown.SpadeTwo; string breakdown.Sweeps
                       ""; string breakdown.Total; ""
                       string (AppState.cumulativeScores |> Map.tryFind player.Name |> Option.defaultValue 0) |]

                for i in 0 .. rows.Length - 1 do
                    let rowColor =
                        if i = 7 then Clr.yellow       // round total
                        elif i = 9 then Clr.gold       // cumulative
                        else Clr.white
                    World.doText $"ScV{col}_{i}"
                        [Entity.Position .= v3 colX (baseY - float32 i * rowH) 0.0f
                         Entity.Text @= rows[i]
                         Entity.TextColor @= rowColor
                         Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                         Entity.FontSizing .= Some 13.0f
                         Entity.Visible @= true
                         Entity.Elevation .= 1.0f] world
            else
                // Empty column slots
                World.doText $"ScPN{col}"
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.0f] world
                for i in 0 .. 9 do
                    World.doText $"ScV{col}_{i}"
                        [Entity.Visible @= false
                         Entity.Elevation .= 1.0f] world

        // Winner announcement (game over only). An exact tie for the deciding
        // score names every tied player rather than an arbitrary one.
        if AppState.scoreIsGameOver then
            let scores = AppState.cumulativeScores |> Map.toList
            let bestScore =
                match AppState.config.Variant with
                | StandardKasino -> scores |> List.map snd |> List.max
                | LaistoKasino   -> scores |> List.map snd |> List.min
            let winners = scores |> List.filter (fun (_, s) -> s = bestScore) |> List.map fst
            let winnerText =
                match winners with
                | [ w ] -> $"{w} wins with {bestScore} points!"
                | ws -> String.concat " & " ws + $" tie with {bestScore} points!"
            World.doText "ScWinner"
                [Entity.Position .= v3 0.0f (baseY - float32 categories.Length * rowH - 16.0f) 0.0f
                 Entity.Size .= v3 450.0f 24.0f 0.0f
                 Entity.Text @= winnerText
                 Entity.TextColor .= Clr.gold
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.FontSizing .= Some 18.0f
                 Entity.Visible @= true
                 Entity.Elevation .= 1.0f] world
        else
            World.doText "ScWinner"
                [Entity.Visible @= false
                 Entity.Elevation .= 1.0f] world

        // Action button
        let btnLabel = if AppState.scoreIsGameOver then "Back to Menu" else "Next Round"
        if World.doButton "BtnScAction"
            [Entity.Position .= v3 0.0f -165.0f 0.0f
             Entity.Size .= v3 180.0f Ly.btnH 0.0f
             Entity.Text @= btnLabel
             Entity.Elevation .= 2.0f] world then
            if AppState.scoreIsGameOver then
                AppState.resetMenu ()
                game.SetKasinoMode KasinoMenu world
            else
                AppState.startNextRound ()
                game.SetKasinoMode KasinoPlaying world

        if world.Advancing then
            if World.isKeyboardKeyPressed KeyboardKey.Enter world && not AppState.enterConsumed then
                if AppState.scoreIsGameOver then
                    AppState.resetMenu ()
                    game.SetKasinoMode KasinoMenu world
                else
                    AppState.startNextRound ()
                    game.SetKasinoMode KasinoPlaying world

    // ═══════════════════════════════════════════════════════════════
    //  RULES SCREEN
    // ═══════════════════════════════════════════════════════════════
    static member RenderRules (game : Game, world : World) =

        World.doText "RuTitle"
            [Entity.Position .= v3 0.0f 165.0f 0.0f
             Entity.Size .= v3 400.0f 28.0f 0.0f
             Entity.Text .= "How to Play Kasino"
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.gold
             Entity.FontSizing .= Some 16.0f
             Entity.Elevation .= 1.0f] world

        let pageTitle = RulesContent.pageTitle AppState.rulesPage
        World.doText "RuPageTitle"
            [Entity.Position .= v3 0.0f 148.0f 0.0f
             Entity.Size .= v3 400.0f 20.0f 0.0f
             Entity.Text @= pageTitle
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.white
             Entity.FontSizing .= Some 13.0f
             Entity.Elevation .= 1.0f] world

        let indicator = $"Page {AppState.rulesPage + 1} / {RulesContent.totalPages}"
        World.doText "RuPageNum"
            [Entity.Position .= v3 0.0f 135.0f 0.0f
             Entity.Size .= v3 200.0f 18.0f 0.0f
             Entity.Text @= indicator
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.gray
             Entity.Elevation .= 1.0f] world

        // Separator line
        World.doStaticSprite "RuSep"
            [Entity.Position .= v3 0.0f 128.0f 0.0f
             Entity.Size .= v3 550.0f 2.0f 0.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Clr.gray
             Entity.Elevation .= 1.0f] world |> ignore

        // Page body: text lines OR a visual page of card images.
        // Both entity pools are emitted every frame; the inactive pool is
        // hidden so nothing lingers when switching pages (ImSim).
        let lines, cardSpots, caps =
            match RulesContent.pages.[AppState.rulesPage] with
            | RulesContent.TextPage (_, ls) -> ls, [||], [||]
            | RulesContent.VisualPage (_, cs, cps) -> [||], List.toArray cs, List.toArray cps

        // Body text (max 14 lines)
        let lineH = 16.0f
        let startY = 118.0f

        for i in 0 .. 13 do
            let name = $"RuLine{i}"
            if i < lines.Length then
                let lineColor =
                    if lines[i].StartsWith "  " then Clr.lightGray
                    elif lines[i] = "" then color 0.0f 0.0f 0.0f 0.0f
                    else Clr.white
                World.doText name
                    [Entity.Position @= v3 0.0f (startY - float32 i * lineH) 0.0f
                     Entity.Size @= v3 520.0f 18.0f 0.0f
                     Entity.Text @= lines[i]
                     Entity.TextColor @= lineColor
                     Entity.Justification @= Justified (JustifyLeft, JustifyMiddle)
                     Entity.FontSizing @= Some 13.0f
                     Entity.Visible @= true
                     Entity.Elevation .= 1.0f] world
            else
                World.doText name
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.0f] world

        // Visual-page card images (max 12)
        for i in 0 .. 11 do
            let name = $"RuCard{i}"
            if i < cardSpots.Length then
                let spot = cardSpots[i]
                World.doStaticSprite name
                    [Entity.Position @= v3 spot.X spot.Y 0.0f
                     Entity.Size @= v3 38.0f 49.0f 0.0f
                     Entity.StaticImage @= CardImg.cardAsset spot.Card
                     Entity.Color @= Clr.white
                     Entity.Visible @= true
                     Entity.Elevation .= 1.0f] world |> ignore
            else
                World.doStaticSprite name
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.0f] world |> ignore

        // Visual-page captions (max 12)
        for i in 0 .. 11 do
            let name = $"RuCap{i}"
            if i < caps.Length then
                let cap = caps[i]
                let justH = if cap.Center then JustifyCenter else JustifyLeft
                let posX = if cap.Center then cap.X else cap.X + cap.W / 2.0f
                World.doText name
                    [Entity.Position @= v3 posX cap.Y 0.0f
                     Entity.Size @= v3 cap.W 18.0f 0.0f
                     Entity.Text @= cap.Text
                     Entity.TextColor @= cap.Col
                     Entity.Justification @= Justified (justH, JustifyMiddle)
                     Entity.FontSizing @= Some cap.Size
                     Entity.Visible @= true
                     Entity.Elevation .= 1.0f] world
            else
                World.doText name
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.0f] world

        // Back button
        if World.doButton "BtnRuBack"
            [Entity.Position .= v3 -260.0f -155.0f 0.0f
             Entity.Size .= v3 100.0f Ly.btnH 0.0f
             Entity.Text .= "Back"
             Entity.Elevation .= 2.0f] world then
            game.SetKasinoMode AppState.rulesReturnMode world

        // Previous button
        let prevVisible = AppState.rulesPage > 0
        if World.doButton "BtnRuPrev"
            [Entity.Position .= v3 -80.0f -155.0f 0.0f
             Entity.Size .= v3 100.0f Ly.btnH 0.0f
             Entity.Text .= "Previous"
             Entity.Visible @= prevVisible
             Entity.Elevation .= 2.0f] world then
            if prevVisible then
                AppState.rulesPage <- AppState.rulesPage - 1

        // Next button
        let nextVisible = AppState.rulesPage < RulesContent.totalPages - 1
        if World.doButton "BtnRuNext"
            [Entity.Position .= v3 80.0f -155.0f 0.0f
             Entity.Size .= v3 100.0f Ly.btnH 0.0f
             Entity.Text .= "Next"
             Entity.Visible @= nextVisible
             Entity.Elevation .= 2.0f] world then
            if nextVisible then
                AppState.rulesPage <- AppState.rulesPage + 1

        // Keyboard navigation
        if world.Advancing then
            if World.isKeyboardKeyPressed KeyboardKey.Escape world then
                game.SetKasinoMode AppState.rulesReturnMode world

            if World.isKeyboardKeyPressed KeyboardKey.Left world && AppState.rulesPage > 0 then
                AppState.rulesPage <- AppState.rulesPage - 1

            if World.isKeyboardKeyPressed KeyboardKey.Right world && AppState.rulesPage < RulesContent.totalPages - 1 then
                AppState.rulesPage <- AppState.rulesPage + 1

        World.doText "RuKeyHint"
            [Entity.Position .= v3 0.0f -172.0f 0.0f
             Entity.Size .= v3 400.0f 18.0f 0.0f
             Entity.Text .= "Arrow keys: navigate  |  Esc: back"
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.gray
             Entity.FontSizing .= Some 11.0f
             Entity.Elevation .= 1.0f] world
