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

    // Game state
    let mutable config : GameEngine.GameConfig =
        { Variant = StandardKasino; PlayerCount = 2; HumanCount = 1; Seed = None; TargetScore = 16 }
    let mutable gameState : GameEngine.GameState option = None
    let mutable phase = Dealing
    let mutable phaseTimer = 0.0f
    let mutable selectedCardIndex : int option = None
    let mutable hoveredCardIndex : int option = None
    let mutable capturePreview = NoCapture
    let mutable captureOptions : Rules.CaptureOption list = []
    let mutable captureCardIdx = 0
    let mutable lastPlayMessage = ""
    let mutable roundNumber = 1
    let mutable cumulativeScores : Map<string, int> = Map.empty
    let mutable rng = Random()
    let mutable lastEval : AI.PlayEvaluation option = None

    // Layout & drag state
    let mutable tableLayout = RandomScatter
    let mutable dragState : DragState = NotDragging
    let mutable scatteredPositions : Map<Card, (float32 * float32 * float32)> = Map.empty  // card -> (x, y, rotation)

    // Animation state
    let mutable currentCardAnim : CardAnimation option = None
    let mutable currentCollectAnim : CollectAnimation option = None
    let mutable shuffleDuration = 0.6f
    let mutable cardSlideDuration = 0.25f
    let mutable collectSlideDuration = 0.35f
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

    let computerDelay = 0.8f
    let animDelay = 1.4f

    /// Reset menu to defaults
    let resetMenu () =
        menuStep <- VariantSelect
        menuVariant <- StandardKasino
        menuPlayerCount <- 2
        menuHumanCount <- 1

    /// Start a new game from current menu settings
    let startGame () =
        config <- { Variant = menuVariant; PlayerCount = menuPlayerCount
                    HumanCount = menuHumanCount; Seed = None; TargetScore = 16 }
        rng <- Random()
        let players = GameEngine.createPlayers config
        cumulativeScores <- players |> List.map (fun p -> p.Name, 0) |> Map.ofList
        roundNumber <- 1
        let state = GameEngine.newRound config rng players 1
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
            scoreBreakdowns <- Scoring.calculateScores finalGs.Players
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

    /// Card back image
    let backAsset : Image AssetTag =
        asset<Image> "Default" "back"

    /// Table felt background image
    let tableBgAsset : Image AssetTag =
        asset<Image> "Default" "table_bg"

// ─── Layout Constants ─────────────────────────────────────────────────
// Nu virtual resolution is 640×360 → visible range ±320 (X) × ±180 (Y)
module Ly =
    // Card dimensions in world units
    let cardW = 44.0f
    let cardH = 57.0f
    let cardGap = 5.0f
    let tableGap = 4.0f

    // Y positions (center origin, Y up) — fitted to 640×360 viewport
    let handY = -130.0f          // human hand at bottom (card bottom at -169)
    let tableY = 10.0f           // table center
    let topOppY = 135.0f         // top opponent (card top at 174)
    let sideLeftX = -285.0f      // left side opponent (card edge at -315)
    let sideRightX = 285.0f      // right side opponent (card edge at 315)

    // Table area dimensions (centered at 0, tableY)
    let tableW = 500.0f          // narrower to leave room for side hands
    let tableH = 130.0f          // compressed to fit viewport

    // Max entity slots
    let maxHand = 4              // max cards in hand (dealt 4 at a time)
    let maxTable = 26            // max table cards (theoretical max)
    let maxOppHand = 4           // max opponent cards shown

    // Menu/UI positions
    let titleY = 155.0f
    let subtitleY = 125.0f
    let menuBaseY = 55.0f
    let btnW = 240.0f
    let btnH = 28.0f
    let btnGap = 36.0f

    // Status bar Y
    let statusY = -95.0f
    let turnTextY = -110.0f

    // Scoreboard position (top right)
    let scoreX = 200.0f
    let scoreTopY = 165.0f

    /// Center N cards horizontally, returns X of leftmost card
    let centerCardsX (count: int) (gap: float32) =
        let totalW = float32 count * (cardW + gap) - gap
        -totalW / 2.0f

// ─── Colors ───────────────────────────────────────────────────────────
module Clr =
    let screenBg = color 0.098f 0.196f 0.137f 1.0f        // dark poker green (25,50,35)
    let tableBg = color 0.137f 0.392f 0.216f 1.0f          // poker-green felt (35,100,55)
    let gold = color 1.0f 0.843f 0.0f 1.0f
    let white = color 1.0f 1.0f 1.0f 1.0f
    let gray = color 0.627f 0.627f 0.627f 1.0f
    let lightGray = color 0.827f 0.827f 0.827f 1.0f
    let green = color 0.0f 0.549f 0.0f 1.0f
    let darkGreen = color 0.0f 0.275f 0.0f 0.353f          // capture definite overlay (unused)
    let darkYellow = color 0.275f 0.275f 0.0f 0.353f       // capture possible overlay (unused)
    let tintGreen = color 0.7f 1.0f 0.7f 1.0f              // definite capture card tint
    let tintYellow = color 1.0f 1.0f 0.65f 1.0f            // possible capture card tint
    let limeGreen = color 0.196f 0.804f 0.196f 1.0f
    let yellow = color 1.0f 1.0f 0.0f 1.0f
    let lightSalmon = color 1.0f 0.627f 0.478f 1.0f
    let lightBlue = color 0.678f 0.847f 0.902f 1.0f
    let plum = color 0.867f 0.627f 0.867f 1.0f
    let lightGreen = color 0.565f 0.933f 0.565f 1.0f
    let btnGreen = color 0.157f 0.392f 0.157f 1.0f
    let btnBlue = color 0.157f 0.314f 0.471f 1.0f
    let btnRed = color 0.471f 0.157f 0.157f 1.0f
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

    /// Build a collect animation from a play result.
    /// Captured cards slide from their table positions toward the player's area.
    let buildCollectAnimation (playResult: PlayResult) (isBottom: bool) (table: Card list) =
        match playResult with
        | Capture(_, captured, _) when not (List.isEmpty captured) ->
            let destY = if isBottom then Ly.handY - 40.0f else Ly.topOppY + 40.0f
            let cards =
                captured |> List.map (fun card ->
                    match Map.tryFind card AppState.scatteredPositions with
                    | Some(sx, sy, _) -> (card, sx, sy)
                    | None ->
                        // Fallback: grid position
                        let tableCols = min (List.length table) 10
                        let idx = table |> List.tryFindIndex ((=) card) |> Option.defaultValue 0
                        let col = idx % tableCols
                        let row = idx / tableCols
                        let tableRows = (List.length table + tableCols - 1) / tableCols
                        let x = Ly.centerCardsX tableCols Ly.tableGap + float32 col * (Ly.cardW + Ly.tableGap) + Ly.cardW / 2.0f
                        let y = Ly.tableY + float32 (tableRows - 1 - row * 2) * (Ly.cardH + Ly.tableGap) / 2.0f
                        (card, x, y))
            Some { CollectCards = cards; CollectToX = 0.0f; CollectToY = destY
                   CollectStart = AppState.cardSlideDuration; CollectDuration = AppState.collectSlideDuration }
        | _ -> None

    /// Build deal animation steps for Nu.
    /// First deal: 4 cards to table, then (2 per player) × 2 rounds.
    /// Subsequent deals: (2 per player) × 2 rounds.
    let buildDealSteps (gs: GameEngine.GameState) (isFirstDeal: bool) =
        let playerCount = gs.Players.Length
        let bottomIdx =
            if gs.Players |> List.exists (fun p -> p.Type = Human) then 0
            else gs.CurrentPlayerIndex

        let playerDest (idx: int) =
            if idx = bottomIdx then (0.0f, Ly.handY)
            else (0.0f, Ly.topOppY)

        let tableStep =
            { DealTargetLabel = "table"; DealCardCount = 4
              DealToX = 0.0f; DealToY = Ly.tableY; DealIsFaceUp = false }

        let playerSteps =
            [for _ in 1 .. 2 do
                for pIdx in 0 .. playerCount - 1 do
                    let (px, py) = playerDest pIdx
                    { DealTargetLabel = gs.Players[pIdx].Name; DealCardCount = 2
                      DealToX = px; DealToY = py; DealIsFaceUp = (pIdx = bottomIdx) }]

        if isFirstDeal then tableStep :: playerSteps
        else playerSteps

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
                AppState.selectedCardIndex <- None
            | _ ->
                // Build card animation: from hand position to scatter/center position
                let handSize = List.length player.Hand
                let startX = Ly.centerCardsX handSize Ly.cardGap
                let fromX = startX + float32 cardIndex * (Ly.cardW + Ly.cardGap) + Ly.cardW / 2.0f
                let fromY = Ly.handY
                let turnResult = GameEngine.playHumanTurn gs cardIndex None
                AppState.currentCollectAnim <- buildCollectAnimation turnResult.PlayResult true gs.Table
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
                AppState.selectedCardIndex <- None
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
            AppState.currentCollectAnim <- buildCollectAnimation turnResult.PlayResult true gs.Table
            // Captures always animate to table center (cards get collected away)
            AppState.currentCardAnim <- Some
                { AnimCard = card
                  FromX = fromX; FromY = fromY
                  ToX = 0.0f; ToY = Ly.tableY
                  Duration = AppState.cardSlideDuration }
            let msg = formatPlayResult player.Name turnResult.PlayResult
            AppState.gameState <- Some turnResult.NewState
            AppState.lastPlayMessage <- msg
            AppState.selectedCardIndex <- None
            AppState.lastEval <- Some turnResult.Evaluation
            AppState.phase <- AnimatingPlay
            AppState.phaseTimer <- 0.0f

// ─── Rules page content ───────────────────────────────────────────────
module RulesContent =
    let totalPages = 6

    let pageTitle = function
        | 0 -> "Game Overview"
        | 1 -> "Card Values"
        | 2 -> "Capturing Cards"
        | 3 -> "Sweeps & Round End"
        | 4 -> "Scoring"
        | 5 -> "Laistokasino"
        | _ -> ""

    let pageLines = function
        | 0 ->
            [| "Kasino is a classic Finnish card game for 2-4 players."
               "The goal is to capture cards from the table by"
               "matching values from your hand."
               ""
               "Each round, players are dealt cards in waves of 4."
               "On your turn you MUST play one card from your hand:"
               "  - If it can capture table cards, you take them."
               "  - If not, your card is placed on the table."
               ""
               "After all cards are played, scores are tallied."
               "First player to reach 16 cumulative points wins!" |]
        | 1 ->
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
               "Kings can only be captured by Kings (value 13)." |]
        | 2 ->
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
               "  Yellow = in some options only (choice needed)" |]
        | 3 ->
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
               "  2 players: 6 waves, 3: 4 waves, 4: 3 waves" |]
        | 4 ->
            [| "SCORING (per round):"
               ""
               "  Most cards captured .... 1 point"
               "  Most spades captured ... 2 points"
               "  Each Ace captured ...... 1 point (max 4)"
               "  10 of Diamonds ......... 2 points"
               "  2 of Spades ............ 1 point"
               "  Each Sweep ............. 1 point"
               ""
               "TIE: Nobody scores tied categories."
               "SWEEPS: Minimum sweep count subtracted from all."
               "TARGET: First to 16 cumulative points wins." |]
        | 5 ->
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
               "    cards on the table." |]
        | _ -> [||]

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

        // handle Alt+F4
        if  World.isKeyboardAltDown world &&
            World.isKeyboardKeyDown KeyboardKey.F4 world &&
            world.Unaccompanied then
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

        // "How to Play" button on all menu steps
        if World.doButton "BtnRules"
            [Entity.Position .= v3 0.0f -160.0f 0.0f
             Entity.Size .= v3 160.0f Ly.btnH 0.0f
             Entity.Text .= "How to Play"
             Entity.Elevation .= 1.0f] world then
            AppState.rulesPage <- 0
            AppState.rulesReturnMode <- KasinoMenu
            game.SetKasinoMode KasinoRules world

        // Escape to quit from menu
        if world.Advancing then
            if World.isKeyboardKeyPressed KeyboardKey.Escape world && world.Unaccompanied then
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
        let bottomIdx = if isHuman then 0 else gs.CurrentPlayerIndex
        let bottomPlayer = gs.Players[bottomIdx]
        let handSize = List.length bottomPlayer.Hand

        // ── Table background ──────────────────────────────────
        World.doStaticSprite "TableBg"
            [Entity.Position .= v3 0.0f 10.0f 0.0f
             Entity.Size .= v3 Ly.tableW Ly.tableH 0.0f
             Entity.StaticImage .= CardImg.tableBgAsset
             Entity.Color .= Clr.white
             Entity.Elevation .= 0.0f] world |> ignore

        // ── Draw table cards ──────────────────────────────────
        let tableCount = List.length gs.Table
        let definiteSet, possibleSet =
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

        // Grid layout computation
        let tableCols = min tableCount 10
        let tableRows = if tableCount > 0 then (tableCount + tableCols - 1) / tableCols else 0
        let tableStartX =
            if tableCols > 0 then Ly.centerCardsX tableCols Ly.tableGap
            else 0.0f

        // During AnimatingPlay, hide the card being animated from the table to prevent flicker
        let animatingCard =
            match AppState.currentCardAnim with
            | Some anim when AppState.phase = AnimatingPlay && AppState.phaseTimer < anim.Duration -> Some anim.AnimCard
            | _ -> None

        for i in 0 .. Ly.maxTable - 1 do
            let name = $"TC{i}"
            if i < tableCount then
                let card = gs.Table[i]
                // Hide the card being animated (Place action flicker fix)
                let isAnimating = animatingCard = Some card

                let cx, cy, rot =
                    match AppState.tableLayout with
                    | StrictGrid ->
                        let col = i % tableCols
                        let row = i / tableCols
                        let x = tableStartX + float32 col * (Ly.cardW + Ly.tableGap) + Ly.cardW / 2.0f
                        let y = Ly.tableY + float32 (tableRows - 1 - row * 2) * (Ly.cardH + Ly.tableGap) / 2.0f
                        (x, y, 0.0f)
                    | RandomScatter ->
                        match Map.tryFind card AppState.scatteredPositions with
                        | Some (sx, sy, sr) -> (sx, sy, sr)
                        | None ->
                            // Fallback to grid
                            let col = i % tableCols
                            let row = i / tableCols
                            let x = tableStartX + float32 col * (Ly.cardW + Ly.tableGap) + Ly.cardW / 2.0f
                            let y = Ly.tableY + float32 (tableRows - 1 - row * 2) * (Ly.cardH + Ly.tableGap) / 2.0f
                            (x, y, 0.0f)

                let rotation = Quaternion.CreateFromAxisAngle(Vector3.UnitZ, rot)

                // Apply capture preview tint directly on the card
                let cardTint =
                    if Set.contains card definiteSet then Clr.tintGreen
                    elif Set.contains card possibleSet then Clr.tintYellow
                    else Clr.white

                World.doStaticSprite name
                    [Entity.Position @= v3 cx cy 0.0f
                     Entity.Size @= v3 Ly.cardW Ly.cardH 0.0f
                     Entity.StaticImage @= CardImg.cardAsset card
                     Entity.Color @= cardTint
                     Entity.Rotation @= rotation
                     Entity.Visible @= (not isAnimating)
                     Entity.Elevation .= 1.0f] world |> ignore

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

        // ── Draw opponent hands ───────────────────────────────
        let opponents =
            gs.Players
            |> List.mapi (fun i p -> (i, p))
            |> List.filter (fun (i, _) -> i <> bottomIdx)

        // Top opponent
        match opponents with
        | (_, opp) :: _ ->
            let oppHandSize = List.length opp.Hand
            for i in 0 .. Ly.maxOppHand - 1 do
                let name = $"OT{i}"
                if i < oppHandSize then
                    let startX = Ly.centerCardsX oppHandSize Ly.cardGap
                    let x = startX + float32 i * (Ly.cardW + Ly.cardGap) + Ly.cardW / 2.0f
                    World.doStaticSprite name
                        [Entity.Position @= v3 x Ly.topOppY 0.0f
                         Entity.Size .= v3 Ly.cardW Ly.cardH 0.0f
                         Entity.StaticImage @= CardImg.backAsset
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
        | _ ->
            for i in 0 .. Ly.maxOppHand - 1 do
                World.doStaticSprite $"OT{i}"
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.0f] world |> ignore
            World.doText "OppTopLabel"
                [Entity.Visible @= false
                 Entity.Elevation .= 2.0f] world

        // Side opponents (3-4 player games)
        if opponents.Length >= 2 then
            let (_, opp2) = opponents[1]
            let opp2Hand = List.length opp2.Hand
            for i in 0 .. Ly.maxOppHand - 1 do
                let name = $"OL{i}"
                if i < opp2Hand then
                    let y = float32 (opp2Hand - 1 - i * 2) * (Ly.cardH * 0.3f) / 2.0f
                    World.doStaticSprite name
                        [Entity.Position @= v3 Ly.sideLeftX y 0.0f
                         Entity.Size .= v3 Ly.cardW Ly.cardH 0.0f
                         Entity.StaticImage @= CardImg.backAsset
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
        else
            for i in 0 .. Ly.maxOppHand - 1 do
                World.doStaticSprite $"OL{i}"
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.0f] world |> ignore
            World.doText "OppLeftLabel"
                [Entity.Visible @= false
                 Entity.Elevation .= 2.0f] world

        if opponents.Length >= 3 then
            let (_, opp3) = opponents[2]
            let opp3Hand = List.length opp3.Hand
            for i in 0 .. Ly.maxOppHand - 1 do
                let name = $"OR{i}"
                if i < opp3Hand then
                    let y = float32 (opp3Hand - 1 - i * 2) * (Ly.cardH * 0.3f) / 2.0f
                    World.doStaticSprite name
                        [Entity.Position @= v3 Ly.sideRightX y 0.0f
                         Entity.Size .= v3 Ly.cardW Ly.cardH 0.0f
                         Entity.StaticImage @= CardImg.backAsset
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
        else
            for i in 0 .. Ly.maxOppHand - 1 do
                World.doStaticSprite $"OR{i}"
                    [Entity.Visible @= false
                     Entity.Elevation .= 1.0f] world |> ignore
            World.doText "OppRightLabel"
                [Entity.Visible @= false
                 Entity.Elevation .= 2.0f] world

        // ── Draw human hand (bottom) ──────────────────────────
        let isDraggingIdx = match AppState.dragState with Dragging(idx, _, _) -> Some idx | _ -> None
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
                let img =
                    if isHuman || bottomIdx = gs.CurrentPlayerIndex then
                        CardImg.cardAsset card
                    else
                        CardImg.backAsset

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
                        CardImg.backAsset
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
                     Entity.StaticImage @= CardImg.backAsset
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
                         Entity.StaticImage @= CardImg.backAsset
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
                     Entity.StaticImage @= CardImg.backAsset
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

        let turnText =
            match AppState.phase with
            | WaitingForHuman when AppState.selectedCardIndex.IsSome -> ""
            | WaitingForHuman -> ""
            | ComputerThinking -> $"{gs.Players[gs.CurrentPlayerIndex].Name} thinking..."
            | ChoosingCaptureOption -> ""
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
             Entity.StaticImage .= CardImg.backAsset
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
        let notDragging = match AppState.dragState with NotDragging -> true | _ -> false
        let btnPlayVisible = AppState.phase = WaitingForHuman && isHuman && AppState.selectedCardIndex.IsSome && notDragging
        let btnPlayLabel =
            if btnPlayVisible then
                match AppState.capturePreview with
                | NoCapture -> "Place on Table"
                | SingleCapture cards -> $"Capture {cards.Length} Cards"
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

        World.doText "CaptureHeader"
            [Entity.Position .= v3 0.0f (60.0f + float32 (if isModal then AppState.captureOptions.Length else 0) * 18.0f) 0.0f
             Entity.Size .= v3 400.0f 24.0f 0.0f
             Entity.Text @= (if isModal then "Choose which cards to capture:" else "")
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.gold
             Entity.Visible @= isModal
             Entity.Elevation .= 7.0f] world

        for i in 0 .. 5 do
            let name = $"BtnOpt{i}"
            if isModal && i < AppState.captureOptions.Length then
                let opt = AppState.captureOptions[i]
                let capturedStr = opt.Captured |> List.map Cards.display |> String.concat " "
                let label = $"{i + 1}: {capturedStr} ({opt.Captured.Length})"
                let y = 40.0f - float32 i * 34.0f
                if World.doButton name
                    [Entity.Position .= v3 0.0f y 0.0f
                     Entity.Size .= v3 340.0f 28.0f 0.0f
                     Entity.Text @= label
                     Entity.Visible @= true
                     Entity.Elevation .= 7.0f] world then
                    Helpers.processCapture AppState.captureCardIdx opt
            else
                World.doButton name
                    [Entity.Visible @= false
                     Entity.Elevation .= 7.0f] world |> ignore

        let cancelY = 40.0f - float32 (if isModal then AppState.captureOptions.Length else 0) * 34.0f - 10.0f
        if World.doButton "BtnCancel"
            [Entity.Position .= v3 0.0f cancelY 0.0f
             Entity.Size .= v3 140.0f 28.0f 0.0f
             Entity.Text .= "Cancel"
             Entity.Visible @= isModal
             Entity.Elevation .= 7.0f] world then
            if isModal then
                AppState.phase <- WaitingForHuman
                AppState.selectedCardIndex <- None
                AppState.capturePreview <- NoCapture

        // ── Phase-specific input handling (advancing only) ────
        if world.Advancing then
            match AppState.phase with
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
                | Dragging _ ->
                    AppState.hoveredCardIndex <- None

                // Update capture preview for selected/dragged card
                let previewIdx =
                    match AppState.dragState with
                    | Dragging(idx, _, _) -> Some idx
                    | NotDragging -> AppState.selectedCardIndex
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
                            // Don't deselect if clicking on the Play button area
                            let btnCenterY = Ly.handY + 45.0f
                            let onPlayBtn =
                                btnPlayVisible &&
                                abs mx <= 90.0f &&
                                abs (my - btnCenterY) <= Ly.btnH / 2.0f
                            if not onPlayBtn then
                                AppState.selectedCardIndex <- None
                                AppState.capturePreview <- NoCapture

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
                    let turnResult = GameEngine.playComputerTurn gs
                    AppState.currentCollectAnim <- Helpers.buildCollectAnimation turnResult.PlayResult false gs.Table
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
                    if not (List.isEmpty player.Hand) then
                        let oppHandSize = List.length player.Hand
                        let fromX = Ly.centerCardsX oppHandSize Ly.cardGap + Ly.cardW / 2.0f
                        let fromY = Ly.topOppY
                        AppState.currentCardAnim <- Some
                            { AnimCard = player.Hand[0]
                              FromX = fromX; FromY = fromY
                              ToX = toX; ToY = toY
                              Duration = AppState.cardSlideDuration }
                    else
                        AppState.currentCardAnim <- None
                    let msg = Helpers.formatPlayResult player.Name turnResult.PlayResult
                    AppState.gameState <- Some turnResult.NewState
                    AppState.lastPlayMessage <- msg
                    AppState.lastEval <- Some turnResult.Evaluation
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
                // Number keys to choose capture option
                elif World.isKeyboardKeyPressed KeyboardKey.Num1 world && AppState.captureOptions.Length >= 1 then
                    Helpers.processCapture AppState.captureCardIdx AppState.captureOptions[0]
                elif World.isKeyboardKeyPressed KeyboardKey.Num2 world && AppState.captureOptions.Length >= 2 then
                    Helpers.processCapture AppState.captureCardIdx AppState.captureOptions[1]
                elif World.isKeyboardKeyPressed KeyboardKey.Num3 world && AppState.captureOptions.Length >= 3 then
                    Helpers.processCapture AppState.captureCardIdx AppState.captureOptions[2]
                elif World.isKeyboardKeyPressed KeyboardKey.Num4 world && AppState.captureOptions.Length >= 4 then
                    Helpers.processCapture AppState.captureCardIdx AppState.captureOptions[3]

            | RoundOver ->
                if World.isKeyboardKeyPressed KeyboardKey.Enter world then
                    AppState.enterConsumed <- true
                    AppState.enterScores ()
                    game.SetKasinoMode KasinoScores world

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

        // Winner announcement (game over only)
        if AppState.scoreIsGameOver then
            let winner =
                match AppState.config.Variant with
                | StandardKasino ->
                    AppState.cumulativeScores |> Map.toList |> List.maxBy snd
                | LaistoKasino ->
                    AppState.cumulativeScores |> Map.toList |> List.minBy snd
            World.doText "ScWinner"
                [Entity.Position .= v3 0.0f (baseY - float32 categories.Length * rowH - 16.0f) 0.0f
                 Entity.Size .= v3 450.0f 24.0f 0.0f
                 Entity.Text @= $"{(fst winner)} wins with {(snd winner)} points!"
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
             Entity.FontSizing .= Some 20.0f
             Entity.Elevation .= 1.0f] world

        let pageTitle = RulesContent.pageTitle AppState.rulesPage
        World.doText "RuPageTitle"
            [Entity.Position .= v3 0.0f 148.0f 0.0f
             Entity.Size .= v3 400.0f 20.0f 0.0f
             Entity.Text @= pageTitle
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Clr.white
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

        // Body text (max 14 lines)
        let lines = RulesContent.pageLines AppState.rulesPage
        let lineH = 16.0f
        let startY = 118.0f

        for i in 0 .. 13 do
            let name = $"RuLine{i}"
            if i < lines.Length then
                let lineColor =
                    if lines[i].StartsWith("  ") then Clr.lightGray
                    elif lines[i] = "" then color 0.0f 0.0f 0.0f 0.0f
                    else Clr.white
                World.doText name
                    [Entity.Position .= v3 0.0f (startY - float32 i * lineH) 0.0f
                     Entity.Size .= v3 520.0f 18.0f 0.0f
                     Entity.Text @= lines[i]
                     Entity.TextColor @= lineColor
                     Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                     Entity.FontSizing .= Some 13.0f
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
