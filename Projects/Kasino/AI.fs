namespace Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Computer player AI for both Standard and Laisto Kasino.
// Standard: maximize captured points.
// Laisto:   minimize captured points (prefer not capturing).
// ─────────────────────────────────────────────────────────────

module AI =

    /// Snapshot of game context visible to the AI
    type GameContext =
        { MyCards: int
          MySpades: int
          OpponentCards: int
          OpponentSpades: int
          CardsRemaining: int }

    /// Evaluation of one possible play
    type PlayEvaluation =
        { HandCard: Card
          Result: PlayResult
          PointValue: float
          CardsCaptured: int
          IsSweep: bool
          CaptureOptions: Rules.CaptureOption list
          ChosenOption: Rules.CaptureOption option }

    // ── Evaluation helpers ──────────────────────────────────

    let private evaluateOption (handCard: Card) (option: Rules.CaptureOption) (tableCards: Card list) : PlayEvaluation =
        let result, _ = Rules.resolveCapture handCard option tableCards
        match result with
        | Capture(_, captured, isSweep) ->
            { HandCard       = handCard
              Result         = result
              PointValue     = Rules.capturePointValue captured + (if isSweep then Rules.sweepBonus else 0.0)
              CardsCaptured  = captured.Length
              IsSweep        = isSweep
              CaptureOptions = []
              ChosenOption   = Some option }
        | Place _ ->
            { HandCard = handCard; Result = result; PointValue = 0.0
              CardsCaptured = 0; IsSweep = false; CaptureOptions = []; ChosenOption = None }

    let private evaluateOptionInContext (ctx: GameContext) (handCard: Card) (option: Rules.CaptureOption) (tableCards: Card list) =
        let result, _ = Rules.resolveCapture handCard option tableCards
        match result with
        | Capture(_, captured, isSweep) ->
            let scoreFn =
                Cards.scoringValueInContext
                    ctx.MyCards ctx.MySpades
                    ctx.OpponentCards ctx.OpponentSpades
                    ctx.CardsRemaining
            { HandCard       = handCard
              Result         = result
              PointValue     = (captured |> List.sumBy scoreFn) + (if isSweep then Rules.sweepBonus else 0.0)
              CardsCaptured  = captured.Length
              IsSweep        = isSweep
              CaptureOptions = []
              ChosenOption   = Some option }
        | Place _ ->
            { HandCard = handCard; Result = result; PointValue = 0.0
              CardsCaptured = 0; IsSweep = false; CaptureOptions = []; ChosenOption = None }

    // ── Public API ──────────────────────────────────────────

    /// Evaluate playing a single hand card (static scoring).
    let evaluatePlay (handCard: Card) (tableCards: Card list) : PlayEvaluation =
        let options = Rules.findCaptureOptions handCard tableCards
        match options with
        | [] ->
            { HandCard = handCard; Result = Place handCard; PointValue = 0.0
              CardsCaptured = 0; IsSweep = false; CaptureOptions = []; ChosenOption = None }
        | [ single ] ->
            { evaluateOption handCard single tableCards with CaptureOptions = options }
        | _ ->
            let best =
                options
                |> List.map (fun opt -> evaluateOption handCard opt tableCards)
                |> List.sortByDescending (fun e -> (e.PointValue, float e.CardsCaptured, if e.IsSweep then 1.0 else 0.0))
                |> List.head
            { best with CaptureOptions = options }

    /// Evaluate a play with context-aware scoring.
    let evaluatePlayInContext (ctx: GameContext) (variant: GameVariant) (handCard: Card) (tableCards: Card list) =
        let options = Rules.findCaptureOptions handCard tableCards
        match options with
        | [] ->
            { HandCard = handCard; Result = Place handCard; PointValue = 0.0
              CardsCaptured = 0; IsSweep = false; CaptureOptions = []; ChosenOption = None }
        | [ single ] ->
            { evaluateOptionInContext ctx handCard single tableCards with CaptureOptions = options }
        | _ ->
            let evals = options |> List.map (fun opt -> evaluateOptionInContext ctx handCard opt tableCards)
            let best =
                match variant with
                | StandardKasino ->
                    evals |> List.maxBy (fun e -> (e.PointValue, float e.CardsCaptured, if e.IsSweep then 1.0 else 0.0))
                | LaistoKasino ->
                    evals |> List.minBy (fun e -> (e.PointValue, float e.CardsCaptured, if e.IsSweep then 1.0 else 0.0))
            { best with CaptureOptions = options }

    /// Evaluate all hand cards (static, for display).
    let evaluateAllPlays (hand: Card list) (tableCards: Card list) =
        hand |> List.map (fun c -> evaluatePlay c tableCards)

    /// Choose best card for Standard Kasino (maximize points).
    let chooseBestStandard (ctx: GameContext) (hand: Card list) (tableCards: Card list) =
        let evals = hand |> List.map (fun c -> evaluatePlayInContext ctx StandardKasino c tableCards)
        let captures = evals |> List.filter (fun e -> e.CardsCaptured > 0)
        if not (List.isEmpty captures) then
            captures
            |> List.maxBy (fun e -> (e.PointValue, float e.CardsCaptured, if e.IsSweep then 1.0 else 0.0))
        else
            let scoreFn = Cards.scoringValueInContext ctx.MyCards ctx.MySpades ctx.OpponentCards ctx.OpponentSpades ctx.CardsRemaining
            evals
            |> List.minBy (fun e -> (scoreFn e.HandCard, float (Cards.tableValue e.HandCard.Rank)))

    /// Choose best card for Laistokasino (minimize points).
    let chooseBestLaisto (ctx: GameContext) (hand: Card list) (tableCards: Card list) =
        let evals = hand |> List.map (fun c -> evaluatePlayInContext ctx LaistoKasino c tableCards)
        let nonCaptures = evals |> List.filter (fun e -> e.CardsCaptured = 0)
        if not (List.isEmpty nonCaptures) then
            let scoreFn = Cards.scoringValueInContext ctx.MyCards ctx.MySpades ctx.OpponentCards ctx.OpponentSpades ctx.CardsRemaining
            nonCaptures
            |> List.maxBy (fun e -> float (Cards.tableValue e.HandCard.Rank) - scoreFn e.HandCard * 10.0)
        else
            evals
            |> List.minBy (fun e -> (e.PointValue, float e.CardsCaptured, if e.IsSweep then 1.0 else 0.0))

    /// Choose the best play based on game variant.
    let chooseBest (variant: GameVariant) (ctx: GameContext) (hand: Card list) (tableCards: Card list) =
        match variant with
        | StandardKasino -> chooseBestStandard ctx hand tableCards
        | LaistoKasino   -> chooseBestLaisto ctx hand tableCards
