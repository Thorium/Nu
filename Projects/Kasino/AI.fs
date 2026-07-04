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

    /// Personality play style. Balanced reproduces the default optimal play;
    /// the others only re-rank how captures are chosen (see chooseBestStyled).
    type PlayStyle =
        | Balanced
        | Aggressive   // grabs as many cards/spades as possible
        | Cautious     // secures direct points, holds cards back

    // ── Evaluation helpers ──────────────────────────────────

    let private evaluateOption (handCard: Card) (option: Rules.CaptureOption) (tableCards: Card list) : PlayEvaluation =
        let result, _ = Rules.resolveCapture handCard option tableCards
        match result with
        | Capture(_, captured, isSweep) ->
            // The played card is banked together with the capture
            // (see GameEngine.applyPlay), so its points count too.
            { HandCard       = handCard
              Result         = result
              PointValue     = Rules.capturePointValue (handCard :: captured) + (if isSweep then Rules.sweepBonus else 0.0)
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
            // Direct points sum per card; the most-cards/most-spades race
            // bonuses are counted once for the whole capture (not per card).
            // The played card lands in the pile with the capture, so it
            // counts toward both the direct points and the spade race.
            let banked = handCard :: captured
            let directPts = banked |> List.sumBy Cards.directValue
            let capturedSpades = banked |> List.filter Cards.isSpade |> List.length
            let raceBonus =
                Cards.captureRaceValue
                    ctx.MyCards ctx.MySpades
                    ctx.OpponentCards ctx.OpponentSpades
                    ctx.CardsRemaining
                    captured.Length capturedSpades
            { HandCard       = handCard
              Result         = result
              PointValue     = directPts + raceBonus + (if isSweep then Rules.sweepBonus else 0.0)
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
                |> List.maxBy (fun e -> (e.PointValue, float e.CardsCaptured, if e.IsSweep then 1.0 else 0.0))
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
            // Shed the most dangerous card first: a hoarded point card
            // (ace, 2♠, 10♦, spades) tends to find a forced capture late in
            // the deal and lands in our own pile — the worst Laisto outcome.
            // Discarding it instead pushes the risk onto whoever captures it.
            // Among equally harmless cards, shed the biggest one.
            nonCaptures
            |> List.maxBy (fun e -> (scoreFn e.HandCard, float (Cards.tableValue e.HandCard.Rank)))
        else
            evals
            |> List.minBy (fun e -> (e.PointValue, float e.CardsCaptured, if e.IsSweep then 1.0 else 0.0))

    /// Choose the best play based on game variant.
    let chooseBest (variant: GameVariant) (ctx: GameContext) (hand: Card list) (tableCards: Card list) =
        match variant with
        | StandardKasino -> chooseBestStandard ctx hand tableCards
        | LaistoKasino   -> chooseBestLaisto ctx hand tableCards

    /// Standard-variant chooser with a personality bias on how captures are
    /// ranked. The no-capture fallback (shed the least valuable card) is shared
    /// with Balanced so a styled AI never plays an outright bad lead.
    let private chooseBestStandardStyled (style: PlayStyle) (ctx: GameContext) (hand: Card list) (tableCards: Card list) =
        let evals = hand |> List.map (fun c -> evaluatePlayInContext ctx StandardKasino c tableCards)
        let captures = evals |> List.filter (fun e -> e.CardsCaptured > 0)
        if not (List.isEmpty captures) then
            match style with
            | Balanced ->
                captures |> List.maxBy (fun e -> (e.PointValue, float e.CardsCaptured, (if e.IsSweep then 1.0 else 0.0)))
            | Aggressive ->
                // Prefer hauling in the most cards (and sweeps) over raw points.
                captures |> List.maxBy (fun e -> (float e.CardsCaptured, (if e.IsSweep then 1.0 else 0.0), e.PointValue))
            | Cautious ->
                // Prefer the highest-scoring capture but take fewer cards when tied.
                captures |> List.maxBy (fun e -> (e.PointValue, (if e.IsSweep then 1.0 else 0.0), -(float e.CardsCaptured)))
        else
            let scoreFn = Cards.scoringValueInContext ctx.MyCards ctx.MySpades ctx.OpponentCards ctx.OpponentSpades ctx.CardsRemaining
            evals |> List.minBy (fun e -> (scoreFn e.HandCard, float (Cards.tableValue e.HandCard.Rank)))

    /// Choose the best play for a given personality style. Balanced reproduces
    /// chooseBest exactly; other styles only affect Standard Kasino capture
    /// preference (Laisto keeps the balanced minimizing strategy, which is hard
    /// to vary without weakening play).
    let chooseBestStyled (style: PlayStyle) (variant: GameVariant) (ctx: GameContext) (hand: Card list) (tableCards: Card list) =
        match variant, style with
        | StandardKasino, Balanced -> chooseBestStandard ctx hand tableCards
        | StandardKasino, _        -> chooseBestStandardStyled style ctx hand tableCards
        | LaistoKasino, _          -> chooseBestLaisto ctx hand tableCards
