namespace Kasino.Domain

open System

// ─────────────────────────────────────────────────────────────
// Game engine: state management, dealing, turns, round flow.
// Pure functions operating on immutable state (no I/O).
// ─────────────────────────────────────────────────────────────

module GameEngine =

    /// Game configuration
    type GameConfig =
        { Variant: GameVariant
          PlayerCount: int
          HumanCount: int
          Seed: int option
          TargetScore: int }

    /// Full game state
    type GameState =
        { Players: Player list
          Table: Card list
          Deck: Card list
          CurrentPlayerIndex: int
          DealRound: int
          TotalDeals: int
          LastCapturer: int option
          Variant: GameVariant }

    /// Result of a single turn
    type TurnResult =
        { NewState: GameState
          PlayResult: PlayResult
          Evaluation: AI.PlayEvaluation }

    /// Total deal rounds based on player count.
    /// 52 cards: first deal = 4*players + 4 table, rest = 4*players each.
    let totalDealRounds (playerCount: int) = 12 / playerCount

    /// Create initial players.
    let createPlayers (config: GameConfig) =
        [ for i in 0 .. config.PlayerCount - 1 do
            if i < config.HumanCount then
                { Name = "Pelaaja"; Type = Human; Hand = []; CapturedCards = []; Sweeps = 0 }
            else
                let cpuIdx = i - config.HumanCount
                let cpuName = $"Tietokone-{cpuIdx + 1}"
                { Name = cpuName; Type = Computer; Hand = []; CapturedCards = []; Sweeps = 0 } ]

    /// Deal cards to all players and optionally to the table (first deal).
    let dealRound (state: GameState) (isFirstDeal: bool) =
        let mutable deck = state.Deck
        let players = Array.ofList state.Players

        // Deal 4 cards to each player (2 at a time, twice)
        for _ in 1 .. 2 do
            for i in 0 .. players.Length - 1 do
                let dealt, remaining = Cards.deal 2 deck
                deck <- remaining
                players[i] <- { players[i] with Hand = players[i].Hand @ dealt }

        // Deal 4 to table on first deal only
        let table =
            if isFirstDeal then
                let tableCards, remaining = Cards.deal 4 deck
                deck <- remaining
                state.Table @ tableCards
            else
                state.Table

        { state with
            Players = Array.toList players
            Table = table
            Deck = deck }

    /// Build AI context for a given player.
    let buildContext (state: GameState) (playerIdx: int) : AI.GameContext =
        let player = state.Players[playerIdx]
        let myCards = List.length player.CapturedCards
        let mySpades = player.CapturedCards |> List.filter Cards.isSpade |> List.length
        let opponents =
            state.Players
            |> List.mapi (fun i p -> (i, p))
            |> List.filter (fun (i, _) -> i <> playerIdx)
        let opponentCards =
            match opponents with
            | [] -> 0
            | _  -> opponents |> List.map (fun (_, p) -> List.length p.CapturedCards) |> List.max
        let opponentSpades =
            match opponents with
            | [] -> 0
            | _  ->
                opponents
                |> List.map (fun (_, p) -> p.CapturedCards |> List.filter Cards.isSpade |> List.length)
                |> List.max
        let cardsRemaining =
            List.length state.Deck
            + (state.Players |> List.sumBy (fun p -> List.length p.Hand))
        { MyCards = myCards; MySpades = mySpades
          OpponentCards = opponentCards; OpponentSpades = opponentSpades
          CardsRemaining = cardsRemaining }

    /// Execute a computer player's turn. Returns TurnResult.
    let playComputerTurn (state: GameState) : TurnResult =
        let idx = state.CurrentPlayerIndex
        let player = state.Players[idx]
        let ctx = buildContext state idx
        let eval = AI.chooseBest state.Variant ctx player.Hand state.Table
        let cardIdx = player.Hand |> List.findIndex (fun c -> c = eval.HandCard)
        let remainingHand = player.Hand |> List.removeAt cardIdx

        let result, newTable =
            match eval.ChosenOption with
            | Some opt -> Rules.resolveCapture eval.HandCard opt state.Table
            | None ->
                let r, t, _ = Rules.playCard eval.HandCard state.Table
                (r, t)

        let updatedPlayer =
            match result with
            | Capture(_, captured, isSweep) ->
                { player with
                    Hand = remainingHand
                    CapturedCards = player.CapturedCards @ [ eval.HandCard ] @ captured
                    Sweeps = player.Sweeps + (if isSweep then 1 else 0) }
            | Place _ ->
                { player with Hand = remainingHand }

        let updatedPlayers =
            state.Players |> List.mapi (fun i p -> if i = idx then updatedPlayer else p)

        let lastCapturer =
            match result with
            | Capture _ -> Some idx
            | Place _   -> state.LastCapturer

        { NewState =
            { state with
                Players = updatedPlayers
                Table = newTable
                CurrentPlayerIndex = (idx + 1) % state.Players.Length
                LastCapturer = lastCapturer }
          PlayResult = result
          Evaluation = eval }

    /// Execute a human player's chosen card. Returns TurnResult.
    let playHumanTurn (state: GameState) (cardIndex: int) (chosenOption: Rules.CaptureOption option) : TurnResult =
        let idx = state.CurrentPlayerIndex
        let player = state.Players[idx]
        let chosenCard = player.Hand[cardIndex]
        let remainingHand = player.Hand |> List.removeAt cardIndex

        let result, newTable =
            match chosenOption with
            | Some opt -> Rules.resolveCapture chosenCard opt state.Table
            | None ->
                let r, t, _ = Rules.playCard chosenCard state.Table
                (r, t)

        let eval =
            match chosenOption with
            | Some opt ->
                let pts =
                    match result with
                    | Capture(_, captured, isSweep) ->
                        Rules.capturePointValue captured + (if isSweep then Rules.sweepBonus else 0.0)
                    | Place _ -> 0.0
                let cc = match result with Capture(_, c, _) -> c.Length | _ -> 0
                let sw = match result with Capture(_, _, s) -> s | _ -> false
                { AI.HandCard = chosenCard; AI.Result = result; AI.PointValue = pts
                  AI.CardsCaptured = cc; AI.IsSweep = sw
                  AI.CaptureOptions = []; AI.ChosenOption = Some opt }
            | None ->
                AI.evaluatePlay chosenCard state.Table

        let updatedPlayer =
            match result with
            | Capture(_, captured, isSweep) ->
                { player with
                    Hand = remainingHand
                    CapturedCards = player.CapturedCards @ [ chosenCard ] @ captured
                    Sweeps = player.Sweeps + (if isSweep then 1 else 0) }
            | Place _ ->
                { player with Hand = remainingHand }

        let updatedPlayers =
            state.Players |> List.mapi (fun i p -> if i = idx then updatedPlayer else p)

        let lastCapturer =
            match result with
            | Capture _ -> Some idx
            | Place _   -> state.LastCapturer

        { NewState =
            { state with
                Players = updatedPlayers
                Table = newTable
                CurrentPlayerIndex = (idx + 1) % state.Players.Length
                LastCapturer = lastCapturer }
          PlayResult = result
          Evaluation = eval }

    /// Check if all players' hands are empty.
    let allHandsEmpty (state: GameState) =
        state.Players |> List.forall (fun p -> List.isEmpty p.Hand)

    /// End of round: last capturer takes remaining table cards.
    let endRound (state: GameState) =
        match state.LastCapturer with
        | Some idx when not (List.isEmpty state.Table) ->
            let p = state.Players[idx]
            let updated = { p with CapturedCards = p.CapturedCards @ state.Table }
            { state with
                Players = state.Players |> List.mapi (fun i pl -> if i = idx then updated else pl)
                Table = [] }
        | _ -> state

    /// Create a fresh round state from config.
    let newRound (config: GameConfig) (rng: Random) (players: Player list) (roundNumber: int) =
        let deck = Cards.createDeck () |> Cards.shuffle rng
        let freshPlayers =
            players |> List.map (fun p -> { p with Hand = []; CapturedCards = []; Sweeps = 0 })
        let startIdx = (roundNumber - 1) % freshPlayers.Length
        { Players = freshPlayers
          Table = []
          Deck = deck
          CurrentPlayerIndex = startIdx
          DealRound = 0
          TotalDeals = totalDealRounds config.PlayerCount
          LastCapturer = None
          Variant = config.Variant }
