namespace Kasino.Domain

open System

// ─────────────────────────────────────────────────────────────
// Game engine: state management, dealing, turns, round flow.
// Pure functions operating on immutable state (no I/O).
// ─────────────────────────────────────────────────────────────

module GameEngine =

    /// Number of seats at the table. Kasino deals its 52 cards evenly only for
    /// 2–4 players, so the supported counts are encoded as a closed type — an
    /// illegal seat count is simply unrepresentable.
    [<Struct>]
    type SeatCount =
        | Two
        | Three
        | Four

    module SeatCount =
        /// The integer number of players.
        let count = function Two -> 2 | Three -> 3 | Four -> 4

        /// Parse a player count; None for anything outside the supported 2–4.
        let ofInt = function 2 -> Some Two | 3 -> Some Three | 4 -> Some Four | _ -> None

        /// Parse a player count, defaulting to a 2-player game for any value
        /// outside 2–4. Used at trusted boundaries (the menus already constrain
        /// the choice to 2–4); the default only guards a programming slip.
        let ofIntOrDefault n = ofInt n |> Option.defaultValue Two

        /// Deal rounds for the game. The first deal gives each player 4 cards and
        /// puts 4 on the table; the remaining 48 − 4·players cards are dealt 4 at
        /// a time. Exhaustive over the supported counts — no silent integer division.
        let dealRounds = function Two -> 6 | Three -> 4 | Four -> 3

    /// Game configuration
    type GameConfig =
        { Variant: GameVariant
          Seats: SeatCount
          HumanCount: int
          Seed: int option
          TargetScore: int
          Settings: Settings.GameSettings }

    /// One play of the round: who played which card and what it took
    /// (Captured is empty for a placement). Kept so a player can ask "what
    /// did the others just do?".
    type PlayRecord =
        { PlayerName: string
          PlayedCard: Card
          Captured: Card list
          IsSweep: bool }

    /// Full game state
    type GameState =
        { Players: Player list
          Table: Card list
          Deck: Card list
          CurrentPlayerIndex: int
          DealRound: int
          TotalDeals: int
          LastCapturer: int option
          /// The last (seat count - 1) plays of the round, oldest first: on a
          /// player's turn these are the other seats' moves since their own.
          RecentPlays: PlayRecord list
          Variant: GameVariant
          /// 10-point freeze: once any player's cumulative game score has
          /// reached 10, sweeps score nothing for the rest of the game.
          /// Set per round by the caller from the cumulative scores.
          SweepsFrozen: bool }

    /// Result of a single turn
    type TurnResult =
        { NewState: GameState
          PlayResult: PlayResult
          Evaluation: AI.PlayEvaluation }

    /// Create initial players.
    let createPlayers (config: GameConfig) =
        let playerCount = SeatCount.count config.Seats
        let cpuCount = playerCount - config.HumanCount
        [ for i in 0 .. playerCount - 1 do
            if i < config.HumanCount then
                let humanName = if config.HumanCount > 1 then $"Pelaaja {i + 1}" else "Pelaaja"
                { Name = humanName; Type = Human; Hand = []; CapturedCards = []; Sweeps = 0 }
            else
                let cpuIdx = i - config.HumanCount
                let cpuName =
                    if config.Settings.AiPersonalities then (Personality.forCpu cpuIdx).Name
                    elif cpuCount > 1 then $"Tietokone-{cpuIdx + 1}"
                    else "Tietokone"
                { Name = cpuName; Type = Computer; Hand = []; CapturedCards = []; Sweeps = 0 } ]

    /// Play style for the computer player at the given seat, honouring the
    /// AiPersonalities setting (plain CPUs always play Balanced).
    let computerStyle (config: GameConfig) (playerIdx: int) : AI.PlayStyle =
        if config.Settings.AiPersonalities && playerIdx >= config.HumanCount then
            (Personality.forCpu (playerIdx - config.HumanCount)).Style
        else
            AI.Balanced

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

    /// Apply a resolved play (the played card + its Capture/Place result) to the
    /// state: update the acting player's hand and captures, advance the turn, and
    /// carry the last-capturer marker. Shared by the computer and human paths.
    let private applyPlay
        (state: GameState) (idx: int) (playedCard: Card)
        (remainingHand: Card list) (result: PlayResult) (newTable: Card list)
        (eval: AI.PlayEvaluation) : TurnResult =
        // 10-point freeze: with sweeps disabled, strip the sweep flag so the
        // pile count, the UI's "SWEEP!" banner, and the eval all agree.
        let result =
            match result with
            | Capture(hc, captured, true) when state.SweepsFrozen -> Capture(hc, captured, false)
            | r -> r
        let eval = if state.SweepsFrozen && eval.IsSweep then { eval with AI.IsSweep = false } else eval
        let player = state.Players[idx]
        let updatedPlayer =
            match result with
            | Capture(_, captured, isSweep) ->
                { player with
                    Hand = remainingHand
                    CapturedCards = player.CapturedCards @ (playedCard :: captured)
                    Sweeps = player.Sweeps + (if isSweep then 1 else 0) }
            | Place _ ->
                { player with Hand = remainingHand }
        let updatedPlayers =
            state.Players |> List.mapi (fun i p -> if i = idx then updatedPlayer else p)
        let lastCapturer =
            match result with
            | Capture _ -> Some idx
            | Place _   -> state.LastCapturer
        let record =
            match result with
            | Capture(_, captured, isSweep) -> { PlayerName = player.Name; PlayedCard = playedCard; Captured = captured; IsSweep = isSweep }
            | Place _ -> { PlayerName = player.Name; PlayedCard = playedCard; Captured = []; IsSweep = false }
        let recentPlays =
            let keep = max 1 (state.Players.Length - 1)
            let all = state.RecentPlays @ [ record ]
            List.skip (max 0 (all.Length - keep)) all
        { NewState =
            { state with
                Players = updatedPlayers
                Table = newTable
                CurrentPlayerIndex = (idx + 1) % state.Players.Length
                LastCapturer = lastCapturer
                RecentPlays = recentPlays }
          PlayResult = result
          Evaluation = eval }

    /// Execute a computer player's turn using the given personality style.
    let playComputerTurnStyled (style: AI.PlayStyle) (state: GameState) : TurnResult =
        let idx = state.CurrentPlayerIndex
        let player = state.Players[idx]
        let ctx = buildContext state idx
        let eval = AI.chooseBestStyled style state.Variant ctx player.Hand state.Table
        let cardIdx = player.Hand |> List.findIndex (fun c -> c = eval.HandCard)
        let remainingHand = player.Hand |> List.removeAt cardIdx

        let result, newTable =
            match eval.ChosenOption with
            | Some opt -> Rules.resolveCapture eval.HandCard opt state.Table
            | None ->
                let r, t, _ = Rules.playCard eval.HandCard state.Table
                (r, t)

        applyPlay state idx eval.HandCard remainingHand result newTable eval

    /// Execute a computer player's turn with balanced (optimal) play.
    let playComputerTurn (state: GameState) : TurnResult =
        playComputerTurnStyled AI.Balanced state

    /// Execute a human player's chosen card. Returns TurnResult.
    let playHumanTurn (state: GameState) (cardIndex: int) (chosenOption: Rules.CaptureOption option) : TurnResult =
        let idx = state.CurrentPlayerIndex
        let player = state.Players[idx]
        let chosenCard = player.Hand[cardIndex]
        let remainingHand = player.Hand |> List.removeAt cardIndex

        let result, newTable, options =
            match chosenOption with
            | Some opt ->
                let r, t = Rules.resolveCapture chosenCard opt state.Table
                (r, t, [])
            | None -> Rules.playCard chosenCard state.Table

        // Describe the play that was actually applied — not the AI's
        // preferred alternative, which can be a different option when
        // several capture options overlap.
        let eval =
            match result with
            | Capture(_, captured, isSweep) ->
                { AI.HandCard = chosenCard; AI.Result = result
                  AI.PointValue = Rules.capturePointValue (chosenCard :: captured) + (if isSweep then Rules.sweepBonus else 0.0)
                  AI.CardsCaptured = captured.Length; AI.IsSweep = isSweep
                  AI.CaptureOptions = options; AI.ChosenOption = chosenOption }
            | Place _ ->
                { AI.HandCard = chosenCard; AI.Result = result; AI.PointValue = 0.0
                  AI.CardsCaptured = 0; AI.IsSweep = false
                  AI.CaptureOptions = options; AI.ChosenOption = None }

        applyPlay state idx chosenCard remainingHand result newTable eval

    /// Place the chosen card on the table without capturing. In Standard
    /// Kasino capturing is optional, so this is a legal alternative even
    /// when the card could capture. In Laisto capture is mandatory: a
    /// capture-capable card falls back to the normal (capturing) play.
    let playHumanPlaceTurn (state: GameState) (cardIndex: int) : TurnResult =
        let idx = state.CurrentPlayerIndex
        let player = state.Players[idx]
        let chosenCard = player.Hand[cardIndex]
        if state.Variant = LaistoKasino && Rules.canCapture chosenCard state.Table then
            playHumanTurn state cardIndex None
        else
            let remainingHand = player.Hand |> List.removeAt cardIndex
            let result = Place chosenCard
            let newTable = chosenCard :: state.Table
            let eval =
                { AI.HandCard = chosenCard; AI.Result = result; AI.PointValue = 0.0
                  AI.CardsCaptured = 0; AI.IsSweep = false
                  AI.CaptureOptions = []; AI.ChosenOption = None }
            applyPlay state idx chosenCard remainingHand result newTable eval

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

    /// One line per recent play, oldest first, for the "Last moves" panel.
    let describeRecentPlays (state: GameState) : string list =
        match state.RecentPlays with
        | [] -> [ "No moves yet this round" ]
        | plays ->
            plays
            |> List.map (fun p ->
                let card = Cards.display p.PlayedCard
                if List.isEmpty p.Captured then $"{p.PlayerName} placed {card}"
                else
                    let taken = p.Captured |> List.map Cards.display |> String.concat " "
                    let sweep = if p.IsSweep then " (sweep)" else ""
                    $"{p.PlayerName} played {card}, taking {taken}{sweep}")

    /// Create a fresh round state from config.
    let newRound (config: GameConfig) (rng: Random) (players: Player list) (roundNumber: int) =
        let deck = Cards.createDeck () |> Cards.shuffle rng
        let freshPlayers =
            players |> List.map (fun p -> { p with Hand = []; CapturedCards = []; Sweeps = 0 })
        let startIdx = (roundNumber - 1) % freshPlayers.Length
        { Players = freshPlayers
          Table = []
          Deck = deck
          SweepsFrozen = false
          CurrentPlayerIndex = startIdx
          DealRound = 0
          TotalDeals = SeatCount.dealRounds config.Seats
          LastCapturer = None
          RecentPlays = []
          Variant = config.Variant }
