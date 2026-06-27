namespace Kasino.Domain

// ─────────────────────────────────────────────────────────────
// End-of-round scoring for Kasino.
//   Most cards:    1 point  (ties: nobody gets it)
//   Most spades:   2 points (ties: nobody gets it)
//   Each Ace:      1 point
//   Diamond 10:    2 points
//   Spade 2:       1 point
//   Each sweep:    1 point
// ─────────────────────────────────────────────────────────────

module Scoring =

    /// Score breakdown for one player
    type ScoreBreakdown =
        { MostCards: int
          MostSpades: int
          Aces: int
          DiamondTen: int
          SpadeTwo: int
          Sweeps: int
          Total: int }

    /// Calculate scores for all players at end of a round.
    let calculateScores (players: Player list) : (Player * ScoreBreakdown) list =
        if List.isEmpty players then [] else

        let cardCounts  = players |> List.map (fun p -> p, List.length p.CapturedCards)
        let spadeCounts = players |> List.map (fun p -> p, p.CapturedCards |> List.filter Cards.isSpade |> List.length)

        let maxCards = cardCounts |> List.map snd |> List.max
        let uniqueMostCards = (cardCounts |> List.filter (fun (_, c) -> c = maxCards)).Length = 1

        let maxSpades = spadeCounts |> List.map snd |> List.max
        let uniqueMostSpades = (spadeCounts |> List.filter (fun (_, c) -> c = maxSpades)).Length = 1

        // Sweep deduction: subtract the minimum sweep count from everyone
        // (players is non-empty here — the empty case returned [] above).
        let minSweeps = players |> List.map (fun p -> p.Sweeps) |> List.min

        players
        |> List.map (fun player ->
            let myCards  = List.length player.CapturedCards
            let mySpades = player.CapturedCards |> List.filter Cards.isSpade |> List.length

            let mostCardsPoints  = if uniqueMostCards  && myCards  = maxCards  then 1 else 0
            let mostSpadesPoints = if uniqueMostSpades && mySpades = maxSpades then 2 else 0
            let acePoints        = player.CapturedCards |> List.filter Cards.isAce |> List.length
            let diamondTenPts    = if player.CapturedCards |> List.exists Cards.isDiamondTen then 2 else 0
            let spadeTwoPts      = if player.CapturedCards |> List.exists Cards.isSpadeTwo then 1 else 0
            let sweepPts         = player.Sweeps - minSweeps

            let total = mostCardsPoints + mostSpadesPoints + acePoints + diamondTenPts + spadeTwoPts + sweepPts

            (player,
             { MostCards  = mostCardsPoints
               MostSpades = mostSpadesPoints
               Aces       = acePoints
               DiamondTen = diamondTenPts
               SpadeTwo   = spadeTwoPts
               Sweeps     = sweepPts
               Total      = total }))
