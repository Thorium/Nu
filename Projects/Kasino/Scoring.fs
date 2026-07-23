namespace Kasino.Domain

// ─────────────────────────────────────────────────────────────
// End-of-round scoring for Kasino.
//   Most cards:    1 point per round in the pot (ties: the point carries
//                  over — see CarryOver — until someone wins it outright)
//   Most spades:   2 points per round in the pot (ties carry over likewise)
//   Each Ace:      1 point
//   Diamond 10:    2 points
//   Spade 2:       1 point
//   Each sweep:    1 point (net of the table minimum — mutual cancellation)
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

    /// Undistributed most-cards / most-spades points. When a category ties,
    /// its round points join the pot and roll into the next round; whoever
    /// then wins the category outright collects the whole pot.
    type CarryOver =
        { CardsPool: int
          SpadesPool: int }

    module CarryOver =
        let zero = { CardsPool = 0; SpadesPool = 0 }

    /// Calculate scores for all players at end of a round, with the carried
    /// most-cards/most-spades pots from earlier tied rounds. Returns the
    /// breakdowns and the pot to carry into the next round.
    let calculateScoresCarry (carry: CarryOver) (players: Player list) : (Player * ScoreBreakdown) list * CarryOver =
        if List.isEmpty players then [], carry else

        let cardCounts  = players |> List.map (fun p -> p, List.length p.CapturedCards)
        let spadeCounts = players |> List.map (fun p -> p, p.CapturedCards |> List.filter Cards.isSpade |> List.length)

        let maxCards = cardCounts |> List.map snd |> List.max
        let uniqueMostCards = (cardCounts |> List.filter (fun (_, c) -> c = maxCards)).Length = 1

        let maxSpades = spadeCounts |> List.map snd |> List.max
        let uniqueMostSpades = (spadeCounts |> List.filter (fun (_, c) -> c = maxSpades)).Length = 1

        // This round's category points plus any pot from earlier tied rounds.
        let cardsPot  = carry.CardsPool + 1
        let spadesPot = carry.SpadesPool + 2

        // Sweep deduction: subtract the minimum sweep count from everyone
        // (players is non-empty here — the empty case returned above).
        let minSweeps = players |> List.map (fun p -> p.Sweeps) |> List.min

        let breakdowns =
            players
            |> List.map (fun player ->
                let myCards  = List.length player.CapturedCards
                let mySpades = player.CapturedCards |> List.filter Cards.isSpade |> List.length

                let mostCardsPoints  = if uniqueMostCards  && myCards  = maxCards  then cardsPot  else 0
                let mostSpadesPoints = if uniqueMostSpades && mySpades = maxSpades then spadesPot else 0
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

        let nextCarry =
            { CardsPool  = if uniqueMostCards  then 0 else cardsPot
              SpadesPool = if uniqueMostSpades then 0 else spadesPot }

        breakdowns, nextCarry

    /// Calculate scores for a single round with no carried pot (ties simply
    /// award nothing). Kept for callers without cross-round state.
    let calculateScores (players: Player list) : (Player * ScoreBreakdown) list =
        calculateScoresCarry CarryOver.zero players |> fst
