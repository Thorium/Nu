namespace Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Pure F# subset-sum combination finder
// Replaces the quantum Knapsack solver with a self-contained
// recursive algorithm for finding all card capture options.
// ─────────────────────────────────────────────────────────────

module Combinations =

    /// Find all subsets of items whose values sum exactly to target.
    /// Each item is (index, value). Returns list of index-lists.
    let findExactSubsets (items: (int * int) list) (target: int) : int list list =
        let rec search remaining target acc =
            match remaining with
            | _ when target = 0 -> [ List.rev acc ]
            | [] -> []
            | _ when target < 0 -> []
            | (idx, value) :: rest ->
                // Branch: include this item or skip it
                let withItem = search rest (target - value) (idx :: acc)
                let without  = search rest target acc
                withItem @ without
        search items target []

    /// Find all subsets of cards on the table that sum exactly to
    /// the hand card's capture value. Returns list of card-lists.
    let findCaptureCombinations (handCard: Card) (tableCards: Card list) : Card list list =
        if List.isEmpty tableCards then []
        else
            let target = Cards.handValue handCard
            let indexed =
                tableCards
                |> List.mapi (fun i c -> (i, Cards.tableValue c.Rank))
            let subsets = findExactSubsets indexed target
            subsets
            |> List.map (fun indices ->
                indices |> List.map (fun i -> tableCards[i]))
            |> List.filter (List.isEmpty >> not)
