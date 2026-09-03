namespace Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Pure F# subset-sum combination finder
// Replaces the quantum Knapsack solver with a self-contained
// recursive algorithm for finding all card capture options.
// ─────────────────────────────────────────────────────────────

module Combinations =

    /// Upper bound on enumerated subsets. A pathological table (15+ low
    /// cards) can produce thousands of exact-sum subsets, and the O(n²)
    /// conflict precompute in Rules.findCaptureOptions over those would
    /// freeze the turn. Real play needs only a handful; Bron-Kerbosch is
    /// separately capped at 64 options.
    [<Literal>]
    let private maxSubsets = 128

    /// Find subsets of items whose values sum exactly to target, up to
    /// maxSubsets of them. Each item is (index, value). Returns list of
    /// index-lists in include-first depth-first order.
    let findExactSubsets (items: (int * int) list) (target: int) : int list list =
        let results = ResizeArray<int list>()
        let rec search remaining target acc =
            if results.Count < maxSubsets then
                match remaining with
                | _ when target = 0 -> results.Add(List.rev acc)
                | [] -> ()
                | _ when target < 0 -> ()
                | (idx, value) :: rest ->
                    // Branch: include this item or skip it
                    search rest (target - value) (idx :: acc)
                    search rest target acc
        search items target []
        List.ofSeq results

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
