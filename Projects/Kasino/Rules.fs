namespace Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Rules engine for the Finnish Kasino card game.
// Handles capture logic including overlapping combo resolution
// via Bron-Kerbosch maximal independent set enumeration.
// ─────────────────────────────────────────────────────────────

module Rules =

    /// A capture option: one valid way to capture cards.
    type CaptureOption =
        { Combos: Card list list   // non-overlapping combo groups
          Captured: Card list }     // union of all combos

    /// Find all subsets of table cards that sum to the hand card's value.
    let findCaptures (handCard: Card) (tableCards: Card list) : Card list list =
        Combinations.findCaptureCombinations handCard tableCards

    /// Check whether two combos share any card.
    let private combosOverlap (a: Card list) (b: Card list) =
        a |> List.exists (fun ca -> b |> List.exists (fun cb -> ca = cb))

    /// Find all maximal non-overlapping selections of combos.
    /// In Kasino, you MUST capture all non-overlapping subsets simultaneously.
    /// When subsets overlap, you choose which to take.
    /// Uses Bron-Kerbosch on the conflict graph.
    let findCaptureOptions (handCard: Card) (tableCards: Card list) : CaptureOption list =
        let allCombos = findCaptures handCard tableCards
        if List.isEmpty allCombos then []
        else
            let arr = Array.ofList allCombos
            let n = arr.Length

            // Precompute conflict sets
            let conflicts =
                Array.init n (fun i ->
                    [ for j in 0 .. n - 1 do
                        if i <> j && combosOverlap arr[i] arr[j] then j ]
                    |> Set.ofList)

            let results = System.Collections.Generic.List<int list>()

            // For pathological tables (many low cards) the number of maximal
            // independent sets can explode into the tens of thousands. Such a
            // modal would be both unusably long and slow to enumerate, so we
            // stop once a generous cap is reached; real play needs only a few.
            let maxOptions = 64

            // Bron-Kerbosch: maximal independent sets in the conflict graph
            let rec bronKerbosch (r: Set<int>) (p: Set<int>) (x: Set<int>) =
                if results.Count >= maxOptions then ()
                elif Set.isEmpty p && Set.isEmpty x then
                    results.Add(Set.toList r)
                else
                    let mutable pMut = p
                    let mutable xMut = x
                    for v in Set.toList p do
                        if results.Count < maxOptions then
                            let nbrs = conflicts[v]
                            let newP = pMut |> Set.remove v |> Set.filter (fun u -> not (Set.contains u nbrs))
                            let newX = xMut |> Set.filter (fun u -> not (Set.contains u nbrs))
                            bronKerbosch (Set.add v r) newP newX
                            pMut <- Set.remove v pMut
                            xMut <- Set.add v xMut

            bronKerbosch Set.empty (set [ 0 .. n - 1 ]) Set.empty

            results
            |> Seq.map (fun sel ->
                let sorted = List.sort sel
                let combos = sorted |> List.map (fun i -> arr[i])
                let union = combos |> List.concat |> List.distinct
                (sorted, { Combos = combos; Captured = union }))
            |> Seq.distinctBy fst
            |> Seq.map snd
            |> Seq.toList

    /// Get captured cards — single option: union; multiple: largest capture.
    let getCapturedCards (handCard: Card) (tableCards: Card list) : Card list =
        match findCaptureOptions handCard tableCards with
        | [] -> []
        | [ single ] -> single.Captured
        | multiple ->
            multiple
            |> List.maxBy (fun opt -> opt.Captured.Length)
            |> fun opt -> opt.Captured

    /// Check if playing a card results in a capture.
    let canCapture (handCard: Card) (tableCards: Card list) =
        findCaptures handCard tableCards |> List.isEmpty |> not

    /// Determine the result of playing a hand card on the table.
    let playCard (handCard: Card) (tableCards: Card list) : PlayResult * Card list * CaptureOption list =
        let options = findCaptureOptions handCard tableCards
        match options with
        | [] ->
            (Place handCard, handCard :: tableCards, [])
        | [ single ] ->
            let newTable = tableCards |> List.filter (fun c -> not (List.contains c single.Captured))
            let isSweep = List.isEmpty newTable
            (Capture(handCard, single.Captured, isSweep), newTable, options)
        | _ ->
            // Multiple overlapping options: default to the largest capture,
            // matching getCapturedCards. (UIs intercept this case and let the
            // player choose; this keeps any direct caller consistent.)
            let chosen = options |> List.maxBy (fun opt -> opt.Captured.Length)
            let newTable = tableCards |> List.filter (fun c -> not (List.contains c chosen.Captured))
            let isSweep = List.isEmpty newTable
            (Capture(handCard, chosen.Captured, isSweep), newTable, options)

    /// Resolve a specific capture option chosen by the player.
    let resolveCapture (handCard: Card) (option: CaptureOption) (tableCards: Card list) : PlayResult * Card list =
        let newTable = tableCards |> List.filter (fun c -> not (List.contains c option.Captured))
        let isSweep = List.isEmpty newTable
        (Capture(handCard, option.Captured, isSweep), newTable)

    /// Point value of a set of captured cards.
    let capturePointValue (captured: Card list) : float =
        captured |> List.sumBy Cards.scoringValue

    /// Sweep bonus points.
    let sweepBonus = 1.0
