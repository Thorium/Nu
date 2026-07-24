/// KASINO — MMCC port — the Rules / Help (tutorial) screen.
///
/// Ported from the original /mnt/c/git/Kasino desktop UI (RulesScreen.fs, the
/// "better tutorial" revision). The original is already a model+update+draw
/// screen: `RulesState = { CurrentPage; BackClicked }`, an `update` that maps
/// input to a new page, and a `draw` that renders text pages or VISUAL pages
/// built from real card images. That shape maps straight onto MMCC:
///   • RulesState        → the `Tutorial` model
///   • update (paging)   → NextPage / PrevPage messages
///   • BackClicked flag  → a GoBack command publishing RulesBackEvent
///   • draw / drawVisual → Content (text + card-image sprites)
///
/// Suit glyphs (♠♦) are rendered as ASCII (S/D) because the Nu default font is
/// the same font-safe one the ImSim port chose; everything else is verbatim.
namespace Kasino
open System
open System.Numerics
open Prime
open Nu
open Kasino.Domain

// ── Model ─────────────────────────────────────────────────────────────
type Tutorial =
    { CurrentPage: int }
    static member initial = { CurrentPage = 0 }

type TutorialMessage =
    | NextPage
    | PrevPage
    | KeyInput of KeyboardKey
    | Nil
    interface Message

type TutorialCommand =
    | GoBack
    interface Command

[<AutoOpen>]
module TutorialExtensions =
    type Screen with
        member this.GetTutorial world = this.GetModelGeneric<Tutorial> world
        member this.SetTutorial value world = this.SetModelGeneric<Tutorial> value world
        member this.Tutorial = this.ModelGeneric<Tutorial> ()
        member this.RulesBackEvent = Events.RulesBackEvent --> this

// ── Page content (verbatim from the original, ASCII-safe) ─────────────
module private Pages =

    type Page =
        | TextPage of title: string * lines: string list
        | VisualPage of title: string * id: int

    let all : Page[] =
        [| TextPage ("Game Overview",
            [ "Kasino is a classic Finnish card game for 2-4 players."
              "The goal is to capture cards from the table by matching"
              "values from your hand."
              ""
              "Each round, players are dealt cards in waves of 4."
              "On your turn you MUST play one card from your hand:"
              "  - If it can capture table cards, you may take them"
              "    (optional in Standard, forced in Laisto)."
              "  - Otherwise your card is placed on the table."
              ""
              "After all cards are played, scores are tallied."
              "The first player to reach 16 cumulative points wins!"
              ""
              "The game uses a standard 52-card deck (no jokers)."
              "2 players: 6 deal waves.  3 players: 4.  4 players: 3." ])
           TextPage ("Card Values",
            [ "Cards have TWO different value systems:"
              ""
              "TABLE VALUE (for summing on the table):"
              "  Ace = 1,  2-10 = face value,  J = 11,  Q = 12,  K = 13"
              ""
              "HAND VALUE (capture power when played from hand):"
              "  Most cards use their table value, but three are special:"
              ""
              "  Ace  = 14   (captures any combo summing to 14)"
              "  2 of Spades  = 15   (captures combos summing to 15)"
              "  10 of Diamonds = 16   (captures combos summing to 16)"
              ""
              "Example: Playing an Ace from hand can capture"
              "  a King + Ace on the table (13 + 1 = 14),"
              "  or a 9 + 5 (= 14), or even 8 + 5 + 1 (= 14)." ])
           VisualPage ("Card Values at a Glance", 1)
           TextPage ("Capturing Cards",
            [ "When you play a card, ALL non-overlapping subsets of"
              "table cards that sum to your hand card's value must"
              "be captured simultaneously."
              ""
              "Example: You play a 7 (hand value 7)."
              "  Table has: 3, 4, 2, 5, 7"
              "  Subsets summing to 7: {7}, {3,4}, {2,5}"
              "  {7} and {3,4} and {2,5} don't overlap => take ALL."
              "  You capture 5 cards at once!"
              ""
              "If subsets OVERLAP, you must choose which to take."
              "The game shows these as tappable buttons."
              ""
              "CAPTURE PREVIEW (green/yellow highlights):"
              "  Green = definitely captured (in all options)"
              "  Yellow = captured in some options (choice needed)" ])
           VisualPage ("Capturing with a 9", 2)
           VisualPage ("Take or Leave", 3)
           TextPage ("Sweeps & Round End",
            [ "SWEEP: If your capture takes ALL remaining table cards,"
              "that's a Sweep! Sweeps earn bonus points."
              ""
              "ROUND END: After all deal waves are exhausted and all"
              "hands are empty, the round ends."
              "  - The last player who captured cards takes any"
              "    cards remaining on the table (NOT a sweep)."
              "  - Scores are calculated for the round."
              "  - Each player's round score adds to their cumulative."
              ""
              "DEALING STRUCTURE:"
              "  The deck has 52 cards. 4 go to the table at the start."
              "  Remaining 48 cards dealt in waves of 4 per player:"
              "    2 players: 6 waves   3 players: 4   4 players: 3" ])
           TextPage ("Scoring",
            [ "SCORING (per round):"
              ""
              "  Most cards captured .... 1 point"
              "  Most spades captured ... 2 points"
              "  Each Ace captured ...... 1 point  (max 4)"
              "  10 of Diamonds ......... 2 points"
              "  2 of Spades ............ 1 point"
              "  Each Sweep ............. 1 point"
              ""
              "TIE RULES: If two or more players tie for most cards"
              "or most spades, nobody scores it that round. The points"
              "carry over as a pot: whoever later wins the category"
              "outright collects the pot plus that round's points."
              ""
              "SWEEP ADJUSTMENT: The minimum sweep count among all"
              "players is subtracted from everyone's sweep total."
              ""
              "SWEEP FREEZE: Once any player has 10 or more total"
              "points, sweeps score nothing for the rest of the game."
              ""
              "TARGET: First player to reach 16 cumulative points wins." ])
           VisualPage ("Scoring Cards", 4)
           TextPage ("Laistokasino",
            [ "LAISTOKASINO (also called Misa-Kasino):"
              ""
              "The rules are identical, but the goal is REVERSED:"
              "you want to MINIMIZE your point total!"
              ""
              "The first player to reach 16 points LOSES."
              "The winner is the player with the FEWEST points."
              ""
              "STRATEGY TIPS:"
              "  - Avoid capturing Aces and special cards."
              "  - Track which specials (Aces, 2S, 10D) are gone."
              "  - Keep a card on the table so an opponent can't"
              "    park a special card on an empty table."
              "  - Sweeps hurt you! Try to leave cards on the table."
              "  - Sometimes placing a card beats capturing." ]) |]

    let count = all.Length
    let title i = match all[i] with TextPage (t, _) -> t | VisualPage (t, _) -> t

// ── Dispatcher ────────────────────────────────────────────────────────
type RulesScreenDispatcher () =
    inherit ScreenDispatcher<Tutorial, TutorialMessage, TutorialCommand> (Tutorial.initial)

    override this.GetFallbackModel (_, _, _) = Tutorial.initial

    override this.Definitions (_, _) =
        [Game.KeyboardKeyDownEvent =|> fun evt ->
            if not evt.Data.Repeated then KeyInput evt.Data.KeyboardKey else Nil]

    override this.Message (model, message, _, _) =
        let next m = { m with CurrentPage = min (Pages.count - 1) (m.CurrentPage + 1) }
        let prev m = { m with CurrentPage = max 0 (m.CurrentPage - 1) }
        match message with
        | NextPage -> just (next model)
        | PrevPage -> just (prev model)
        | KeyInput k ->
            match k with
            | KeyboardKey.Right -> just (next model)
            | KeyboardKey.Left -> just (prev model)
            | KeyboardKey.Escape -> withSignal GoBack model
            | _ -> just model
        | Nil -> just model

    override this.Command (_, command, screen, world) =
        match command with
        | GoBack -> World.publish () screen.RulesBackEvent screen world

    override this.Content (model, _) =

        // visual-page helpers (Nu coords: center origin, Y up, 640x360)
        let cw, ch = 30.0f, 39.0f
        let cardSprite key (c: Card) x y =
            Content.staticSprite key
                [Entity.Position == v3 x y 0.0f
                 Entity.Size == v3 cw ch 0.0f
                 Entity.StaticImage == CardImg.cardAsset c
                 Entity.Elevation == 1.0f]
        let centerLine key (s: string) y (col: Color) (size: single) =
            Content.text key
                [Entity.Position == v3 0.0f y 0.0f
                 Entity.Size == v3 624.0f 14.0f 0.0f
                 Entity.Text == s
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor == col
                 Entity.FontSizing == Some size
                 Entity.Elevation == 2.0f]
        // a centered row of cards, each with a caption beneath
        let row prefix (items: (Card * string) list) y (col: Color) =
            let n = items.Length
            let gap = 36.0f
            let totalW = float32 n * cw + float32 (max 0 (n - 1)) * gap
            let startX = -totalW / 2.0f + cw / 2.0f
            [ for i, (c, lbl) in List.indexed items do
                let x = startX + float32 i * (cw + gap)
                cardSprite (prefix + "c" + string i) c x y
                Content.text (prefix + "l" + string i)
                    [Entity.Position == v3 x (y - ch / 2.0f - 9.0f) 0.0f
                     Entity.Size == v3 (cw + gap) 12.0f 0.0f
                     Entity.Text == lbl
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.TextColor == col
                     Entity.FontSizing == Some 8.5f
                     Entity.Elevation == 2.0f] ]
        // a group of cards left-to-right at (x,y) with a label to the right
        let group prefix (cards: Card list) x y (label: string) (col: Color) =
            let gap = 6.0f
            let rightEdge = x + float32 (max 0 (cards.Length - 1)) * (cw + gap) + cw / 2.0f
            let labelW = 360.0f
            [ for i, c in List.indexed cards do
                cardSprite (prefix + string i) c (x + float32 i * (cw + gap)) y
              Content.text (prefix + "lbl")
                [Entity.Position == v3 (rightEdge + 12.0f + labelW / 2.0f) y 0.0f
                 Entity.Size == v3 labelW 14.0f 0.0f
                 Entity.Text == label
                 Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                 Entity.TextColor == col
                 Entity.FontSizing == Some 9.0f
                 Entity.Elevation == 2.0f] ]

        let visual vid =
            match vid with
            | 1 ->
                [ yield centerLine "v1a" "Every card has a TABLE value and a HAND value." 92.0f Clr.white 10.0f
                  yield centerLine "v1b" "Add table values; spend HAND value to capture." 76.0f Clr.lightGray 9.0f
                  yield centerLine "v1c" "Normal cards: value = face value" 50.0f Clr.gold 9.0f
                  yield! row "v1r1" [ ({Suit=Clubs;Rank=Four}, "worth 4"); ({Suit=Hearts;Rank=Seven}, "worth 7"); ({Suit=Spades;Rank=King}, "worth 13") ] 18.0f Clr.white
                  yield centerLine "v1d" "Three special cards have EXTRA capture power:" (-50.0f) Clr.gold 9.0f
                  yield! row "v1r2" [ ({Suit=Diamonds;Rank=Ace}, "Ace = 14"); ({Suit=Spades;Rank=Two}, "2S = 15"); ({Suit=Diamonds;Rank=Ten}, "10D = 16") ] (-82.0f) Clr.lightGreen ]
            | 2 ->
                let gx = -150.0f
                [ yield centerLine "v2a" "You play a 9 from your hand." 96.0f Clr.white 10.0f
                  yield centerLine "v2b" "It captures any group of table cards that ADDS UP to 9." 80.0f Clr.lightGray 8.5f
                  yield! group "v2g1" [ {Suit=Hearts;Rank=Nine} ] gx 48.0f "<-  the 9 you play from hand" Clr.gold
                  yield! group "v2g2" [ {Suit=Clubs;Rank=Nine} ] gx 6.0f "a 9        = 9    captured" Clr.limeGreen
                  yield! group "v2g3" [ {Suit=Spades;Rank=Three}; {Suit=Diamonds;Rank=Six} ] gx (-36.0f) "3 + 6   = 9    captured" Clr.limeGreen
                  yield! group "v2g4" [ {Suit=Hearts;Rank=Eight} ] gx (-78.0f) "8 alone is not 9  ->  it stays" Clr.lightSalmon
                  yield centerLine "v2c" "One 9 grabs BOTH groups at once (here: 3 cards)." (-118.0f) Clr.white 9.0f ]
            | 3 ->
                let gx = -60.0f
                [ yield centerLine "v3a" "Your 9 CAN capture the 9 on the table. Must you?" 96.0f Clr.white 10.0f
                  yield! group "v3g" [ {Suit=Hearts;Rank=Nine}; {Suit=Clubs;Rank=Nine} ] gx 50.0f "hand 9  +  table 9" Clr.gold
                  yield centerLine "v3b" "STANDARD KASINO  -  capturing is OPTIONAL" 0.0f Clr.limeGreen 9.5f
                  yield centerLine "v3c" "Take the 9, or simply place a card on the table." (-18.0f) Clr.lightGray 8.5f
                  yield centerLine "v3d" "LAISTOKASINO  -  capturing is FORCED" (-58.0f) Clr.lightSalmon 9.5f
                  yield centerLine "v3e" "If a capture is possible, you MUST take it." (-76.0f) Clr.lightGray 8.5f
                  yield centerLine "v3f" "(In Laisto you try NOT to collect, so a forced take hurts.)" (-104.0f) Clr.gray 8.5f ]
            | 4 ->
                let gx = -150.0f
                [ yield centerLine "v4a" "Most points come from special cards and majorities:" 96.0f Clr.white 9.5f
                  yield! group "v4g1" [ {Suit=Diamonds;Rank=Ten} ] gx 52.0f "10D  =  2 points" Clr.gold
                  yield! group "v4g2" [ {Suit=Spades;Rank=Two} ] gx 8.0f "2S   =  1 point" Clr.gold
                  yield! group "v4g3" [ {Suit=Spades;Rank=Ace}; {Suit=Hearts;Rank=Ace}; {Suit=Diamonds;Rank=Ace}; {Suit=Clubs;Rank=Ace} ] gx (-36.0f) "each Ace = 1 point (4 total)" Clr.white
                  yield! group "v4g4" [ {Suit=Spades;Rank=Four}; {Suit=Spades;Rank=Seven}; {Suit=Spades;Rank=Nine} ] gx (-80.0f) "most Spades = 2 points" Clr.white
                  yield centerLine "v4b" "Most cards = 1 point          Each sweep = 1 point" (-120.0f) Clr.lightGray 9.0f ]
            | _ -> []

        // body: text lines or a visual page
        let body =
            match Pages.all[model.CurrentPage] with
            | Pages.TextPage (_, lines) ->
                let startY = 92.0f
                let lineH = 12.0f
                [ for i, line in List.indexed lines do
                    if line <> "" then
                        let col = if line.StartsWith "  " then Clr.lightGray else Clr.white
                        Content.text ("Line" + string i)
                            // Position is the entity CENTRE, so a left-justified box at x=-305
                            // with width 620 started its text at x=-615 — far off the left edge
                            // (visible range is ±320). Centre the box (left edge at -300) instead.
                            // Text/TextColor must use := (dynamic): the "Line{i}" entities are
                            // reused when paging between adjacent text pages, and == only applies
                            // at creation, so == left stale text from the previous page.
                            [Entity.Position == v3 0.0f (startY - float32 i * lineH) 0.0f
                             Entity.Size == v3 600.0f 12.0f 0.0f
                             Entity.Text := line
                             Entity.Justification == Justified (JustifyLeft, JustifyMiddle)
                             Entity.TextColor := col
                             Entity.FontSizing == Some 9.0f
                             Entity.Elevation == 2.0f] ]
            | Pages.VisualPage (_, vid) -> visual vid

        // ── assemble ────────────────────────────────────────────────
        [Content.group "Gui" []
            [Content.staticSprite "Bg"
                [Entity.Absolute == true
                 Entity.Position == v3 0.0f 0.0f 0.0f
                 Entity.Size == v3 640.0f 360.0f 0.0f
                 Entity.StaticImage == Assets.Default.White
                 Entity.Color == Clr.screenBg
                 Entity.Elevation == -1.0f]
             Content.text "Header"
                [Entity.Position == v3 0.0f 165.0f 0.0f
                 Entity.Size == v3 600.0f 20.0f 0.0f
                 Entity.Text == "How to Play Kasino"
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor == Clr.gold
                 Entity.FontSizing == Some 16.0f
                 Entity.Elevation == 2.0f]
             Content.text "PageTitle"
                [Entity.Position == v3 0.0f 145.0f 0.0f
                 Entity.Size == v3 600.0f 16.0f 0.0f
                 Entity.Text := Pages.title model.CurrentPage
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor == Clr.white
                 Entity.FontSizing == Some 12.0f
                 Entity.Elevation == 2.0f]
             Content.text "PageIndicator"
                [Entity.Position == v3 0.0f 128.0f 0.0f
                 Entity.Size == v3 300.0f 14.0f 0.0f
                 Entity.Text := $"Page {model.CurrentPage + 1} / {Pages.count}"
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor == Clr.gray
                 Entity.FontSizing == Some 9.0f
                 Entity.Elevation == 2.0f]
             Content.staticSprite "Separator"
                [Entity.Position == v3 0.0f 118.0f 0.0f
                 Entity.Size == v3 600.0f 1.5f 0.0f
                 Entity.StaticImage == Assets.Default.White
                 Entity.Color == Clr.darkGray
                 Entity.Elevation == 1.0f]

             yield! body

             // navigation
             Content.button "BackBtn"
                [Entity.Position == v3 -270.0f -158.0f 0.0f
                 Entity.Size == v3 90.0f 24.0f 0.0f
                 Entity.Text == "Back"
                 Entity.Elevation == 3.0f
                 Entity.ClickEvent => GoBack]
             if model.CurrentPage > 0 then
                Content.button "PrevBtn"
                    [Entity.Position == v3 -70.0f -158.0f 0.0f
                     Entity.Size == v3 110.0f 24.0f 0.0f
                     Entity.Text == "Previous"
                     Entity.Elevation == 3.0f
                     Entity.ClickEvent => PrevPage]
             if model.CurrentPage < Pages.count - 1 then
                Content.button "NextBtn"
                    [Entity.Position == v3 70.0f -158.0f 0.0f
                     Entity.Size == v3 110.0f 24.0f 0.0f
                     Entity.Text == "Next"
                     Entity.Elevation == 3.0f
                     Entity.ClickEvent => NextPage]
             Content.text "KbHint"
                [Entity.Position == v3 0.0f -175.0f 0.0f
                 Entity.Size == v3 500.0f 12.0f 0.0f
                 Entity.Text == "Arrow keys: navigate   |   Esc: back"
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor == Clr.darkGray
                 Entity.FontSizing == Some 8.0f
                 Entity.Elevation == 2.0f]]]
