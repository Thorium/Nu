/// KASINO — MMCC port — presentation helpers.
/// Card-image mapping, layout constants and the colour palette, lifted from the
/// ImSim project's Kasino.fs but with the module-level mutable `AppState`
/// dependency removed: anything that varied per-game (e.g. the card back) is now
/// passed in explicitly, so these are pure helpers the MMCC Content can call.
namespace Kasino
open System
open System.Numerics
open Prime
open Nu
open Kasino.Domain

// ─── Card Image Mapping ───────────────────────────────────────────────
[<RequireQualifiedAccess>]
module CardImg =

    let private suitPrefix = function
        | Spades   -> "sp"
        | Hearts   -> "he"
        | Diamonds -> "di"
        | Clubs    -> "cl"

    let private rankSuffix = function
        | Ace   -> "1"  | Two   -> "2"  | Three -> "3"
        | Four  -> "4"  | Five  -> "5"  | Six   -> "6"
        | Seven -> "7"  | Eight -> "8"  | Nine  -> "9"
        | Ten   -> "10" | Jack  -> "j"  | Queen -> "q"
        | King  -> "k"

    /// Nu asset for a card face image.
    let cardAsset (card: Card) : Image AssetTag =
        asset<Image> "Default" ($"{suitPrefix card.Suit}{rankSuffix card.Rank}")

    /// Deck image (scenic design with stacked edges baked in, chosen per-game,
    /// e.g. "back1") — used for the deck pile and deck icon only.
    let backAsset (backName: string) : Image AssetTag =
        asset<Image> "Default" backName

    /// Plain single-card back for face-down hand cards (not the deck image).
    let handBackAsset : Image AssetTag =
        asset<Image> "Default" "back"

// ─── Layout Constants ─────────────────────────────────────────────────
// Nu virtual resolution is 640×360 → visible range ±320 (X) × ±180 (Y)
[<RequireQualifiedAccess>]
module Ly =

    let cardW = 44.0f
    let cardH = 57.0f
    let cardGap = 5.0f
    let tableGap = 4.0f

    let handY = -130.0f          // human hand at bottom
    let tableY = 10.0f           // table center
    let topOppY = 135.0f         // top opponent
    let sideLeftX = -285.0f
    let sideRightX = 285.0f

    /// Which edge of the table a player occupies, viewed from the bottom seat.
    type Seat =
        | SeatBottom
        | SeatLeft
        | SeatTop
        | SeatRight

    /// Seats advance clockwise (seen from above, like poker): the player after
    /// the bottom seat sits on the left, then the top, then the right — so the
    /// index turn order reads clockwise around the table. The lone opponent of
    /// a 2-player game stays at the top.
    let seatOf (playerCount: int) (bottomIdx: int) (idx: int) =
        match playerCount, (idx - bottomIdx + playerCount) % playerCount with
        | _, 0 -> SeatBottom
        | 2, _ -> SeatTop
        | 3, 1 -> SeatLeft
        | 3, _ -> SeatTop
        | _, 1 -> SeatLeft
        | _, 2 -> SeatTop
        | _, _ -> SeatRight

    let tableW = 500.0f
    let tableH = 130.0f

    let titleY = 155.0f
    let subtitleY = 125.0f
    let menuBaseY = 55.0f
    let btnW = 240.0f
    let btnH = 28.0f
    let btnGap = 36.0f

    let statusY = -95.0f
    let turnTextY = -110.0f

    /// X of the leftmost of N centered cards.
    let centerCardsX (count: int) (gap: float32) =
        let totalW = float32 count * (cardW + gap) - gap
        -totalW / 2.0f

    /// Cards per table row (10 * 48px ≈ 480 ≤ tableW).
    let tablePerRow = 10
    let private tableRowGap = 5.0f

    /// Center position of the i-th of `count` table cards (wrapped rows, centered
    /// on tableY). Shared by the board renderer and the capture/collect animation
    /// so a captured card animates from exactly where it sat.
    let tablePos (count: int) (i: int) : single * single =
        let rows = max 1 ((count + tablePerRow - 1) / tablePerRow)
        let rowPitch = cardH + tableRowGap
        let row = i / tablePerRow
        let col = i % tablePerRow
        let cardsInRow = min tablePerRow (count - row * tablePerRow)
        let leftX = centerCardsX cardsInRow tableGap
        let cx = leftX + cardW / 2.0f + float32 col * (cardW + tableGap)
        let cy = tableY + (float32 (rows - 1) / 2.0f - float32 row) * rowPitch
        (cx, cy)

    /// Center position of the i-th of `count` hand cards (single centered row).
    let handPos (count: int) (i: int) : single * single =
        let leftX = centerCardsX count cardGap
        (leftX + cardW / 2.0f + float32 i * (cardW + cardGap), handY)

    /// Deterministic scattered position for a card within the table area. Pure in
    /// the card (hashed), so it's stable across frames without storing state and
    /// the same card animates from exactly where it was drawn.
    let scatterPos (card: Card) : single * single =
        let h = abs (hash (struct (card.Suit, card.Rank)))
        let rx = float32 (h % 997) / 997.0f * 2.0f - 1.0f
        let ry = float32 (h / 997 % 991) / 991.0f * 2.0f - 1.0f
        let maxX = tableW / 2.0f - cardW / 2.0f - 8.0f
        let maxY = tableH / 2.0f - cardH / 2.0f - 6.0f
        (rx * maxX, tableY + ry * maxY)

    /// Table position honouring the layout option: scattered (per-card) or wrapped
    /// rows (by index). Shared by the board renderer and the play/collect anims.
    let tableCardPos (scatter: bool) (count: int) (i: int) (card: Card) : single * single =
        if scatter then scatterPos card else tablePos count i

// ─── Colours ──────────────────────────────────────────────────────────
[<RequireQualifiedAccess>]
module Clr =
    let screenBg = color 0.098f 0.196f 0.137f 1.0f
    let tableBg = color 0.137f 0.392f 0.216f 1.0f
    let gold = color 1.0f 0.843f 0.0f 1.0f
    let white = color 1.0f 1.0f 1.0f 1.0f
    let gray = color 0.627f 0.627f 0.627f 1.0f
    let lightGray = color 0.827f 0.827f 0.827f 1.0f
    let tintGreen = color 0.7f 1.0f 0.7f 1.0f              // definite capture tint
    let tintYellow = color 1.0f 1.0f 0.65f 1.0f            // possible capture tint
    let yellow = color 1.0f 1.0f 0.0f 1.0f
    let limeGreen = color 0.196f 0.804f 0.196f 1.0f
    let lightSalmon = color 1.0f 0.627f 0.478f 1.0f
    let lightGreen = color 0.565f 0.933f 0.565f 1.0f
    let darkGray = color 0.412f 0.412f 0.412f 1.0f
    let modalOverlay = color 0.0f 0.0f 0.0f 0.627f
