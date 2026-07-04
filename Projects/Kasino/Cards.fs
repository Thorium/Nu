namespace Kasino.Domain

open System

// ─────────────────────────────────────────────────────────────
// Core types for the Finnish Kasino card game
// ─────────────────────────────────────────────────────────────

/// Card suit (maa)
[<Struct>]
type Suit =
    | Spades    // Pata
    | Hearts    // Hertta
    | Diamonds  // Ruutu
    | Clubs     // Risti

/// Card rank (arvo)
[<Struct>]
type Rank =
    | Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King

/// A playing card
[<Struct>]
type Card = { Suit: Suit; Rank: Rank }

/// Game variant
[<Struct>]
type GameVariant =
    | StandardKasino   // Maximize points
    | LaistoKasino     // Minimize points (also known as Misa-Kasino)

/// Player type
[<Struct>]
type PlayerType =
    | Human
    | Computer

/// A player in the game
type Player =
    { Name: string
      Type: PlayerType
      Hand: Card list
      CapturedCards: Card list
      Sweeps: int }

/// Result of playing a card
type PlayResult =
    | Capture of handCard: Card * captured: Card list * sweep: bool
    | Place of handCard: Card

// ─────────────────────────────────────────────────────────────
// Card utility functions
// ─────────────────────────────────────────────────────────────

module Cards =

    /// Rank display name
    let rankName = function
        | Ace   -> "A"  | Two   -> "2"  | Three -> "3"
        | Four  -> "4"  | Five  -> "5"  | Six   -> "6"
        | Seven -> "7"  | Eight -> "8"  | Nine  -> "9"
        | Ten   -> "10" | Jack  -> "J"  | Queen -> "Q"
        | King  -> "K"

    /// Suit text abbreviation (font-safe, no Unicode symbols)
    let suitSymbol = function
        | Spades   -> "s"
        | Hearts   -> "h"
        | Diamonds -> "d"
        | Clubs    -> "c"

    /// Suit name
    let suitName = function
        | Spades   -> "Spades"
        | Hearts   -> "Hearts"
        | Diamonds -> "Diamonds"
        | Clubs    -> "Clubs"

    /// Card display string (e.g. "As", "10d")
    let display (card: Card) =
        $"{rankName card.Rank}{suitSymbol card.Suit}"

    /// Face value on the table: A=1, 2..10, J=11, Q=12, K=13
    let tableValue = function
        | Ace -> 1    | Two -> 2    | Three -> 3  | Four -> 4
        | Five -> 5   | Six -> 6    | Seven -> 7  | Eight -> 8
        | Nine -> 9   | Ten -> 10   | Jack -> 11  | Queen -> 12
        | King -> 13

    /// Capture power from hand: A=14, ♠2=15, ♦10=16, rest=tableValue
    let handValue (card: Card) =
        match card.Suit, card.Rank with
        | _,        Ace -> 14
        | Spades,   Two -> 15
        | Diamonds, Ten -> 16
        | _,        _   -> tableValue card.Rank

    let isSpade    (c: Card) = c.Suit = Spades
    let isSpadeTwo (c: Card) = c.Suit = Spades && c.Rank = Two
    let isDiamondTen (c: Card) = c.Suit = Diamonds && c.Rank = Ten
    let isAce      (c: Card) = c.Rank = Ace

    /// All 13 ranks in order
    let allRanks =
        [ Ace; Two; Three; Four; Five; Six; Seven
          Eight; Nine; Ten; Jack; Queen; King ]

    /// All 4 suits
    let allSuits = [ Spades; Hearts; Diamonds; Clubs ]

    /// Create a full 52-card deck
    let createDeck () =
        [ for suit in allSuits do
            for rank in allRanks do
                { Suit = suit; Rank = rank } ]

    /// Fisher-Yates shuffle
    let shuffle (rng: Random) (deck: Card list) =
        let arr = Array.ofList deck
        for i = arr.Length - 1 downto 1 do
            let j = rng.Next(i + 1)
            let tmp = arr[i]
            arr[i] <- arr[j]
            arr[j] <- tmp
        Array.toList arr

    /// Deal n cards from the top: returns (dealt, remaining)
    let deal (n: int) (deck: Card list) =
        let count = min n (List.length deck)
        List.splitAt count deck

    /// Direct, per-card points: 10♦ = 2, 2♠ = 1, each Ace = 1.
    let directValue (card: Card) : float =
        if isDiamondTen card then 2.0
        elif isSpadeTwo card then 1.0
        elif isAce card then 1.0
        else 0.0

    /// Static scoring value of a captured card.
    /// Combines direct points with fractional contributions toward
    /// "most cards" (1pt / 52 cards) and "most spades" (2pts / 13 spades).
    let scoringValue (card: Card) : float =
        let cardFrac = 1.0 / 52.0
        let spadeFrac = if isSpade card then 2.0 / 13.0 else 0.0
        directValue card + cardFrac + spadeFrac

    /// Likelihood-weighted value of leading the "most cards" / "most spades"
    /// races, modelled as logistic sigmoids over the current lead. Used as a
    /// per-card heuristic when choosing which single card to play (it is NOT
    /// summed across a capture — see captureRaceValue for that).
    let private cardRaceMargin (cardGap: float) (cardsRemaining: int) =
        if cardsRemaining <= 0 then
            if cardGap >= 1.0 then 1.0 else 0.0
        else
            let half = float cardsRemaining / 2.0
            1.0 / (1.0 + exp (-(cardGap / half) * 3.0))

    let private spadeRaceMargin (spadeGap: float) (cardsRemaining: int) =
        let spadesRem = max 1 (cardsRemaining / 4)
        let half = float spadesRem / 2.0
        2.0 / (1.0 + exp (-(spadeGap / half) * 3.0))

    /// Context-aware scoring value of a single card, considering the race for
    /// "most cards/spades". Suitable for ranking individual hand cards.
    let scoringValueInContext
        (myCards: int) (mySpades: int)
        (opponentCards: int) (opponentSpades: int)
        (cardsRemaining: int)
        (card: Card) : float =

        let cardMargin = cardRaceMargin (float (myCards + 1 - opponentCards)) cardsRemaining
        let spadeMargin =
            if isSpade card then spadeRaceMargin (float (mySpades + 1 - opponentSpades)) cardsRemaining
            else 0.0
        directValue card + cardMargin + spadeMargin

    /// Context-aware value of an entire capture. The "most cards" and "most
    /// spades" race bonuses (worth 1 and 2 points for the whole round) are
    /// counted ONCE here — based on the totals after the capture — rather than
    /// per captured card, which would multiply a single-point race many times.
    let captureRaceValue
        (myCards: int) (mySpades: int)
        (opponentCards: int) (opponentSpades: int)
        (cardsRemaining: int)
        (capturedCards: int) (capturedSpades: int) : float =

        // The played hand card also lands in the pile, hence the +1 in the
        // card count. capturedSpades must already include the played card
        // when it is a spade (the caller banks handCard :: captured).
        let cardMargin = cardRaceMargin (float (myCards + capturedCards + 1 - opponentCards)) cardsRemaining
        let spadeMargin =
            if capturedSpades > 0 then spadeRaceMargin (float (mySpades + capturedSpades - opponentSpades)) cardsRemaining
            else 0.0
        cardMargin + spadeMargin
