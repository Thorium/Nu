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

    /// Static scoring value of a captured card.
    /// Combines direct points with fractional contributions toward
    /// "most cards" (1pt / 52 cards) and "most spades" (2pts / 13 spades).
    let scoringValue (card: Card) : float =
        let direct =
            if isDiamondTen card then 2.0
            elif isSpadeTwo card then 1.0
            elif isAce card then 1.0
            else 0.0
        let cardFrac = 1.0 / 52.0
        let spadeFrac = if isSpade card then 2.0 / 13.0 else 0.0
        direct + cardFrac + spadeFrac

    /// Context-aware scoring value considering the race for "most cards/spades".
    let scoringValueInContext
        (myCards: int) (mySpades: int)
        (opponentCards: int) (opponentSpades: int)
        (cardsRemaining: int)
        (card: Card) : float =

        let direct =
            if isDiamondTen card then 2.0
            elif isSpadeTwo card then 1.0
            elif isAce card then 1.0
            else 0.0

        let cardGap = float (myCards + 1 - opponentCards)
        let cardMargin =
            if cardsRemaining <= 0 then
                if cardGap >= 1.0 then 1.0 else 0.0
            else
                let half = float cardsRemaining / 2.0
                1.0 / (1.0 + exp (-(cardGap / half) * 3.0))

        let spadeMargin =
            if isSpade card then
                let spadeGap = float (mySpades + 1 - opponentSpades)
                let spadesRem = max 1 (cardsRemaining / 4)
                let half = float spadesRem / 2.0
                2.0 / (1.0 + exp (-(spadeGap / half) * 3.0))
            else 0.0

        direct + cardMargin + spadeMargin
