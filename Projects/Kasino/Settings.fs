namespace Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Optional, player-configurable settings plus the data that backs
// the two opt-in flavour features (AI personalities and table-talk).
// All of this is shared by the desktop (MonoGame) and web (Fable)
// front-ends so they behave identically.
// ─────────────────────────────────────────────────────────────

/// User-adjustable options, edited on the start-menu Options screen.
/// Everything that could distract defaults to OFF.
module Settings =

    type GameSettings =
        { RandomCardBacks: bool   // pick a random scenic card back per game
          DefaultScatter: bool    // start games in Random Scatter (vs Strict Grid)
          ChatEnabled: bool       // AI table-talk / banter
          AiPersonalities: bool } // named opponents with distinct play styles

    /// Defaults: the look that shipped (random backs + scatter) stays on; the
    /// two new gameplay-flavour features are off so nothing changes unasked.
    let defaultSettings =
        { RandomCardBacks = true
          DefaultScatter = true
          ChatEnabled = false
          AiPersonalities = false }

/// Named AI opponents with a play style. Used only when AiPersonalities is on;
/// otherwise computer players are plainly named "CPU"/"CPU 1"…
module Personality =

    type Personality =
        { Name: string
          Style: AI.PlayStyle }

    /// Up to four distinct opponents (max CPU count is 4). Inspired by the
    /// original 2002 prototype's named players, translated to English.
    let all : Personality list =
        [ { Name = "Reno the Risk-taker"; Style = AI.Aggressive }
          { Name = "Cautious Cara";       Style = AI.Cautious }
          { Name = "Steady Pat";          Style = AI.Balanced }
          { Name = "Greedy Greta";        Style = AI.Aggressive } ]

    /// Personality for the n-th computer player (0-based), wrapping if needed.
    let forCpu (cpuIndex: int) =
        all[((cpuIndex % all.Length) + all.Length) % all.Length]

/// English table-talk, translated/condensed from the prototype's Finnish
/// banter. Pure and deterministic (no Random) so both front-ends match; the
/// caller passes a varying seed (e.g. a turn counter) to rotate phrases.
module Chat =

    /// What just happened on the speaker's turn — picks the phrase flavour.
    type Mood =
        | Sweep
        | Capture
        | Place
        | Idle

    let private lines =
        function
        | Sweep   -> [| "There it is!"; "Swept the whole table! :)"; "All mine!" |]
        | Capture -> [| "Mmm, points incoming..."; "That'll do nicely."; "Into the pile you go." |]
        | Place   -> [| "Nothing fits... your turn."; "Hmm, I'll just leave this here."; "Can't do much with that." |]
        | Idle    -> [| "Did the aces go already?"; "Who's holding the ten of diamonds?"
                        "Any spades left in the deck?"; "How many tens are still out?" |]

    /// A phrase for the mood, chosen deterministically from the seed.
    let pick (seed: int) (mood: Mood) =
        let xs = lines mood
        xs[((seed % xs.Length) + xs.Length) % xs.Length]
