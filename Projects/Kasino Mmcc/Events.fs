namespace Kasino
open System
open Nu
open Kasino.Domain

// User-defined events used to communicate between screens and the top-level game.
[<RequireQualifiedAccess>]
module Events =

    /// Raised by the Menu screen: start a match with the chosen configuration.
    let StartGameEvent = stoa<GameEngine.GameConfig> "StartGame/Event"

    /// Raised by the Gameplay screen: the match is over / the player asked to
    /// return — the game should go back to the menu.
    let QuitEvent = stoa<unit> "Quit/Event"

    /// Raised by the Menu screen: open the rules / help (tutorial) screen.
    let ShowRulesEvent = stoa<unit> "ShowRules/Event"

    /// Raised by the Rules screen: close it and return to the menu.
    let RulesBackEvent = stoa<unit> "RulesBack/Event"

    /// Raised by the Gameplay screen when a round/game ends: show the score screen.
    let ShowScoresEvent = stoa<unit> "ShowScores/Event"

    /// Raised by the Scores screen: continue to the next round.
    let ScoresContinueEvent = stoa<unit> "ScoresContinue/Event"

    /// Raised by the Scores screen: return to the menu (after game over).
    let ScoresMenuEvent = stoa<unit> "ScoresMenu/Event"

    /// Raised by the Gameplay screen "?" button: open the rules, returning to the game.
    let HelpEvent = stoa<unit> "Help/Event"
