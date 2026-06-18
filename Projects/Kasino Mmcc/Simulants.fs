namespace Kasino
open System
open Nu

// Global handles to the game's key simulants (screens & a few named entities).
[<RequireQualifiedAccess>]
module Simulants =

    // splash screen
    let Splash = Game / "Splash"

    // menu screen
    let Menu = Game / "Menu"

    // gameplay screen
    let Gameplay = Game / "Gameplay"
    let GameplayScene = Gameplay / "Scene"
    let GameplayGui = Gameplay / "Gui"

    // scores screen
    let Scores = Game / "Scores"
    let ScoresGui = Scores / "Gui"

    // rules / help (tutorial) screen
    let Rules = Game / "Rules"
