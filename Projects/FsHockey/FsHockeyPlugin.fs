namespace FsHockey
open System
open Nu
open HockeyDemo.Physics
open HockeyDemo.Game
open FsHockey

// this is a plugin for the Nu game engine that directs the execution of your application and editor.
type FsHockeyPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor. Because the whole
    // match simulation is one immutable value stored on the Gameplay screen,
    // the editor's undo/redo can rewind live CPU-vs-CPU gameplay in the
    // "Gameplay" mode below (PRNG included — it's part of the state).
    override this.EditModes =
        Map.ofList
            [("Menu", fun world -> Game.SetHockeyMode HockeyMenu world)
             ("Gameplay", fun world ->
                Simulants.Gameplay.SetMatchState
                    (createMatch
                        { Team1Idx = 4; Team2Idx = 6           // Neptune vs Jupiter, CPU vs CPU
                          Team1Human = false; Team2Human = false
                          FivePlayer = false; FastHuman = true; HardMode = false
                          NumPeriods = ExhibitionPeriods; Seed = 42UL }) world
                Simulants.Gameplay.SetLeagueMode false world
                Game.SetHockeyMode HockeyPlaying world)]
