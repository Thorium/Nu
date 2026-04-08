namespace FsHockey
open System
open Nu
open FsHockey

// this is a plugin for the Nu game engine that directs the execution of your application and editor.
type FsHockeyPlugin () =
    inherit NuPlugin ()

    // this exposes different editing modes in the editor.
    override this.EditModes =
        Map.ofList
            [("Initial", fun world -> Game.SetHockeyMode HockeyMenu world)]
