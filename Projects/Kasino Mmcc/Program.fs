namespace Kasino
open System
open System.IO
open Nu
module Program =

    // entry point for the MMCC Kasino application
    let [<EntryPoint; STAThread>] main _ =

        // point the working directory at the application's base directory
        Directory.SetCurrentDirectory AppContext.BaseDirectory

        // initialize Nu before other Nu code is run
        Nu.init ()

        // window configuration
        let sdlWindowConfig = { SdlWindowConfig.defaultConfig with WindowTitle = "Kasino (MMCC) - Finnish Card Game" }

        // SDL configuration
        let sdlConfig = { SdlConfig.defaultConfig with WindowConfig = sdlWindowConfig }

        // world configuration
        let worldConfig = { WorldConfig.defaultConfig with SdlConfig = sdlConfig }

        // run the engine with the MMCC plugin
        World.run worldConfig (KasinoMmccPlugin ())
