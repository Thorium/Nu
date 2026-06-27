namespace Kasino
open System
open Prime
open Nu
open Kasino.Domain
open Kasino

// Nu plugin for the MMCC Kasino port: directs execution and the editor.
type KasinoMmccPlugin () =
    inherit NuPlugin ()

    override this.EditModes =
        let demoConfig : GameEngine.GameConfig =
            { Variant = StandardKasino; Seats = GameEngine.SeatCount.ofIntOrDefault 2; HumanCount = 1
              Seed = None; TargetScore = 16
              Settings = { Settings.defaultSettings with AiPersonalities = true; ChatEnabled = true } }
        Map.ofList
            [("Splash", fun world -> Game.SetKasinoGame Splash world)
             ("Menu", fun world -> Game.SetKasinoGame AtMenu world)
             ("Gameplay", fun world ->
                Simulants.Gameplay.SetGameplay (Gameplay.start demoConfig) world
                Game.SetKasinoGame Playing world)]
