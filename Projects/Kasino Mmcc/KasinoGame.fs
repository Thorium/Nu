/// KASINO — MMCC port — the top-level game.
///
/// Owns which screen is desired and bridges the screens:
///   • Menu's StartGameEvent → install a fresh Gameplay model, go Playing.
///   • Menu's ShowRulesEvent  → Tutorial; Rules' RulesBackEvent → Menu.
///   • Gameplay's QuitEvent       → Menu.
///   • Gameplay's ShowScoresEvent → copy the round result into the Scores model, show it.
///   • Scores' ContinueEvent → resume the match (next round); MenuEvent → Menu.
namespace Kasino
open System
open System.Numerics
open Prime
open Nu
open Kasino.Domain

// top-level MMCC model: which part of the game we're in
type KasinoGame =
    | Splash
    | AtMenu
    | Playing
    | AtScores
    /// rules screen remembers where to go back
    | Tutorial of returnTo: KasinoGame

type KasinoGameMessage =
    | ShowMenu
    /// from menu → return to menu
    | ShowRules
    /// from gameplay → return to gameplay
    | ShowHelp
    | CloseRules
    | StartGame of GameEngine.GameConfig
    | ShowScores
    | ResumeMatch
    interface Message

type KasinoGameCommand =
    | InstallGame of GameEngine.GameConfig
    | PopulateScores
    | ResumeGameplay
    | Exit
    interface Command

[<AutoOpen>]
module KasinoGameExtensions =
    type Game with
        member this.GetKasinoGame world = this.GetModelGeneric<KasinoGame> world
        member this.SetKasinoGame value world = this.SetModelGeneric<KasinoGame> value world
        member this.KasinoGame = this.ModelGeneric<KasinoGame> ()

type KasinoGameDispatcher () =
    inherit GameDispatcher<KasinoGame, KasinoGameMessage, KasinoGameCommand> (Splash)

    override this.Definitions (model, _) =
        [Game.DesiredScreen :=
            match model with
            | Splash -> Desire Simulants.Splash
            | AtMenu -> Desire Simulants.Menu
            | Playing -> Desire Simulants.Gameplay
            | AtScores -> Desire Simulants.Scores
            | Tutorial _ -> Desire Simulants.Rules
         if model = Splash then Simulants.Splash.DeselectingEvent => ShowMenu
         Simulants.Menu.StartGameEvent =|> fun evt -> StartGame evt.Data
         Simulants.Menu.ShowRulesEvent => ShowRules
         Simulants.Gameplay.HelpEvent => ShowHelp
         Simulants.Rules.RulesBackEvent => CloseRules
         Simulants.Gameplay.QuitEvent => ShowMenu
         Simulants.Gameplay.ShowScoresEvent => ShowScores
         Simulants.Scores.ContinueEvent => ResumeMatch
         Simulants.Scores.MenuEvent => ShowMenu
         Game.ExitRequestEvent => Exit]

    override this.Message (model, message, _, _) =
        match message with
        | ShowMenu -> just AtMenu
        | ShowRules -> just (Tutorial AtMenu)
        | ShowHelp -> just (Tutorial Playing)
        | CloseRules -> (match model with Tutorial back -> just back | _ -> just AtMenu)
        | StartGame config -> withSignal (InstallGame config) Playing
        | ShowScores -> withSignal PopulateScores AtScores
        | ResumeMatch -> withSignal ResumeGameplay Playing

    override this.Command (_, command, _, world) =
        match command with
        | InstallGame config ->
            Simulants.Gameplay.SetGameplay (Gameplay.start config) world
        | PopulateScores ->
            // copy the just-finished round's result out of the (still-selected)
            // Gameplay model into the Scores screen's own model
            let gp = Simulants.Gameplay.GetGameplay world
            Simulants.Scores.SetScores
                { Breakdowns = gp.ScoreBreakdowns
                  Cumulative = gp.CumulativeScores
                  RoundNumber = gp.RoundNumber
                  Variant = gp.Config.Variant
                  IsGameOver = (gp.Phase = GameOver) }
                world
        | ResumeGameplay ->
            // the Gameplay model persisted across the Scores detour; deal next round
            let gp = Simulants.Gameplay.GetGameplay world
            Simulants.Gameplay.SetGameplay (GameplayLogic.startNextRound gp) world
        | Exit ->
            // close-window button (engine publishes ExitRequestEvent); ignore in the editor
            if world.Unaccompanied then World.exit world

    override this.Content (_, _) =

        // a little snappier than the engine default (incoming 0.5s / outgoing 1.0s)
        let fastDissolve = { Constants.Dissolve.Default with IncomingTime = GameTime.ofSeconds 0.35; OutgoingTime = GameTime.ofSeconds 0.5 }

        // splash art: "KASINO" above a fan of the four aces (held-in-hand shape),
        // replacing the default Nu slide logo (SlideImageOpt = None hides it).
        let aces = [ Spades, 18.0f; Hearts, 6.0f; Diamonds, -6.0f; Clubs, -18.0f ]
        let aceCards =
            let r = 130.0f
            [ for i, (suit, deg) in List.indexed aces do
                let a = deg * 0.017453293f          // degrees → radians
                Content.staticSprite ("Ace" + string i)
                    [Entity.Position == v3 (-r * sin a) (-100.0f + r * cos a) 0.0f
                     Entity.Size == v3 60.0f 78.0f 0.0f
                     Entity.Rotation == Quaternion.CreateFromAngle2d a
                     Entity.StaticImage == CardImg.cardAsset { Suit = suit; Rank = Ace }
                     Entity.Elevation == (1.0f + float32 i * 0.01f)] ]
        let splashContent =
            [Content.group "Gui" []
                ([Content.staticSprite "Bg"
                    [Entity.Absolute == true
                     Entity.Position == v3 0.0f 0.0f 0.0f
                     Entity.Size == v3 640.0f 360.0f 0.0f
                     Entity.StaticImage == Assets.Default.White
                     Entity.Color == Clr.screenBg
                     Entity.Elevation == -1.0f]
                  Content.text "Title"
                    [Entity.Position == v3 0.0f 120.0f 0.0f
                     Entity.Size == v3 600.0f 40.0f 0.0f
                     Entity.Text == "KASINO"
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.TextColor == Clr.gold
                     Entity.FontSizing == Some 36.0f
                     Entity.Elevation == 2.0f]
                  Content.text "Subtitle"
                    [Entity.Position == v3 0.0f -60.0f 0.0f
                     Entity.Size == v3 600.0f 20.0f 0.0f
                     Entity.Text == "Finnish Card Game"
                     Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                     Entity.TextColor == Clr.white
                     Entity.FontSizing == Some 14.0f
                     Entity.Elevation == 2.0f]]
                 @ aceCards)]

        [Content.screen Simulants.Splash.Name (Slide (Constants.Dissolve.Default, { Constants.Slide.Default with SlideImageOpt = None }, None, Simulants.Menu)) [] splashContent
         Content.screen<MenuDispatcher> Simulants.Menu.Name (Dissolve (fastDissolve, None)) [] []
         Content.screen<GameplayDispatcher> Simulants.Gameplay.Name (Dissolve (Constants.Dissolve.Default, None)) [] []
         Content.screen<RulesScreenDispatcher> Simulants.Rules.Name (Dissolve (Constants.Dissolve.Default, None)) [] []
         Content.screen<ScoreDispatcher> Simulants.Scores.Name (Dissolve (Constants.Dissolve.Default, None)) [] []]
