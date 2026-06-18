/// KASINO — MMCC port — the Menu screen.
///
/// Ported from the original /mnt/c/git/Kasino MenuScreen + OptionsScreen. The
/// original's `MenuState = { Step; Variant; PlayerCount; HumanCount }` + per-step
/// `update` maps directly onto an MMCC model + messages; the Options screen is
/// folded in here as an overlay sub-state (`ShowingOptions`) rather than a second
/// top-level screen, which avoids cross-screen state plumbing.
namespace Kasino
open System
open System.Numerics
open Prime
open Nu
open Kasino.Domain

type MenuStep =
    | VariantSelect
    | PlayerCountSelect
    | HumanCountSelect

// MMCC model: the in-progress menu selection plus the options sub-state.
type Menu =
    { Step: MenuStep
      Variant: GameVariant
      PlayerCount: int
      HumanCount: int
      Settings: Settings.GameSettings
      ShowingOptions: bool }

    static member initial =
        { Step = VariantSelect
          Variant = StandardKasino
          PlayerCount = 2
          HumanCount = 1
          Settings = { Settings.defaultSettings with AiPersonalities = true; ChatEnabled = true }
          ShowingOptions = false }

type MenuMessage =
    | ChooseVariant of GameVariant
    | ChoosePlayerCount of int
    | ChooseHumanCount of int
    | BackStep
    | OpenOptions
    | CloseOptions
    | ToggleRandomBacks
    | ToggleScatter
    | ToggleChat
    | TogglePersonalities
    | KeyInput of KeyboardKey
    | Nil
    interface Message

type MenuCommand =
    | StartGameCmd of GameEngine.GameConfig   // publish StartGameEvent (→ game starts a match)
    | ShowRulesCmd                            // publish ShowRulesEvent (→ tutorial screen)
    | ExitGame                                // quit the whole application
    interface Command

[<AutoOpen>]
module MenuExtensions =
    type Screen with
        member this.StartGameEvent = Events.StartGameEvent --> this
        member this.ShowRulesEvent = Events.ShowRulesEvent --> this

type MenuDispatcher () =
    inherit ScreenDispatcher<Menu, MenuMessage, MenuCommand> (Menu.initial)

    override this.GetFallbackModel (_, _, _) = Menu.initial

    override this.Definitions (_, _) =
        [Game.KeyboardKeyDownEvent =|> fun evt ->
            if not evt.Data.Repeated then KeyInput evt.Data.KeyboardKey else Nil]

    override this.Message (menu, message, _, _) =
        let mkConfig (m: Menu) : GameEngine.GameConfig =
            { Variant = m.Variant; PlayerCount = m.PlayerCount; HumanCount = m.HumanCount
              Seed = None; TargetScore = 16; Settings = m.Settings }
        match message with
        | ChooseVariant v -> just { menu with Variant = v; Step = PlayerCountSelect }
        | ChoosePlayerCount n -> just { menu with PlayerCount = n; Step = HumanCountSelect }
        | ChooseHumanCount n ->
            let m = { menu with HumanCount = n }
            withSignal (StartGameCmd (mkConfig m)) m
        | BackStep ->
            let step =
                match menu.Step with
                | HumanCountSelect -> PlayerCountSelect
                | PlayerCountSelect -> VariantSelect
                | VariantSelect -> VariantSelect
            just { menu with Step = step }
        | OpenOptions -> just { menu with ShowingOptions = true }
        | CloseOptions -> just { menu with ShowingOptions = false }
        | ToggleRandomBacks -> just { menu with Settings = { menu.Settings with RandomCardBacks = not menu.Settings.RandomCardBacks } }
        | ToggleScatter -> just { menu with Settings = { menu.Settings with DefaultScatter = not menu.Settings.DefaultScatter } }
        | ToggleChat -> just { menu with Settings = { menu.Settings with ChatEnabled = not menu.Settings.ChatEnabled } }
        | TogglePersonalities -> just { menu with Settings = { menu.Settings with AiPersonalities = not menu.Settings.AiPersonalities } }
        // Esc closes the options overlay if open, otherwise quits the application
        | KeyInput KeyboardKey.Escape -> if menu.ShowingOptions then just { menu with ShowingOptions = false } else withSignal ExitGame menu
        | KeyInput _ | Nil -> just menu

    override this.Command (_, command, screen, world) =
        match command with
        | StartGameCmd config -> World.publish config screen.StartGameEvent screen world
        | ShowRulesCmd -> World.publish () screen.ShowRulesEvent screen world
        | ExitGame -> World.exit world

    override this.Content (menu, _) =

        // ── small builders ──────────────────────────────────────────
        let textLine name (s: string) y (col: Color) (size: single) =
            Content.text name
                [Entity.Position == v3 0.0f y 0.0f
                 Entity.Size == v3 600.0f 22.0f 0.0f
                 Entity.Text == s
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor == col
                 Entity.FontSizing == Some size
                 Entity.Elevation == 1.0f]
        let btn name (s: string) x y w (msg: MenuMessage) =
            Content.button name
                [Entity.Position == v3 x y 0.0f
                 Entity.Size == v3 w Ly.btnH 0.0f
                 Entity.Text == s
                 Entity.Elevation == 1.0f
                 Entity.ClickEvent => msg]

        // ── current step's prompt + choice buttons ──────────────────
        let vName = match menu.Variant with StandardKasino -> "Standard" | LaistoKasino -> "Laisto"
        let stepContent =
            match menu.Step with
            | VariantSelect ->
                [ textLine "Prompt" "Choose game variant:" 88.0f Clr.lightGray 12.0f
                  btn "BtnStd" "Standard Kasino (maximize)" 0.0f 45.0f 280.0f (ChooseVariant StandardKasino)
                  btn "BtnLaisto" "Laistokasino (minimize)" 0.0f 8.0f 280.0f (ChooseVariant LaistoKasino) ]
            | PlayerCountSelect ->
                [ textLine "Prompt" $"Variant: {vName}  —  Number of players:" 88.0f Clr.lightGray 12.0f
                  btn "Btn2" "2 Players" -130.0f 45.0f 120.0f (ChoosePlayerCount 2)
                  btn "Btn3" "3 Players" 0.0f 45.0f 120.0f (ChoosePlayerCount 3)
                  btn "Btn4" "4 Players" 130.0f 45.0f 120.0f (ChoosePlayerCount 4)
                  btn "BtnBack" "< Back" -250.0f -150.0f 100.0f BackStep ]
            | HumanCountSelect ->
                [ textLine "Prompt" $"Variant: {vName}  |  Players: {menu.PlayerCount}  —  Humans:" 88.0f Clr.lightGray 12.0f
                  btn "BtnAI" "Watch AI Only" 0.0f 45.0f 280.0f (ChooseHumanCount 0)
                  btn "BtnPlay" "Play Yourself" 0.0f 8.0f 280.0f (ChooseHumanCount 1)
                  btn "BtnBack" "< Back" -250.0f -150.0f 100.0f BackStep ]

        // ── options overlay ─────────────────────────────────────────
        let onOff b = if b then "ON" else "OFF"
        let scatterLabel = if menu.Settings.DefaultScatter then "Scatter" else "Grid"
        let optBtn name (s: string) y (msg: MenuMessage) =
            Content.button name
                [Entity.Position == v3 0.0f y 0.0f
                 Entity.Size == v3 380.0f Ly.btnH 0.0f
                 Entity.Text == s
                 Entity.Elevation == 7.0f
                 Entity.ClickEvent => msg]
        let optionsOverlay =
            [ Content.staticSprite "OptBg"
                [Entity.Absolute == true
                 Entity.Position == v3 0.0f 0.0f 0.0f
                 Entity.Size == v3 640.0f 360.0f 0.0f
                 Entity.StaticImage == Assets.Default.White
                 Entity.Color == Clr.modalOverlay
                 Entity.Elevation == 6.0f]
              Content.text "OptTitle"
                [Entity.Position == v3 0.0f 130.0f 0.0f
                 Entity.Size == v3 600.0f 22.0f 0.0f
                 Entity.Text == "Options"
                 Entity.Justification == Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor == Clr.gold
                 Entity.FontSizing == Some 18.0f
                 Entity.Elevation == 7.0f]
              optBtn "OptBacks" $"Random card backs:  {onOff menu.Settings.RandomCardBacks}" 80.0f ToggleRandomBacks
              optBtn "OptScatter" $"Table layout:  {scatterLabel}" 42.0f ToggleScatter
              optBtn "OptChat" $"AI table-talk:  {onOff menu.Settings.ChatEnabled}" 4.0f ToggleChat
              optBtn "OptPers" $"AI personalities:  {onOff menu.Settings.AiPersonalities}" -34.0f TogglePersonalities
              // NB: elevation 7 so it draws ABOVE the modal overlay sprite (elevation 6);
              // the generic `btn` helper uses elevation 1, which left it behind the dark dim.
              Content.button "OptBack"
                [Entity.Position == v3 0.0f -100.0f 0.0f
                 Entity.Size == v3 160.0f Ly.btnH 0.0f
                 Entity.Text == "Back"
                 Entity.Elevation == 7.0f
                 Entity.ClickEvent => CloseOptions] ]

        // ── assemble ────────────────────────────────────────────────
        [Content.group "Gui" []
            [Content.staticSprite "Bg"
                [Entity.Absolute == true
                 Entity.Position == v3 0.0f 0.0f 0.0f
                 Entity.Size == v3 640.0f 360.0f 0.0f
                 Entity.StaticImage == Assets.Default.White
                 Entity.Color == Clr.screenBg
                 Entity.Elevation == -1.0f]
             textLine "Title" "KASINO" Ly.titleY Clr.gold 30.0f
             textLine "Subtitle" "Finnish Card Game — MMCC" Ly.subtitleY Clr.white 14.0f

             if menu.ShowingOptions then
                yield! optionsOverlay
             else
                yield! stepContent
                // Options / How to Play available on every step
                btn "BtnOptions" "Options" 0.0f -110.0f 200.0f OpenOptions
                Content.button "BtnRules"
                    [Entity.Position == v3 0.0f -148.0f 0.0f
                     Entity.Size == v3 200.0f Ly.btnH 0.0f
                     Entity.Text == "How to Play"
                     Entity.Elevation == 1.0f
                     Entity.ClickEvent => ShowRulesCmd]
                // quit the application (also available via Esc, or the window's X button)
                Content.button "BtnQuit"
                    [Entity.Position == v3 270.0f 165.0f 0.0f
                     Entity.Size == v3 90.0f 22.0f 0.0f
                     Entity.Text == "Quit"
                     Entity.Elevation == 1.0f
                     Entity.ClickEvent => ExitGame]]]
