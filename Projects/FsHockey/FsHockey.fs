/// THE FS HOCKEY LEAGUE — Nu Engine Port (idiomatic ImSim edition)
///
/// All application state lives in simulant properties as immutable values:
/// the menu settings on the Game, the whole match simulation on the Gameplay
/// screen (a single immutable Match record, advanced by the pure gameTick),
/// and the league table on the Game. Nothing is mutated in place, so the
/// editor's live gameplay undo/redo works out of the box, and all GUI events
/// are handled inline in the ImSim style (if World.doButton ... then ...).
namespace FsHockey
open System
open System.Numerics
open Prime
open Nu
open HockeyDemo.Physics
open HockeyDemo.Game

// ─── Application Mode ─────────────────────────────────────────────────
[<Struct>]
type HockeyMode =
    | HockeyMenu
    | HockeyPlaying
    | HockeyLeagueMatchup
    | HockeyLeaguePlaying
    | HockeyLeagueStandings
    | HockeyLeagueFinalStandings

/// Menu selections, kept as one immutable value on the Game simulant.
type MenuSettings =
    { SelectedTeam1: int
      SelectedTeam2: int
      ActiveColumn: int
      FastHuman: bool
      HardMode: bool
      FivePlayer: bool
      GamepadEnabled: bool }

module MenuSettings =
    let initial =
        { SelectedTeam1 = 0
          SelectedTeam2 = 1
          ActiveColumn = 0
          FastHuman = true
          HardMode = false
          FivePlayer = false
          GamepadEnabled = true }

/// What the gameplay screen asks the game dispatcher to do next. The screen
/// only reports the request; the game dispatcher owns the mode transitions
/// (mirroring how Breakout ImSim's Gameplay screen reports Quit).
[<Struct>]
type GameplayRequest =
    | NoRequest
    | ContinueRequested
    | QuitRequested

// ─── Defaults ─────────────────────────────────────────────────────────
module Defaults =

    /// A quiet, non-advancing match used as the default property value.
    let matchState =
        { createMatch
            { Team1Idx = 4; Team2Idx = 6
              Team1Human = false; Team2Human = false
              FivePlayer = false; FastHuman = true; HardMode = false
              NumPeriods = ExhibitionPeriods; Seed = 1UL } with Playing = false }

// ─── Property Extensions ──────────────────────────────────────────────
[<AutoOpen>]
module FsHockeyExtensions =

    type Game with
        member this.GetHockeyMode world : HockeyMode = this.Get (nameof Game.HockeyMode) world
        member this.SetHockeyMode (value : HockeyMode) world = this.Set (nameof Game.HockeyMode) value world
        member this.HockeyMode = lens (nameof Game.HockeyMode) this this.GetHockeyMode this.SetHockeyMode
        member this.GetSettings world : MenuSettings = this.Get (nameof Game.Settings) world
        member this.SetSettings (value : MenuSettings) world = this.Set (nameof Game.Settings) value world
        member this.Settings = lens (nameof Game.Settings) this this.GetSettings this.SetSettings
        member this.GetLeague world : League option = this.Get (nameof Game.League) world
        member this.SetLeague (value : League option) world = this.Set (nameof Game.League) value world
        member this.League = lens (nameof Game.League) this this.GetLeague this.SetLeague

    type Screen with
        member this.GetMatchState world : Match = this.Get (nameof Screen.MatchState) world
        member this.SetMatchState (value : Match) world = this.Set (nameof Screen.MatchState) value world
        member this.MatchState = lens (nameof Screen.MatchState) this this.GetMatchState this.SetMatchState
        member this.GetLeagueMode world : bool = this.Get (nameof Screen.LeagueMode) world
        member this.SetLeagueMode (value : bool) world = this.Set (nameof Screen.LeagueMode) value world
        member this.LeagueMode = lens (nameof Screen.LeagueMode) this this.GetLeagueMode this.SetLeagueMode
        member this.GetRequest world : GameplayRequest = this.Get (nameof Screen.Request) world
        member this.SetRequest (value : GameplayRequest) world = this.Set (nameof Screen.Request) value world
        member this.Request = lens (nameof Screen.Request) this this.GetRequest this.SetRequest

// ─── Coordinate Mapping ───────────────────────────────────────────────
// Game: top-left origin (0,0), X right, Y down, ~320x200 field + HUD
// Nu: center origin (0,0), X right, Y up
module Coords =
    let scale = 1.5f
    let gameW = 320.0f
    let gameH = 248.0f // 200 field + 48 HUD
    let halfW = gameW / 2.0f
    let halfH = gameH / 2.0f

    /// Map game X to Nu X
    let inline nuX (gx: float<px>) = (float32 (stripPx gx) - halfW) * scale

    /// Map game Y to Nu Y (flip vertical)
    let inline nuY (gy: float<px>) = -(float32 (stripPx gy) - halfH) * scale

    /// Map raw float game coords to Nu position
    let inline nuPos (gx: float<px>) (gy: float<px>) = v3 (nuX gx) (nuY gy) 0.0f

    /// Nu size from game-coordinate dimensions
    let inline nuSize (w: float32) (h: float32) = v3 (w * scale) (h * scale) 0.0f

    /// Position from raw float screen coords (for HUD/menus)
    let inline screenPos (x: float32) (y: float32) = v3 ((x - halfW) * scale) (-(y - halfH) * scale) 0.0f

// ─── Colors (CGA-inspired) ────────────────────────────────────────────
module Colors =
    let ice = color 0.784f 0.863f 0.941f 1.0f
    let board = color 0.235f 0.314f 0.471f 1.0f
    let red = color 0.706f 0.157f 0.157f 1.0f
    let blue = color 0.157f 0.314f 0.706f 1.0f
    let team1 = color 0.863f 0.235f 0.235f 1.0f
    let team2 = color 0.235f 0.392f 0.863f 1.0f
    let puck = color 0.078f 0.078f 0.078f 1.0f
    let hudBg = color 0.078f 0.078f 0.157f 1.0f
    let hudText = color 0.863f 0.863f 0.863f 1.0f
    let goalFlash = color 1.0f 1.0f 0.314f 1.0f
    let white = color 1.0f 1.0f 1.0f 1.0f
    let gray = color 0.627f 0.627f 0.627f 1.0f
    let dim = color 0.412f 0.412f 0.49f 1.0f
    // button label color: the default ButtonUp/ButtonDown images are dark
    // green, so labels want to be near-white
    let buttonText = color 0.94f 0.94f 0.94f 1.0f
    let darkBg = color 0.039f 0.039f 0.118f 1.0f
    let rowBg = color 0.09f 0.09f 0.2f 1.0f
    let rowSelBg = color 0.157f 0.235f 0.392f 1.0f
    let skin = color 0.902f 0.765f 0.627f 1.0f
    let trousers = color 0.118f 0.118f 0.118f 1.0f
    let skate = color 0.314f 0.314f 0.314f 1.0f
    let stickBrown = color 0.545f 0.353f 0.169f 1.0f
    let goaliePad = color 0.902f 0.863f 0.784f 1.0f
    let goalieMask = color 0.863f 0.863f 0.863f 1.0f
    let helmetGold = color 0.784f 0.706f 0.157f 1.0f
    let helmetBlack = color 0.118f 0.118f 0.118f 1.0f
    let glove = color 0.235f 0.235f 0.235f 1.0f

// ─── Simulants ────────────────────────────────────────────────────────
[<RequireQualifiedAccess>]
module Simulants =
    let Menu = Game / "Menu"
    let Gameplay = Game / "Gameplay"
    let Matchup = Game / "Matchup"
    let Standings = Game / "Standings"

// ─── Input Helpers ────────────────────────────────────────────────────
module HockeyInput =

    let GamepadDeadzone = 0.35f

    /// Read pad `idx` as an Input snapshot (all-false when not connected).
    let gamepadInput (idx: int) (world: World) : Input =
        let stick = World.getStickLeft idx world // SDL convention: Y positive = down
        let dir = World.getDirection idx world
        { Left =
            stick.X < -GamepadDeadzone
            || (match dir with DirectionLeft | DirectionUpLeft | DirectionDownLeft -> true | _ -> false)
          Right =
            stick.X > GamepadDeadzone
            || (match dir with DirectionRight | DirectionUpRight | DirectionDownRight -> true | _ -> false)
          Up =
            stick.Y < -GamepadDeadzone
            || (match dir with DirectionUp | DirectionUpLeft | DirectionUpRight -> true | _ -> false)
          Down =
            stick.Y > GamepadDeadzone
            || (match dir with DirectionDown | DirectionDownLeft | DirectionDownRight -> true | _ -> false)
          Fire =
            World.isButtonDown idx ButtonA world
            || World.isButtonDown idx ButtonB world
            || World.getTriggerRight idx world > 0.12f }

    /// Combine keyboard and gamepad snapshots (either source counts).
    let mergeInput (a: Input) (b: Input) : Input =
        { Left = a.Left || b.Left
          Right = a.Right || b.Right
          Up = a.Up || b.Up
          Down = a.Down || b.Down
          Fire = a.Fire || b.Fire }

    /// Player 1: Arrow keys + RShift/Enter (+ gamepad 0)
    let player1 gamepadOn (world: World) : Input =
        let kb =
            { Left = World.isKeyboardKeyDown KeyboardKey.Left world
              Right = World.isKeyboardKeyDown KeyboardKey.Right world
              Up = World.isKeyboardKeyDown KeyboardKey.Up world
              Down = World.isKeyboardKeyDown KeyboardKey.Down world
              Fire = World.isKeyboardKeyDown KeyboardKey.RShift world || World.isKeyboardKeyDown KeyboardKey.Enter world }
        if gamepadOn then mergeInput kb (gamepadInput 0 world) else kb

    /// Player 2: WASD + Space/Tab (+ gamepad 1)
    let player2 gamepadOn (world: World) : Input =
        let kb =
            { Left = World.isKeyboardKeyDown KeyboardKey.A world
              Right = World.isKeyboardKeyDown KeyboardKey.D world
              Up = World.isKeyboardKeyDown KeyboardKey.W world
              Down = World.isKeyboardKeyDown KeyboardKey.S world
              Fire = World.isKeyboardKeyDown KeyboardKey.Space world || World.isKeyboardKeyDown KeyboardKey.Tab world }
        if gamepadOn then mergeInput kb (gamepadInput 1 world) else kb

// ─── Shared Drawing (declarative, reads only immutable state) ─────────
module Draw =

    let background name (c: Color) world =
        World.doStaticSprite name
            [Entity.Position .= v3 0.0f 0.0f 0.0f
             Entity.Size .= v3 720.0f 558.0f 0.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= c
             Entity.Elevation .= -1.0f] world |> ignore

    let rink world =

        // Ice surface
        let iceW = float32 (stripPx FieldRight - stripPx FieldLeft) + 8.0f
        let iceH = float32 (stripPx FieldBottom - stripPx FieldTop) + 8.0f
        let iceCx = (float32 (stripPx FieldLeft) + float32 (stripPx FieldRight)) / 2.0f
        let iceCy = (float32 (stripPx FieldTop) + float32 (stripPx FieldBottom)) / 2.0f
        World.doStaticSprite "Ice"
            [Entity.Position .= Coords.screenPos iceCx iceCy
             Entity.Size .= Coords.nuSize iceW iceH
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.ice
             Entity.Elevation .= 0.0f] world |> ignore

        // Boards (4 edges)
        let fl = float32 (stripPx FieldLeft)
        let fr = float32 (stripPx FieldRight)
        let ft = float32 (stripPx FieldTop)
        let fb = float32 (stripPx FieldBottom)
        let boardW = 3.0f
        let boardSprite name pos size =
            World.doStaticSprite name
                [Entity.Position .= pos
                 Entity.Size .= size
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.board
                 Entity.Elevation .= 0.2f] world |> ignore
        boardSprite "BoardTop" (Coords.screenPos ((fl + fr) / 2.0f) (ft - boardW / 2.0f)) (Coords.nuSize (fr - fl + boardW * 2.0f) boardW)
        boardSprite "BoardBot" (Coords.screenPos ((fl + fr) / 2.0f) (fb + boardW / 2.0f)) (Coords.nuSize (fr - fl + boardW * 2.0f) boardW)
        boardSprite "BoardLeft" (Coords.screenPos (fl - boardW / 2.0f) ((ft + fb) / 2.0f)) (Coords.nuSize boardW (fb - ft + boardW * 2.0f))
        boardSprite "BoardRight" (Coords.screenPos (fr + boardW / 2.0f) ((ft + fb) / 2.0f)) (Coords.nuSize boardW (fb - ft + boardW * 2.0f))

        // Goal nets
        let gt = float32 (stripPx GoalTop)
        let gb = float32 (stripPx GoalBottom)
        let gd = float32 (stripPx GoalDepth)
        World.doStaticSprite "GoalLeft"
            [Entity.Position .= Coords.screenPos (fl - gd / 2.0f) ((gt + gb) / 2.0f)
             Entity.Size .= Coords.nuSize gd (gb - gt)
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= color 0.863f 0.235f 0.235f 0.3f
             Entity.Elevation .= 0.1f] world |> ignore
        World.doStaticSprite "GoalRight"
            [Entity.Position .= Coords.screenPos (fr + gd / 2.0f) ((gt + gb) / 2.0f)
             Entity.Size .= Coords.nuSize gd (gb - gt)
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= color 0.235f 0.392f 0.863f 0.3f
             Entity.Elevation .= 0.1f] world |> ignore

        // Center line + dot
        let cx = float32 (stripPx CenterX)
        World.doStaticSprite "CenterLine"
            [Entity.Position .= Coords.screenPos cx ((ft + fb) / 2.0f)
             Entity.Size .= Coords.nuSize 1.5f (fb - ft)
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.red
             Entity.Elevation .= 0.15f] world |> ignore
        World.doStaticSprite "CenterDot"
            [Entity.Position .= Coords.screenPos cx (float32 (stripPx CenterY))
             Entity.Size .= Coords.nuSize 5.0f 5.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.red
             Entity.Elevation .= 0.15f] world |> ignore

        // Blue lines
        let fieldW = fr - fl
        for i, bx in [1, fl + fieldW / 3.0f; 2, fl + fieldW * 2.0f / 3.0f] do
            World.doStaticSprite $"BlueLine{i}"
                [Entity.Position .= Coords.screenPos bx ((ft + fb) / 2.0f)
                 Entity.Size .= Coords.nuSize 2.0f (fb - ft)
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.blue
                 Entity.Elevation .= 0.15f] world |> ignore

        // Goal lines
        for i, gx in [1, float32 (stripPx GoalLeftX); 2, float32 (stripPx GoalRightX)] do
            World.doStaticSprite $"GoalLine{i}"
                [Entity.Position .= Coords.screenPos gx ((ft + fb) / 2.0f)
                 Entity.Size .= Coords.nuSize 1.0f (fb - ft)
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= color 0.706f 0.157f 0.157f 0.5f
                 Entity.Elevation .= 0.15f] world |> ignore

    let player (name: string) (ent: Entity) (teamColor: Color) (helmetColor: Color) isActive isGoalie (stickAnim: int) (frameTime: single) world =
        let u = 1.2f
        let s = Coords.scale
        let us = u * s
        let basePos = Coords.nuPos ent.X ent.Y

        // Facing angle: rotate from default "up" (+Y in Nu) to entity direction
        let angleRad =
            if ent.DirX <> 0.0 || ent.DirY <> 0.0 then
                atan2 (float32 -ent.DirX) (float32 -ent.DirY)
            else 0.0f
        let rot = v3 0.0f 0.0f (angleRad * (180.0f / MathF.PI))
        let cosA = cos angleRad
        let sinA = sin angleRad

        /// Position with rotated offset from player center ((ox, oy) in Nu
        /// world units; +Y = forward when angle=0).
        let rpos ox oy =
            basePos + v3 (ox * cosA - oy * sinA) (ox * sinA + oy * cosA) 0.0f

        // Skating leg animation
        let speedSq = float (ent.VelX * ent.VelX + ent.VelY * ent.VelY)
        let legOff = if speedSq > 16.0 then sin (frameTime * 0.04f) * 0.36f * us else 0.0f

        let part partName pos size (c: Color) elevation =
            World.doStaticSprite $"{name}{partName}"
                [Entity.Position @= pos
                 Entity.Size .= size
                 Entity.Degrees @= rot
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= c
                 Entity.Elevation .= elevation] world |> ignore

        part "Hlm" (rpos 0.0f (4.5f * us)) (Coords.nuSize (3.0f * u) (2.0f * u)) helmetColor 1.15f
        part "Fce" (rpos 0.0f (3.0f * us)) (Coords.nuSize (2.0f * u) (1.0f * u)) Colors.skin 1.1f
        if isGoalie then
            part "Msk" (rpos (0.5f * us) (3.75f * us)) (Coords.nuSize (1.2f * u) (1.5f * u)) Colors.goalieMask 1.12f
        part "Shd" (rpos 0.0f (1.75f * us)) (Coords.nuSize (7.0f * u) (1.5f * u)) teamColor 1.0f
        part "Bdy" (rpos 0.0f (-0.25f * us)) (Coords.nuSize (6.0f * u) (2.5f * u)) teamColor 1.0f
        part "Str" (rpos 0.0f (0.2f * us)) (Coords.nuSize (6.0f * u) (0.6f * u)) (color 1.0f 1.0f 1.0f 0.3f) 1.05f
        part "LA" (rpos (-3.4f * us) (0.5f * us)) (Coords.nuSize (1.2f * u) (2.8f * u)) teamColor 1.0f
        part "LG" (rpos (-3.4f * us) (-0.8f * us)) (Coords.nuSize (1.2f * u) (0.8f * u)) Colors.glove 1.02f
        part "RA" (rpos (3.4f * us) (0.5f * us)) (Coords.nuSize (1.2f * u) (2.8f * u)) teamColor 1.0f
        part "RG" (rpos (3.4f * us) (-0.8f * us)) (Coords.nuSize (1.2f * u) (0.8f * u)) Colors.glove 1.02f

        let hipW = if isGoalie then 7.0f else 6.0f
        part "Hip" (rpos 0.0f (-2.1f * us)) (Coords.nuSize (hipW * u) (1.2f * u)) (if isGoalie then Colors.goaliePad else Colors.trousers) 1.0f

        if isGoalie then
            part "LPd" (rpos (-1.5f * us) (-3.7f * us)) (Coords.nuSize (3.0f * u) (2.0f * u)) Colors.goaliePad 1.0f
            part "RPd" (rpos (1.5f * us) (-3.7f * us)) (Coords.nuSize (3.0f * u) (2.0f * u)) Colors.goaliePad 1.0f
            part "LSk" (rpos (-1.25f * us) (-4.8f * us)) (Coords.nuSize (1.5f * u) (0.4f * u)) Colors.skate 0.95f
            part "RSk" (rpos (1.25f * us) (-4.8f * us)) (Coords.nuSize (1.5f * u) (0.4f * u)) Colors.skate 0.95f
        else
            part "LL" (rpos (-1.4f * us) (-3.2f * us - legOff)) (Coords.nuSize (2.2f * u) (1.0f * u)) Colors.trousers 1.0f
            part "RL" (rpos (1.4f * us) (-3.2f * us + legOff)) (Coords.nuSize (2.2f * u) (1.0f * u)) Colors.trousers 1.0f
            part "LSk" (rpos (-1.15f * us) (-4.0f * us - legOff)) (Coords.nuSize (1.7f * u) (0.4f * u)) Colors.skate 0.95f
            part "RSk" (rpos (1.15f * us) (-4.0f * us + legOff)) (Coords.nuSize (1.7f * u) (0.4f * u)) Colors.skate 0.95f

        // Stick
        let wobble = if stickAnim > 0 then sin (float32 stickAnim * 1.5f) * 2.5f else 0.0f
        part "Stk" (rpos (2.5f * us) ((3.5f + wobble * 0.3f) * us)) (Coords.nuSize (1.2f * u) (7.0f * u)) Colors.stickBrown 0.9f

        // Active player marker (not rotated, always above player)
        if isActive then
            World.doStaticSprite $"{name}Mrk"
                [Entity.Position @= basePos + v3 0.0f (8.0f * us) 0.0f
                 Entity.Size .= Coords.nuSize (3.0f * u) (2.0f * u)
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.white
                 Entity.Elevation .= 1.2f] world |> ignore

    let hud (m: Match) world =
        let hudY = float32 (stripPx FieldBottom) + 10.0f

        World.doStaticSprite "HudBg"
            [Entity.Position .= Coords.screenPos 160.0f (hudY + 20.0f)
             Entity.Size .= Coords.nuSize 320.0f 44.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.hudBg
             Entity.Elevation .= 2.0f] world |> ignore

        let hudText name pos size (text: string) justify (c: Color) fontSizing =
            World.doText name
                ([Entity.Position .= pos
                  Entity.Size .= size
                  Entity.Text @= text
                  Entity.Justification .= Justified (justify, JustifyMiddle)
                  Entity.TextColor .= c
                  Entity.Elevation .= 3.0f]
                 @ (match fontSizing with Some fs -> [Entity.FontSizing .= Some fs] | None -> []))
                world

        hudText "HudT1Name" (Coords.screenPos 30.0f (hudY + 8.0f)) (Coords.nuSize 100.0f 14.0f) teamNames[m.Team1Idx] JustifyLeft Colors.team1 (Some 8.0f)
        hudText "HudT1Score" (Coords.screenPos 30.0f (hudY + 22.0f)) (Coords.nuSize 60.0f 20.0f) $"{m.Team1Score}" JustifyLeft Colors.team1 None
        hudText "HudT2Name" (Coords.screenPos 280.0f (hudY + 8.0f)) (Coords.nuSize 100.0f 14.0f) teamNames[m.Team2Idx] JustifyRight Colors.team2 (Some 8.0f)
        hudText "HudT2Score" (Coords.screenPos 280.0f (hudY + 22.0f)) (Coords.nuSize 60.0f 20.0f) $"{m.Team2Score}" JustifyRight Colors.team2 None

        let secs = int m.ClockSeconds
        hudText "HudClock" (Coords.screenPos 160.0f (hudY + 8.0f)) (Coords.nuSize 80.0f 20.0f) $"{secs / 60}:{secs % 60:D2}" JustifyCenter Colors.hudText None
        if m.NumPeriods > 1 then
            hudText "HudPeriod" (Coords.screenPos 160.0f (hudY + 22.0f)) (Coords.nuSize 120.0f 14.0f) $"PERIOD {m.CurrentPeriod + 1} of {m.NumPeriods}" JustifyCenter Colors.hudText (Some 8.0f)

// ─── Gameplay Screen ──────────────────────────────────────────────────
type GameplayDispatcher () =
    inherit ScreenDispatcherImSim ()

    static member Properties =
        [define Screen.MatchState Defaults.matchState
         define Screen.LeagueMode false
         define Screen.Request NoRequest]

    override this.Process (_, screen, world) =
        if screen.GetSelected world then

            // advance the simulation: one pure step from Match to Match. The
            // whole game state (including its PRNG) is an immutable value on
            // this screen, so the editor can undo/redo live gameplay.
            let gamepadOn = (Game.GetSettings world).GamepadEnabled
            if world.Advancing && (screen.GetMatchState world).Playing then
                let input1 = HockeyInput.player1 gamepadOn world
                let input2 = if screen.GetLeagueMode world then Input.none else HockeyInput.player2 gamepadOn world
                screen.MatchState.Map (gameTick input1 input2) world

            let m = screen.GetMatchState world

            // declare the scene from the immutable match state
            World.beginGroup "Scene" [] world

            Draw.background "GameBg" (color 0.118f 0.118f 0.196f 1.0f) world
            Draw.rink world

            // trail marks
            for i in 0 .. m.TrailMarkCount - 1 do
                let mark = m.TrailMarks[i]
                if mark.Life > 0<tick> then
                    let alpha = min 0.85f (float32 (int mark.Life) / float32 (int TrailMarkLifetime) * 0.7f + 0.15f)
                    World.doStaticSprite $"Trail{i}"
                        [Entity.Position @= Coords.nuPos mark.MarkX mark.MarkY
                         Entity.Size .= Coords.nuSize 2.5f 2.5f
                         Entity.StaticImage .= Assets.Default.White
                         Entity.Color @= color 1.0f 1.0f 1.0f alpha
                         Entity.Elevation .= 0.1f] world |> ignore

            // puck
            let ball = m.Entities[m.BallIdx]
            World.doStaticSprite "Puck"
                [Entity.Position @= Coords.nuPos ball.X ball.Y
                 Entity.Size .= Coords.nuSize 5.0f 5.0f
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.puck
                 Entity.Elevation .= 0.5f] world |> ignore
            World.doStaticSprite "PuckHL"
                [Entity.Position @= Coords.nuPos ball.X ball.Y + v3 0.0f 0.5f 0.0f
                 Entity.Size .= Coords.nuSize 2.0f 2.0f
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= color 0.235f 0.235f 0.235f 1.0f
                 Entity.Elevation .= 0.6f] world |> ignore

            // players
            let frameTime = single world.UpdateTime
            let t1Helmet = if m.Team1Idx = 0 then Colors.helmetGold else Colors.helmetBlack
            let t2Helmet = if m.Team2Idx = 0 then Colors.helmetGold else Colors.helmetBlack
            for i in 0 .. m.PlayersPerTeam - 1 do
                let isGoalie = m.FivePlayerMode && i = 0
                Draw.player $"P1_{i}" m.Entities[i] Colors.team1 t1Helmet (i = m.ActivePlayer1) isGoalie m.StickAnimTimers[i] frameTime world
            for i in 0 .. m.PlayersPerTeam - 1 do
                let ei = m.Team2Start + i
                let isGoalie = m.FivePlayerMode && i = 0
                Draw.player $"P2_{i}" m.Entities[ei] Colors.team2 t2Helmet (ei = m.ActivePlayer2) isGoalie m.StickAnimTimers[ei] frameTime world

            // HUD
            Draw.hud m world

            // goal flash overlay: a calm fade-out, not a strobe (the old
            // 5-tick alpha alternation flashed the whole screen at ~6 Hz)
            if m.GoalFlashTimer > 0<tick> then
                let alpha = 0.235f * single (int m.GoalFlashTimer) / 90.0f
                World.doStaticSprite "GoalFlash"
                    [Entity.Position .= v3 0.0f 0.0f 0.0f
                     Entity.Size .= v3 720.0f 558.0f 0.0f
                     Entity.StaticImage .= Assets.Default.White
                     Entity.Color @= color 1.0f 1.0f 0.314f alpha
                     Entity.Elevation .= 5.0f] world |> ignore
                let scorerName =
                    match m.GoalScoredBy with
                    | Team1Scored -> teamNames[m.Team1Idx]
                    | Team2Scored -> teamNames[m.Team2Idx]
                    | NoGoal -> ""
                World.doText "GoalText"
                    [Entity.Position .= v3 0.0f 30.0f 0.0f
                     Entity.Size .= v3 400.0f 32.0f 0.0f
                     Entity.Text @= $"GOAL! {scorerName}"
                     Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                     Entity.TextColor .= Colors.goalFlash
                     Entity.Elevation .= 6.0f] world
                World.doText "GoalScore"
                    [Entity.Position .= v3 0.0f 0.0f 0.0f
                     Entity.Size .= v3 300.0f 32.0f 0.0f
                     Entity.Text @= $"{m.Team1Score} - {m.Team2Score}"
                     Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                     Entity.TextColor .= Colors.white
                     Entity.Elevation .= 6.0f] world

            // game-over overlay with inline-handled buttons
            if matchOver m then
                World.doStaticSprite "GameOverBg"
                    [Entity.Position .= v3 0.0f 0.0f 0.0f
                     Entity.Size .= v3 720.0f 558.0f 0.0f
                     Entity.StaticImage .= Assets.Default.White
                     Entity.Color .= color 0.0f 0.0f 0.0f 0.627f
                     Entity.Elevation .= 7.0f] world |> ignore
                World.doText "GameOverTitle"
                    [Entity.Position .= v3 0.0f 80.0f 0.0f
                     Entity.Size .= v3 300.0f 32.0f 0.0f
                     Entity.Text .= "GAME OVER"
                     Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                     Entity.TextColor .= Colors.goalFlash
                     Entity.Elevation .= 8.0f] world
                World.doText "GameOverScore"
                    [Entity.Position .= v3 0.0f 40.0f 0.0f
                     Entity.Size .= v3 500.0f 32.0f 0.0f
                     Entity.Text @= $"{teamNames[m.Team1Idx]}  {m.Team1Score}  -  {m.Team2Score}  {teamNames[m.Team2Idx]}"
                     Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                     Entity.TextColor .= Colors.white
                     Entity.Elevation .= 8.0f] world
                let winner =
                    if m.Team1Score > m.Team2Score then $"{teamNames[m.Team1Idx]} WINS!"
                    elif m.Team2Score > m.Team1Score then $"{teamNames[m.Team2Idx]} WINS!"
                    else "IT'S A TIE!"
                World.doText "GameOverWinner"
                    [Entity.Position .= v3 0.0f 10.0f 0.0f
                     Entity.Size .= v3 400.0f 32.0f 0.0f
                     Entity.Text @= winner
                     Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                     Entity.TextColor .= Colors.goalFlash
                     Entity.Elevation .= 8.0f] world

                // inline event handling: the button click IS the event handler
                let continueText = if screen.GetLeagueMode world then "Standings" else "Rematch"
                if World.doButton "Continue"
                    [Entity.Position .= v3 -90.0f -70.0f 0.0f
                     Entity.Size .= v3 160.0f 40.0f 0.0f
                     Entity.Text .= continueText
                     Entity.TextColor .= Colors.buttonText
                     Entity.Elevation .= 8.0f] world
                   || World.isKeyboardKeyPressed KeyboardKey.Space world then
                    screen.SetRequest ContinueRequested world

                if World.doButton "ToMenu"
                    [Entity.Position .= v3 90.0f -70.0f 0.0f
                     Entity.Size .= v3 160.0f 40.0f 0.0f
                     Entity.Text .= "Menu"
                     Entity.TextColor .= Colors.buttonText
                     Entity.Elevation .= 8.0f] world then
                    screen.SetRequest QuitRequested world

            // escape quits to menu at any point
            if World.isKeyboardKeyPressed KeyboardKey.Escape world then
                screen.SetRequest QuitRequested world

            World.endGroup world

// ─── Game Dispatcher ──────────────────────────────────────────────────
type FsHockeyDispatcher () =
    inherit GameDispatcherImSim ()

    static member Properties =
        [define Game.HockeyMode HockeyMenu
         define Game.Settings MenuSettings.initial
         define Game.League None]

    /// A fresh PRNG seed at the moment a match/league starts.
    static member private FreshSeed (world: World) =
        uint64 (Gen.randomf * 4294967295.0f) ^^^ (uint64 world.UpdateTime <<< 20) ^^^ 0x2545F4914F6CDD1DUL

    static member private StartExhibition (game: Game) world =
        let s = game.GetSettings world
        Simulants.Gameplay.SetMatchState
            (createMatch
                { Team1Idx = s.SelectedTeam1; Team2Idx = s.SelectedTeam2
                  Team1Human = s.SelectedTeam1 = 0; Team2Human = s.SelectedTeam2 = 0
                  FivePlayer = s.FivePlayer; FastHuman = s.FastHuman; HardMode = s.HardMode
                  NumPeriods = ExhibitionPeriods; Seed = FsHockeyDispatcher.FreshSeed world }) world
        Simulants.Gameplay.SetLeagueMode false world
        game.SetHockeyMode HockeyPlaying world

    static member private StartLeagueMatch (game: Game) (league: League) world =
        let s = game.GetSettings world
        let t1, t2 = currentMatchup league
        Simulants.Gameplay.SetMatchState
            (createMatch
                { Team1Idx = t1; Team2Idx = t2
                  Team1Human = t1 = league.HumanTeam; Team2Human = false
                  FivePlayer = s.FivePlayer; FastHuman = s.FastHuman; HardMode = s.HardMode
                  NumPeriods = LeaguePeriods; Seed = FsHockeyDispatcher.FreshSeed world }) world
        Simulants.Gameplay.SetLeagueMode true world
        game.SetHockeyMode HockeyLeaguePlaying world

    override this.Process (game, world) =

        let mode = game.GetHockeyMode world
        let behavior = Dissolve (Constants.Dissolve.Default, None)

        // ─── MENU SCREEN ───────────────────────────────────────────
        World.beginScreen Simulants.Menu.Name (mode = HockeyMenu) behavior [] world |> ignore
        World.beginGroup "Gui" [] world

        Draw.background "MenuBg" Colors.darkBg world

        World.doText "Title"
            [Entity.Position .= Coords.screenPos 160.0f 16.0f
             Entity.Size .= Coords.nuSize 300.0f 16.0f
             Entity.Text .= "THE FS HOCKEY LEAGUE"
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Colors.goalFlash
             Entity.Elevation .= 1.0f] world
        World.doText "Subtitle"
            [Entity.Position .= Coords.screenPos 280.0f 16.0f
             Entity.Size .= Coords.nuSize 100.0f 12.0f
             Entity.Text .= "Tuomas Hietanen, 2026"
             Entity.Justification .= Justified (JustifyRight, JustifyMiddle)
             Entity.TextColor .= Colors.gray
             Entity.FontSizing .= Some 8.0f
             Entity.Elevation .= 1.0f] world

        let settings = game.GetSettings world
        let listBaseY = 32.0f
        let rowH = 12.0f

        // ignore keys/clicks during the first half second so the Enter that
        // launched the game from a terminal can't instantly start a match
        let ready = world.UpdateTime > 30L

        World.doText "Col1Header"
            [Entity.Position .= Coords.screenPos 110.0f (listBaseY - 4.0f)
             Entity.Size .= Coords.nuSize 110.0f 12.0f
             Entity.Text .= "TEAM 1 (LEFT)"
             Entity.TextColor @= (if settings.ActiveColumn = 0 then Colors.team1 else Colors.gray)
             Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
             Entity.Elevation .= 1.0f] world
        World.doText "Col2Header"
            [Entity.Position .= Coords.screenPos 270.0f (listBaseY - 4.0f)
             Entity.Size .= Coords.nuSize 110.0f 12.0f
             Entity.Text .= "TEAM 2 (RIGHT)"
             Entity.TextColor @= (if settings.ActiveColumn = 1 then Colors.team2 else Colors.gray)
             Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
             Entity.Elevation .= 1.0f] world

        // team selection: every row is a clickable button, handled inline.
        // ButtonFacet always renders its image untinted, so the retro row
        // visuals come from a tinted sprite + text (name left, dim speed hint
        // right), with an invisible (EmptyImage) button of the same rect
        // providing the click area.
        let teamRow name x ty selected (label: string) (speed: int) =
            World.doStaticSprite $"{name}Bg"
                [Entity.Position .= Coords.screenPos x (ty + 6.0f)
                 Entity.Size .= Coords.nuSize 110.0f (rowH - 1.0f)
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color @= (if selected then Colors.rowSelBg else Colors.rowBg)
                 Entity.Elevation .= 0.5f] world |> ignore
            World.doText $"{name}Txt"
                [Entity.Position .= Coords.screenPos x (ty + 6.0f)
                 Entity.Size .= Coords.nuSize 110.0f (rowH - 1.0f)
                 Entity.Text @= label
                 Entity.TextColor @= (if selected then Colors.goalFlash else Colors.white)
                 Entity.FontSizing .= Some 9.0f
                 Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                 Entity.Elevation .= 1.0f] world
            // team skating speed, dimmed — a hint of how hard a CPU opponent
            // is; @= because it changes with the fast/hard toggles
            World.doText $"{name}Spd"
                [Entity.Position .= Coords.screenPos x (ty + 6.0f)
                 Entity.Size .= Coords.nuSize 106.0f (rowH - 1.0f)
                 Entity.Text @= $"({speed})"
                 Entity.TextColor .= Colors.dim
                 Entity.FontSizing .= Some 9.0f
                 Entity.Justification .= Justified (JustifyRight, JustifyMiddle)
                 Entity.Elevation .= 1.0f] world
            World.doButton name
                [Entity.Position .= Coords.screenPos x (ty + 6.0f)
                 Entity.Size .= Coords.nuSize 110.0f (rowH - 1.0f)
                 Entity.UpImage .= Assets.Default.EmptyImage
                 Entity.DownImage .= Assets.Default.EmptyImage
                 Entity.ClickSoundOpt .= None
                 Entity.Elevation .= 1.1f] world

        for i in 0 .. NumTeams - 1 do
            let ty = listBaseY + 12.0f + float32 i * rowH
            let sel1 = i = settings.SelectedTeam1
            let sel2 = i = settings.SelectedTeam2
            // reflects the current fast-human/hard-mode settings live
            let spd = displayedTeamSpeed settings.FastHuman settings.HardMode i

            if teamRow $"T1Row{i}" 110.0f ty sel1 $"""{(if sel1 then "> " else "  ")}{teamNames[i]}""" spd then
                game.Settings.Map (fun s -> { s with SelectedTeam1 = i; ActiveColumn = 0 }) world

            if teamRow $"T2Row{i}" 270.0f ty sel2 $"""{(if sel2 then "> " else "  ")}{teamNames[i]}""" spd then
                game.Settings.Map (fun s -> { s with SelectedTeam2 = i; ActiveColumn = 1 }) world

        // toggles: buttons whose labels show the current immutable settings
        let toggle name x (text: string) (f: MenuSettings -> MenuSettings) =
            if World.doButton name
                [Entity.Position .= Coords.screenPos x 172.0f
                 Entity.Size .= Coords.nuSize 74.0f 13.0f
                 Entity.Text @= text
                 Entity.TextColor .= Colors.buttonText
                 Entity.FontSizing .= Some 8.0f
                 Entity.Elevation .= 1.0f] world
               && ready then
                game.Settings.Map f world

        toggle "FastToggle" 60.0f $"""(F)ast Human {(if settings.FastHuman then "ON" else "OFF")}""" (fun s -> { s with FastHuman = not s.FastHuman })
        toggle "HardToggle" 140.0f $"""(H)ard Mode {(if settings.HardMode then "ON" else "OFF")}""" (fun s -> { s with HardMode = not s.HardMode })
        toggle "FiveToggle" 220.0f $"""(5) Players {(if settings.FivePlayer then "6v6" else "3v3")}""" (fun s -> { s with FivePlayer = not s.FivePlayer })
        toggle "PadToggle" 300.0f $"""(G)amepad {(if settings.GamepadEnabled then "ON" else "OFF")}""" (fun s -> { s with GamepadEnabled = not s.GamepadEnabled })

        // main actions, handled inline
        if (World.doButton "StartGame"
                [Entity.Position .= Coords.screenPos 100.0f 188.0f
                 Entity.Size .= Coords.nuSize 110.0f 14.0f
                 Entity.Text .= "START GAME"
                 Entity.TextColor .= Colors.buttonText
                 Entity.FontSizing .= Some 10.0f
                 Entity.Elevation .= 1.0f] world
            || World.isKeyboardKeyPressed KeyboardKey.Enter world)
           && ready then
            FsHockeyDispatcher.StartExhibition game world

        if (World.doButton "PlayLeague"
                [Entity.Position .= Coords.screenPos 220.0f 188.0f
                 Entity.Size .= Coords.nuSize 110.0f 14.0f
                 Entity.Text .= "PLAY LEAGUE"
                 Entity.TextColor .= Colors.buttonText
                 Entity.FontSizing .= Some 10.0f
                 Entity.Elevation .= 1.0f] world
            || World.isKeyboardKeyPressed KeyboardKey.L world)
           && ready then
            game.SetLeague (Some (createLeague settings.SelectedTeam1 (FsHockeyDispatcher.FreshSeed world))) world
            game.SetHockeyMode HockeyLeagueMatchup world

        if (World.doButton "ExitGame"
                [Entity.Position .= Coords.screenPos 330.0f 188.0f
                 Entity.Size .= Coords.nuSize 50.0f 14.0f
                 Entity.Text .= "EXIT"
                 Entity.TextColor .= Colors.buttonText
                 Entity.FontSizing .= Some 10.0f
                 Entity.Elevation .= 1.0f] world
            || World.isKeyboardKeyPressed KeyboardKey.Escape world)
           && ready && world.Unaccompanied then
            World.exit world

        // keyboard team selection (same immutable updates as the buttons)
        if world.Advancing && ready then
            if World.isKeyboardKeyPressed KeyboardKey.Tab world then
                game.Settings.Map (fun s -> { s with ActiveColumn = 1 - s.ActiveColumn }) world
            if World.isKeyboardKeyPressed KeyboardKey.Up world then
                game.Settings.Map
                    (fun s ->
                        if s.ActiveColumn = 0 then { s with SelectedTeam1 = (s.SelectedTeam1 - 1 + NumTeams) % NumTeams }
                        else { s with SelectedTeam2 = (s.SelectedTeam2 - 1 + NumTeams) % NumTeams }) world
            if World.isKeyboardKeyPressed KeyboardKey.Down world then
                game.Settings.Map
                    (fun s ->
                        if s.ActiveColumn = 0 then { s with SelectedTeam1 = (s.SelectedTeam1 + 1) % NumTeams }
                        else { s with SelectedTeam2 = (s.SelectedTeam2 + 1) % NumTeams }) world
            if World.isKeyboardKeyPressed KeyboardKey.F world then
                game.Settings.Map (fun s -> { s with FastHuman = not s.FastHuman }) world
            if World.isKeyboardKeyPressed KeyboardKey.H world then
                game.Settings.Map (fun s -> { s with HardMode = not s.HardMode }) world
            if World.isKeyboardKeyPressed KeyboardKey.Num5 world then
                game.Settings.Map (fun s -> { s with FivePlayer = not s.FivePlayer }) world
            if World.isKeyboardKeyPressed KeyboardKey.G world then
                game.Settings.Map (fun s -> { s with GamepadEnabled = not s.GamepadEnabled }) world

        // instructions (kept to two compact lines — the default window shows
        // roughly game-y 10..205, so these sit right under the action row)
        let instrLines =
            [| "P1: Arrows + RShift/Enter or Pad 1  |  P2: WASD + Space/Tab or Pad 2  |  hold shoot for a harder shot"
               "Pick HUMAN PLAYER for keyboard control  |  click a team or use UP/DOWN + TAB" |]
        for i in 0 .. instrLines.Length - 1 do
            World.doText $"Instr{i}"
                [Entity.Position .= Coords.screenPos 160.0f (200.0f + float32 i * 9.0f)
                 Entity.Size .= Coords.nuSize 400.0f 12.0f
                 Entity.Text .= instrLines[i]
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.gray
                 Entity.FontSizing .= Some 8.0f
                 Entity.Elevation .= 1.0f] world

        World.endGroup world
        World.endScreen world

        // ─── GAMEPLAY SCREEN ───────────────────────────────────────
        let gameplayActive = mode = HockeyPlaying || mode = HockeyLeaguePlaying
        World.beginScreen<GameplayDispatcher> Simulants.Gameplay.Name gameplayActive behavior [] world |> ignore
        World.endScreen world

        // handle the gameplay screen's requests (mode transitions + pure league updates)
        if gameplayActive && Simulants.Gameplay.GetSelected world then
            match Simulants.Gameplay.GetRequest world with
            | NoRequest -> ()
            | QuitRequested ->
                Simulants.Gameplay.SetRequest NoRequest world
                if mode = HockeyLeaguePlaying then game.SetLeague None world
                game.SetHockeyMode HockeyMenu world
            | ContinueRequested ->
                Simulants.Gameplay.SetRequest NoRequest world
                let m = Simulants.Gameplay.GetMatchState world
                if matchOver m then
                    if mode = HockeyLeaguePlaying then
                        match game.GetLeague world with
                        | Some league ->
                            // the whole league phase transition is one pure pipeline
                            let league =
                                league
                                |> recordMatchResult m.Team1Idx m.Team2Idx m.Team1Score m.Team2Score
                                |> simulateCpuRound league.CurrentRound
                                |> advanceRound
                            game.SetLeague (Some league) world
                            game.SetHockeyMode (if league.Finished then HockeyLeagueFinalStandings else HockeyLeagueStandings) world
                        | None -> game.SetHockeyMode HockeyMenu world
                    else
                        FsHockeyDispatcher.StartExhibition game world

        // ─── LEAGUE MATCHUP SCREEN ─────────────────────────────────
        World.beginScreen Simulants.Matchup.Name (mode = HockeyLeagueMatchup) behavior [] world |> ignore
        World.beginGroup "Gui" [] world
        Draw.background "MatchupBg" Colors.darkBg world
        (match game.GetLeague world with
         | Some league ->
            let t1, t2 = currentMatchup league
            World.doText "MatchupTitle"
                [Entity.Position .= v3 0.0f 120.0f 0.0f
                 Entity.Size .= v3 300.0f 32.0f 0.0f
                 Entity.Text .= "LEAGUE MODE"
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.goalFlash
                 Entity.Elevation .= 1.0f] world
            World.doText "MatchupRound"
                [Entity.Position .= v3 0.0f 70.0f 0.0f
                 Entity.Size .= v3 300.0f 32.0f 0.0f
                 Entity.Text @= $"ROUND {league.CurrentRound + 1} of {league.Schedule.Length}"
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.white
                 Entity.Elevation .= 1.0f] world
            World.doText "MatchupT1"
                [Entity.Position .= v3 0.0f 30.0f 0.0f
                 Entity.Size .= v3 300.0f 32.0f 0.0f
                 Entity.Text @= teamNames[t1]
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.team1
                 Entity.Elevation .= 1.0f] world
            World.doText "MatchupVs"
                [Entity.Position .= v3 0.0f 0.0f 0.0f
                 Entity.Text .= "vs"
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.gray
                 Entity.Elevation .= 1.0f] world
            World.doText "MatchupT2"
                [Entity.Position .= v3 0.0f -30.0f 0.0f
                 Entity.Size .= v3 300.0f 32.0f 0.0f
                 Entity.Text @= teamNames[t2]
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.team2
                 Entity.Elevation .= 1.0f] world

            if World.doButton "StartMatch"
                [Entity.Position .= v3 0.0f -100.0f 0.0f
                 Entity.Size .= v3 200.0f 40.0f 0.0f
                 Entity.Text .= "Start Match"
                 Entity.TextColor .= Colors.buttonText
                 Entity.Elevation .= 1.0f] world
               || World.isKeyboardKeyPressed KeyboardKey.Space world then
                FsHockeyDispatcher.StartLeagueMatch game league world

            if World.doButton "AbandonLeague"
                [Entity.Position .= v3 0.0f -150.0f 0.0f
                 Entity.Size .= v3 200.0f 32.0f 0.0f
                 Entity.Text .= "Abandon League"
                 Entity.TextColor .= Colors.buttonText
                 Entity.FontSizing .= Some 12.0f
                 Entity.Elevation .= 1.0f] world
               || World.isKeyboardKeyPressed KeyboardKey.Escape world then
                game.SetLeague None world
                game.SetHockeyMode HockeyMenu world
         | None -> ())
        World.endGroup world
        World.endScreen world

        // ─── STANDINGS SCREENS ─────────────────────────────────────
        let standingsActive = mode = HockeyLeagueStandings || mode = HockeyLeagueFinalStandings
        let isFinal = mode = HockeyLeagueFinalStandings
        World.beginScreen Simulants.Standings.Name standingsActive behavior [] world |> ignore
        World.beginGroup "Gui" [] world
        Draw.background "StandingsBg" Colors.darkBg world
        (match game.GetLeague world with
         | Some league ->
            // compact vertical layout: Nu's GUI space is 640x360, so
            // everything must stay within y = +-180
            World.doText "StandingsTitle"
                [Entity.Position .= v3 0.0f 160.0f 0.0f
                 Entity.Size .= v3 300.0f 28.0f 0.0f
                 Entity.Text @= (if isFinal then "FINAL STANDINGS" else "LEAGUE STANDINGS")
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.goalFlash
                 Entity.Elevation .= 1.0f] world
            World.doText "StandingsHeader"
                [Entity.Position .= v3 -200.0f 134.0f 0.0f
                 Entity.Size .= v3 500.0f 20.0f 0.0f
                 Entity.Text .= "   TEAM                 W   L   D  PTS  GF  GA"
                 Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                 Entity.TextColor .= Colors.gray
                 Entity.FontSizing .= Some 8.0f
                 Entity.Elevation .= 1.0f] world
            World.doStaticSprite "StandingsSep"
                [Entity.Position .= v3 0.0f 123.0f 0.0f
                 Entity.Size .= v3 450.0f 1.5f 0.0f
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.board
                 Entity.Elevation .= 1.0f] world |> ignore

            let standings = getSortedStandings league
            for rank in 0 .. standings.Length - 1 do
                let teamIdx, stats = standings[rank]
                let ry = 112.0f - float32 rank * 22.0f
                let isHuman = teamIdx = league.HumanTeam
                if isHuman then
                    World.doStaticSprite $"StHL{rank}"
                        [Entity.Position .= v3 0.0f ry 0.0f
                         Entity.Size .= v3 450.0f 20.0f 0.0f
                         Entity.StaticImage .= Assets.Default.White
                         Entity.Color .= color 0.118f 0.196f 0.314f 1.0f
                         Entity.Elevation .= 0.5f] world |> ignore
                let rowText =
                    $"{rank + 1,2}. {teamNames[teamIdx],-20} {stats.Wins,2}  {stats.Losses,2}  {stats.Draws,2}  {stats.Points,3}  {stats.GoalsFor,3}  {stats.GoalsAgainst,3}"
                World.doText $"StRow{rank}"
                    [Entity.Position .= v3 -200.0f ry 0.0f
                     Entity.Size .= v3 500.0f 20.0f 0.0f
                     Entity.Text @= rowText
                     Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                     Entity.TextColor @= (if isHuman then Colors.goalFlash else Colors.white)
                     Entity.FontSizing .= Some 8.0f
                     Entity.Elevation .= 1.0f] world

            if isFinal && standings.Length > 0 then
                let winnerIdx, winnerStats = standings[0]
                World.doText "StWinner"
                    [Entity.Position .= v3 -220.0f -130.0f 0.0f
                     Entity.Size .= v3 440.0f 28.0f 0.0f
                     Entity.Text @= $"{teamNames[winnerIdx]} WINS THE LEAGUE!  ({winnerStats.Points} pts)"
                     Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                     Entity.TextColor .= Colors.goalFlash
                     Entity.FontSizing .= Some 12.0f
                     Entity.Elevation .= 1.0f] world

            if World.doButton "StContinue"
                [Entity.Position .= v3 250.0f -140.0f 0.0f
                 Entity.Size .= v3 180.0f 36.0f 0.0f
                 Entity.Text @= (if isFinal then "Back to Menu" else "Next Round")
                 Entity.TextColor .= Colors.buttonText
                 Entity.Elevation .= 1.0f] world
               || World.isKeyboardKeyPressed KeyboardKey.Space world then
                if isFinal then
                    game.SetLeague None world
                    game.SetHockeyMode HockeyMenu world
                else
                    game.SetHockeyMode HockeyLeagueMatchup world

            if World.isKeyboardKeyPressed KeyboardKey.Escape world then
                game.SetLeague None world
                game.SetHockeyMode HockeyMenu world
         | None -> ())
        World.endGroup world
        World.endScreen world

        // when not in editor, handle the close-window button or Alt+F4
        if world.Unaccompanied then
            if  World.doSubscriptionAny "Exit" game.ExitRequestEvent world ||
                World.isKeyboardAltDown world && World.isKeyboardKeyDown KeyboardKey.F4 world then
                World.exit world
