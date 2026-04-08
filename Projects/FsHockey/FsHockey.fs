/// THE FS HOCKEY LEAGUE -- Nu Engine Port
/// Replaces WinForms Renderer.fs + Program.fs with Nu engine equivalents.
/// Physics.fs and Game.fs are reused unchanged.
namespace FsHockey
open System
open System.Numerics
open Prime
open Nu
open HockeyDemo.Physics
open HockeyDemo.Game

// ─── Application Mode (mirrors Program.fs AppMode) ────────────────────
[<Struct>]
type HockeyMode =
    | HockeyMenu
    | HockeyPlaying
    | HockeyLeagueMatchup
    | HockeyLeaguePlaying
    | HockeyLeagueStandings
    | HockeyLeagueFinalStandings

// ─── Game Property Extensions ─────────────────────────────────────────
[<AutoOpen>]
module FsHockeyExtensions =
    type Game with
        member this.GetHockeyMode world : HockeyMode = this.Get (nameof Game.HockeyMode) world
        member this.SetHockeyMode (value : HockeyMode) world = this.Set (nameof Game.HockeyMode) value world
        member this.HockeyMode = lens (nameof Game.HockeyMode) this this.GetHockeyMode this.SetHockeyMode

// ─── Module-level mutable state ───────────────────────────────────────
// FsHockey's GameState is heavily mutable and complex; storing it in Nu's
// property system would be impractical. We keep it at module level instead.
module AppData =
    let gs = createGameState ()
    let mutable selectedTeam1 = 0
    let mutable selectedTeam2 = 1
    let mutable activeColumn = 0
    let mutable fastHuman = true
    let mutable hardMode = false
    let mutable fivePlayerMode = false
    let mutable league : LeagueState option = None
    let mutable frameTick = 0 // counts Nu frames for animation

// ─── Coordinate Mapping ───────────────────────────────────────────────
// Game: top-left origin (0,0), X right, Y down, ~320x200 field + HUD
// Nu: center origin (0,0), X right, Y up
// Scale factor 1.5 gives ~480x372 Nu-unit game area
module Coords =
    let scale = 1.5f
    let gameW = 320.0f
    let gameH = 248.0f // 200 field + 48 HUD
    let halfW = gameW / 2.0f // 160
    let halfH = gameH / 2.0f // 124

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

// ─── Colors (CGA-inspired, matching Renderer.fs) ──────────────────────
module Colors =
    let ice = color 0.784f 0.863f 0.941f 1.0f
    let board = color 0.235f 0.314f 0.471f 1.0f
    let red = color 0.706f 0.157f 0.157f 1.0f
    let blue = color 0.157f 0.314f 0.706f 1.0f
    let team1 = color 0.863f 0.235f 0.235f 1.0f
    let team1Light = color 1.0f 0.471f 0.471f 1.0f
    let team2 = color 0.235f 0.392f 0.863f 1.0f
    let team2Light = color 0.471f 0.627f 1.0f 1.0f
    let puck = color 0.078f 0.078f 0.078f 1.0f
    let hudBg = color 0.078f 0.078f 0.157f 1.0f
    let hudText = color 0.863f 0.863f 0.863f 1.0f
    let goalFlash = color 1.0f 1.0f 0.314f 1.0f
    let white = color 1.0f 1.0f 1.0f 1.0f
    let black = color 0.0f 0.0f 0.0f 1.0f
    let gray = color 0.627f 0.627f 0.627f 1.0f
    let darkBg = color 0.039f 0.039f 0.118f 1.0f
    let skin = color 0.902f 0.765f 0.627f 1.0f
    let trousers = color 0.118f 0.118f 0.118f 1.0f
    let skate = color 0.314f 0.314f 0.314f 1.0f
    let stickBrown = color 0.545f 0.353f 0.169f 1.0f
    let goaliePad = color 0.902f 0.863f 0.784f 1.0f
    let goalieMask = color 0.863f 0.863f 0.863f 1.0f
    let helmetGold = color 0.784f 0.706f 0.157f 1.0f
    let helmetBlack = color 0.118f 0.118f 0.118f 1.0f
    let glove = color 0.235f 0.235f 0.235f 1.0f

// ─── Helper: set team speeds (from Program.fs) ────────────────────────
module Helpers =
    let setTeamSpeeds () =
        let gs = AppData.gs
        let ppt = gs.PlayersPerTeam

        let applyTeam teamIdx startEnt isFast isCpu =
            let srcIdx = if isFast then humanFastTeamIdx else teamIdx
            let speeds = teamMaxSpeed[srcIdx]
            let powers = teamShotPower[srcIdx]
            let mult = if isCpu && AppData.hardMode then HardModeSpeedMult else 1.0

            for i in 0 .. ppt - 1 do
                let ent = gs.Entities[startEnt + i]
                let statIdx =
                    if gs.FivePlayerMode then
                        match i with
                        | 0 -> 0
                        | i when i <= 2 -> min i 2
                        | _ -> 2
                    else
                        min i 2
                ent.MaxSpeed <- speeds[statIdx] * mult
                ent.ShotPower <- powers[statIdx] * mult
                ent.Accel <-
                    (if i = 0 && gs.FivePlayerMode then GoalieAccel else ForwardAccel) * mult
                if i = 0 && gs.FivePlayerMode then
                    ent.MaxSpeed <- min ent.MaxSpeed GoalieMaxSpeed

        let t1Human = gs.Team1Idx = 0
        let t2Human = gs.Team2Idx = 0
        applyTeam gs.Team1Idx 0 (AppData.fastHuman && t1Human) (not t1Human)
        applyTeam gs.Team2Idx gs.Team2Start (AppData.fastHuman && t2Human) (not t2Human)

    let startExhibitionMatch () =
        let gs = AppData.gs
        gs.Team1Idx <- AppData.selectedTeam1
        gs.Team2Idx <- AppData.selectedTeam2
        gs.NumPeriods <- ExhibitionPeriods
        setPlayerMode gs AppData.fivePlayerMode
        setTeamSpeeds ()
        initMatch gs

    let startLeagueMatch () =
        match AppData.league with
        | None -> ()
        | Some league ->
            let gs = AppData.gs
            let t1, t2 = currentMatchup league
            gs.Team1Idx <- t1
            gs.Team2Idx <- t2
            gs.NumPeriods <- LeaguePeriods
            setPlayerMode gs AppData.fivePlayerMode
            setTeamSpeeds ()
            initMatch gs

    let matchOver () =
        let gs = AppData.gs
        not gs.Playing && gs.ClockSeconds >= gs.PeriodLength

// ─── Game Dispatcher ──────────────────────────────────────────────────
type FsHockeyDispatcher () =
    inherit GameDispatcherImSim ()

    static member Properties =
        [define Game.HockeyMode HockeyMenu]

    override this.Process (game, world) =

        // single screen, single group
        World.beginScreen "Hockey" true Vanilla [] world |> ignore
        World.beginGroup "Main" [] world

        let mode = game.GetHockeyMode world

        match mode with
        | HockeyMenu -> FsHockeyDispatcher.RenderMenu (game, world)
        | HockeyPlaying -> FsHockeyDispatcher.RenderGame (game, world, false)
        | HockeyLeaguePlaying -> FsHockeyDispatcher.RenderGame (game, world, true)
        | HockeyLeagueMatchup -> FsHockeyDispatcher.RenderMatchup (game, world)
        | HockeyLeagueStandings -> FsHockeyDispatcher.RenderStandings (game, world, false)
        | HockeyLeagueFinalStandings -> FsHockeyDispatcher.RenderStandings (game, world, true)

        World.endGroup world
        World.endScreen world

        // handle Alt+F4 when not in editor
        if  World.isKeyboardAltDown world &&
            World.isKeyboardKeyDown KeyboardKey.F4 world &&
            world.Unaccompanied then
            World.exit world

    // ─── MENU SCREEN ──────────────────────────────────────────────
    static member RenderMenu (game : Game, world : World) =

        // Background
        World.doStaticSprite "MenuBg"
            [Entity.Position .= v3 0.0f 0.0f 0.0f
             Entity.Size .= v3 720.0f 558.0f 0.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.darkBg
             Entity.Elevation .= -1.0f] world |> ignore

        // Title
        World.doText "Title"
            [Entity.Position .= Coords.screenPos 160.0f 18.0f
             Entity.Size .= Coords.nuSize 300.0f 24.0f
             Entity.Text .= "THE FS HOCKEY LEAGUE"
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Colors.goalFlash
             Entity.Elevation .= 1.0f] world

        // Subtitle
        World.doText "Subtitle"
            [Entity.Position .= Coords.screenPos 160.0f 34.0f
             Entity.Size .= Coords.nuSize 300.0f 24.0f
             Entity.Text .= "Tuomas Hietanen, 2026"
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Colors.gray
             Entity.Elevation .= 1.0f] world

        // Team lists
        let listBaseY = 60.0f
        let rowH = 14.0f

        // Column 1 header
        World.doText "Col1Header"
            [Entity.Position .= Coords.screenPos 60.0f (listBaseY - 4.0f)
             Entity.Size .= Coords.nuSize 140.0f 20.0f
             Entity.Text .= "TEAM 1 (LEFT)"
             Entity.TextColor @= (if AppData.activeColumn = 0 then Colors.team1 else Colors.gray)
             Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
             Entity.Elevation .= 1.0f] world

        // Column 2 header
        World.doText "Col2Header"
            [Entity.Position .= Coords.screenPos 220.0f (listBaseY - 4.0f)
             Entity.Size .= Coords.nuSize 140.0f 20.0f
             Entity.Text .= "TEAM 2 (RIGHT)"
             Entity.TextColor @= (if AppData.activeColumn = 1 then Colors.team2 else Colors.gray)
             Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
             Entity.Elevation .= 1.0f] world

        // Team selection lists
        for i in 0 .. NumTeams - 1 do
            let ty = listBaseY + 12.0f + float32 i * rowH
            let prefix1 = if i = AppData.selectedTeam1 then "> " else "  "
            let prefix2 = if i = AppData.selectedTeam2 then "> " else "  "

            // Highlight bars for selected teams
            if i = AppData.selectedTeam1 then
                World.doStaticSprite $"Sel1Bg{i}"
                    [Entity.Position .= Coords.screenPos 55.0f (ty + 1.0f)
                     Entity.Size .= Coords.nuSize 100.0f (rowH - 1.0f)
                     Entity.StaticImage .= Assets.Default.White
                     Entity.Color .= color 0.157f 0.235f 0.392f 1.0f
                     Entity.Elevation .= 0.5f] world |> ignore

            if i = AppData.selectedTeam2 then
                World.doStaticSprite $"Sel2Bg{i}"
                    [Entity.Position .= Coords.screenPos 215.0f (ty + 1.0f)
                     Entity.Size .= Coords.nuSize 100.0f (rowH - 1.0f)
                     Entity.StaticImage .= Assets.Default.White
                     Entity.Color .= color 0.157f 0.235f 0.392f 1.0f
                     Entity.Elevation .= 0.5f] world |> ignore

            World.doText $"T1_{i}"
                [Entity.Position .= Coords.screenPos 60.0f ty
                 Entity.Size .= Coords.nuSize 140.0f 16.0f
                 Entity.Text @= $"{prefix1}{teamNames[i]}"
                 Entity.TextColor @= (if i = AppData.selectedTeam1 then Colors.goalFlash else Colors.white)
                 Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                 Entity.Elevation .= 1.0f] world

            World.doText $"T2_{i}"
                [Entity.Position .= Coords.screenPos 220.0f ty
                 Entity.Size .= Coords.nuSize 140.0f 16.0f
                 Entity.Text @= $"{prefix2}{teamNames[i]}"
                 Entity.TextColor @= (if i = AppData.selectedTeam2 then Colors.goalFlash else Colors.white)
                 Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                 Entity.Elevation .= 1.0f] world

        // Instructions
        let fastStr = if AppData.fastHuman then "ON" else "OFF"
        let hardStr = if AppData.hardMode then "ON" else "OFF"
        let fiveStr = if AppData.fivePlayerMode then "6v6" else "3v3"
        let instrY = 210.0f

        let instrLines =
            [| "UP/DOWN = Select Team  |  TAB = Switch Column"
               "ENTER = Start Game  |  L = Play League  |  ESC = Quit"
               $"F = Fast Human [{fastStr}]  |  H = Hard Mode [{hardStr}]  |  5 = Players [{fiveStr}]"
               "Hold shoot key longer for harder shot, quick tap for a pass"
               "Player 1: Arrow Keys + RShift/Enter to shoot"
               "Player 2: WASD + Space/Tab to shoot"
               "(Set team to HUMAN PLAYER for keyboard control)" |]

        for i in 0 .. instrLines.Length - 1 do
            World.doText $"Instr{i}"
                [Entity.Position .= Coords.screenPos 160.0f (instrY + float32 i * 10.0f)
                 Entity.Size .= Coords.nuSize 320.0f 14.0f
                 Entity.Text @= instrLines[i]
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.gray
                 Entity.FontSizing .= Some 8.0f
                 Entity.Elevation .= 1.0f] world

        // ─── Menu Input ───
        if world.Advancing then
            if World.isKeyboardKeyPressed KeyboardKey.Tab world then
                AppData.activeColumn <- 1 - AppData.activeColumn

            if World.isKeyboardKeyPressed KeyboardKey.Up world then
                if AppData.activeColumn = 0 then
                    AppData.selectedTeam1 <- (AppData.selectedTeam1 - 1 + NumTeams) % NumTeams
                else
                    AppData.selectedTeam2 <- (AppData.selectedTeam2 - 1 + NumTeams) % NumTeams

            if World.isKeyboardKeyPressed KeyboardKey.Down world then
                if AppData.activeColumn = 0 then
                    AppData.selectedTeam1 <- (AppData.selectedTeam1 + 1) % NumTeams
                else
                    AppData.selectedTeam2 <- (AppData.selectedTeam2 + 1) % NumTeams

            if World.isKeyboardKeyPressed KeyboardKey.Enter world then
                Helpers.startExhibitionMatch ()
                game.SetHockeyMode HockeyPlaying world

            if World.isKeyboardKeyPressed KeyboardKey.L world then
                AppData.league <- Some (createLeagueState AppData.selectedTeam1)
                game.SetHockeyMode HockeyLeagueMatchup world

            if World.isKeyboardKeyPressed KeyboardKey.F world then
                AppData.fastHuman <- not AppData.fastHuman

            if World.isKeyboardKeyPressed KeyboardKey.H world then
                AppData.hardMode <- not AppData.hardMode

            if World.isKeyboardKeyPressed KeyboardKey.Num5 world then
                AppData.fivePlayerMode <- not AppData.fivePlayerMode

            if World.isKeyboardKeyPressed KeyboardKey.Escape world && world.Unaccompanied then
                World.exit world

    // ─── GAME RENDERING (Exhibition + League) ─────────────────────
    static member RenderGame (game : Game, world : World, leagueMode : bool) =
        let gs = AppData.gs

        // ─── Update Input ───
        if world.Advancing then
            // Player 1: Arrow keys + RShift/Enter
            gs.KeyLeft1 <- World.isKeyboardKeyDown KeyboardKey.Left world
            gs.KeyRight1 <- World.isKeyboardKeyDown KeyboardKey.Right world
            gs.KeyUp1 <- World.isKeyboardKeyDown KeyboardKey.Up world
            gs.KeyDown1 <- World.isKeyboardKeyDown KeyboardKey.Down world
            gs.KeyFire1 <- World.isKeyboardKeyDown KeyboardKey.RShift world || World.isKeyboardKeyDown KeyboardKey.Enter world

            // Player 2: WASD + Space/Tab (only in exhibition)
            if not leagueMode then
                gs.KeyLeft2 <- World.isKeyboardKeyDown KeyboardKey.A world
                gs.KeyRight2 <- World.isKeyboardKeyDown KeyboardKey.D world
                gs.KeyUp2 <- World.isKeyboardKeyDown KeyboardKey.W world
                gs.KeyDown2 <- World.isKeyboardKeyDown KeyboardKey.S world
                gs.KeyFire2 <- World.isKeyboardKeyDown KeyboardKey.Space world || World.isKeyboardKeyDown KeyboardKey.Tab world

            // Run physics ticks (1 per frame for Nu's ~60fps, not PhysicsTicksPerFrame=2 which was for 30fps WinForms)
            gameTick gs

            gs.BallAnimFrame <- (gs.BallAnimFrame + 1) % (BallAnimFrames * 2) // Note: BallAnimFrame is not read by any renderer
            AppData.frameTick <- AppData.frameTick + 1

            // Handle game-over transitions
            if Helpers.matchOver () then
                if World.isKeyboardKeyPressed KeyboardKey.Space world then
                    if leagueMode then
                        match AppData.league with
                        | Some league ->
                            recordMatchResult league gs.Team1Idx gs.Team2Idx gs.Team1Score gs.Team2Score
                            simulateCpuRound league league.CurrentRound
                            let finished = advanceRound league
                            game.SetHockeyMode (if finished then HockeyLeagueFinalStandings else HockeyLeagueStandings) world
                        | None -> game.SetHockeyMode HockeyMenu world
                    else
                        initMatch gs

            if World.isKeyboardKeyPressed KeyboardKey.Escape world then
                if leagueMode then
                    AppData.league <- None
                game.SetHockeyMode HockeyMenu world

        // ─── Draw Background ───
        World.doStaticSprite "GameBg"
            [Entity.Position .= v3 0.0f 0.0f 0.0f
             Entity.Size .= v3 720.0f 558.0f 0.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= color 0.118f 0.118f 0.196f 1.0f
             Entity.Elevation .= -1.0f] world |> ignore

        // ─── Draw Rink ───
        FsHockeyDispatcher.DrawRink world

        // ─── Draw Trail Marks ───
        for i in 0 .. gs.TrailMarkCount - 1 do
            let mark = gs.TrailMarks[i]
            if mark.Life > 0<tick> then
                let alpha = float32 (int mark.Life) / float32 (int TrailMarkLifetime) * 0.7f + 0.15f
                let alpha = min 0.85f alpha
                World.doStaticSprite $"Trail{i}"
                    [Entity.Position @= Coords.nuPos mark.X mark.Y
                     Entity.Size .= Coords.nuSize 2.5f 2.5f
                     Entity.StaticImage .= Assets.Default.White
                     Entity.Color @= color 1.0f 1.0f 1.0f alpha
                     Entity.Elevation .= 0.1f] world |> ignore

        // ─── Draw Puck ───
        let ball = gs.Entities[gs.BallIdx]
        World.doStaticSprite "Puck"
            [Entity.Position @= Coords.nuPos ball.X ball.Y
             Entity.Size .= Coords.nuSize 5.0f 5.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.puck
             Entity.Elevation .= 0.5f] world |> ignore

        // Puck highlight
        World.doStaticSprite "PuckHL"
            [Entity.Position @= Coords.nuPos ball.X ball.Y + v3 0.0f 0.5f 0.0f
             Entity.Size .= Coords.nuSize 2.0f 2.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= color 0.235f 0.235f 0.235f 1.0f
             Entity.Elevation .= 0.6f] world |> ignore

        // ─── Draw Players ───
        let ppt = gs.PlayersPerTeam
        let t2s = gs.Team2Start
        let t1Helmet = if gs.Team1Idx = 0 then Colors.helmetGold else Colors.helmetBlack
        let t2Helmet = if gs.Team2Idx = 0 then Colors.helmetGold else Colors.helmetBlack

        for i in 0 .. ppt - 1 do
            let isGoalie = gs.FivePlayerMode && i = 0
            let isActive = (i = gs.ActivePlayer1)
            FsHockeyDispatcher.DrawPlayer ($"P1_{i}", gs.Entities[i], Colors.team1, t1Helmet, isActive, isGoalie, gs.StickAnimTimers[i], world)

        for i in 0 .. ppt - 1 do
            let ei = t2s + i
            let isGoalie = gs.FivePlayerMode && i = 0
            let isActive = (ei = gs.ActivePlayer2)
            FsHockeyDispatcher.DrawPlayer ($"P2_{i}", gs.Entities[ei], Colors.team2, t2Helmet, isActive, isGoalie, gs.StickAnimTimers[ei], world)

        // ─── Draw HUD ───
        FsHockeyDispatcher.DrawHud (gs, world)

        // ─── Goal Flash Overlay ───
        if gs.GoalFlashTimer > 0<tick> then
            let alpha = if int gs.GoalFlashTimer % 10 < 5 then 0.3f else 0.12f
            World.doStaticSprite "GoalFlash"
                [Entity.Position .= v3 0.0f 0.0f 0.0f
                 Entity.Size .= v3 720.0f 558.0f 0.0f
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color @= color 1.0f 1.0f 0.314f alpha
                 Entity.Elevation .= 5.0f] world |> ignore

            let scorerName =
                match gs.GoalScoredBy with
                | Team1Scored -> teamNames[gs.Team1Idx]
                | Team2Scored -> teamNames[gs.Team2Idx]
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
                 Entity.Text @= $"{gs.Team1Score} - {gs.Team2Score}"
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.white
                 Entity.Elevation .= 6.0f] world

        // ─── Game Over Overlay ───
        if Helpers.matchOver () then
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

            let scoreStr = $"{teamNames[gs.Team1Idx]}  {gs.Team1Score}  -  {gs.Team2Score}  {teamNames[gs.Team2Idx]}"
            World.doText "GameOverScore"
                [Entity.Position .= v3 0.0f 40.0f 0.0f
                 Entity.Size .= v3 500.0f 32.0f 0.0f
                 Entity.Text @= scoreStr
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.white
                 Entity.Elevation .= 8.0f] world

            let winner =
                if gs.Team1Score > gs.Team2Score then $"{teamNames[gs.Team1Idx]} WINS!"
                elif gs.Team2Score > gs.Team1Score then $"{teamNames[gs.Team2Idx]} WINS!"
                else "IT'S A TIE!"

            World.doText "GameOverWinner"
                [Entity.Position .= v3 0.0f 10.0f 0.0f
                 Entity.Size .= v3 400.0f 32.0f 0.0f
                 Entity.Text @= winner
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.goalFlash
                 Entity.Elevation .= 8.0f] world

            let instrStr =
                if leagueMode then "Press SPACE for standings"
                else "Press SPACE to play again  |  ESC to quit"

            World.doText "GameOverInstr"
                [Entity.Position .= v3 0.0f -60.0f 0.0f
                 Entity.Size .= v3 500.0f 32.0f 0.0f
                 Entity.Text @= instrStr
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.gray
                 Entity.Elevation .= 8.0f] world

    // ─── Draw Rink ────────────────────────────────────────────────
    static member DrawRink (world : World) =

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

        // Board outline (4 edges)
        let fl = float32 (stripPx FieldLeft)
        let fr = float32 (stripPx FieldRight)
        let ft = float32 (stripPx FieldTop)
        let fb = float32 (stripPx FieldBottom)
        let boardW = 3.0f

        // Top board
        World.doStaticSprite "BoardTop"
            [Entity.Position .= Coords.screenPos ((fl + fr) / 2.0f) (ft - boardW / 2.0f)
             Entity.Size .= Coords.nuSize (fr - fl + boardW * 2.0f) boardW
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.board
             Entity.Elevation .= 0.2f] world |> ignore

        // Bottom board
        World.doStaticSprite "BoardBot"
            [Entity.Position .= Coords.screenPos ((fl + fr) / 2.0f) (fb + boardW / 2.0f)
             Entity.Size .= Coords.nuSize (fr - fl + boardW * 2.0f) boardW
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.board
             Entity.Elevation .= 0.2f] world |> ignore

        // Left board
        World.doStaticSprite "BoardLeft"
            [Entity.Position .= Coords.screenPos (fl - boardW / 2.0f) ((ft + fb) / 2.0f)
             Entity.Size .= Coords.nuSize boardW (fb - ft + boardW * 2.0f)
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.board
             Entity.Elevation .= 0.2f] world |> ignore

        // Right board
        World.doStaticSprite "BoardRight"
            [Entity.Position .= Coords.screenPos (fr + boardW / 2.0f) ((ft + fb) / 2.0f)
             Entity.Size .= Coords.nuSize boardW (fb - ft + boardW * 2.0f)
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.board
             Entity.Elevation .= 0.2f] world |> ignore

        // Goal nets
        let gt = float32 (stripPx GoalTop)
        let gb = float32 (stripPx GoalBottom)
        let gd = float32 (stripPx GoalDepth)

        // Left goal (team1 color)
        World.doStaticSprite "GoalLeft"
            [Entity.Position .= Coords.screenPos (fl - gd / 2.0f) ((gt + gb) / 2.0f)
             Entity.Size .= Coords.nuSize gd (gb - gt)
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= color 0.863f 0.235f 0.235f 0.3f
             Entity.Elevation .= 0.1f] world |> ignore

        // Right goal (team2 color)
        World.doStaticSprite "GoalRight"
            [Entity.Position .= Coords.screenPos (fr + gd / 2.0f) ((gt + gb) / 2.0f)
             Entity.Size .= Coords.nuSize gd (gb - gt)
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= color 0.235f 0.392f 0.863f 0.3f
             Entity.Elevation .= 0.1f] world |> ignore

        // Center line (red)
        let cx = float32 (stripPx CenterX)
        World.doStaticSprite "CenterLine"
            [Entity.Position .= Coords.screenPos cx ((ft + fb) / 2.0f)
             Entity.Size .= Coords.nuSize 1.5f (fb - ft)
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.red
             Entity.Elevation .= 0.15f] world |> ignore

        // Center dot
        World.doStaticSprite "CenterDot"
            [Entity.Position .= Coords.screenPos cx (float32 (stripPx CenterY))
             Entity.Size .= Coords.nuSize 5.0f 5.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.red
             Entity.Elevation .= 0.15f] world |> ignore

        // Blue lines
        let fieldW = fr - fl
        let bl1x = fl + fieldW / 3.0f
        let bl2x = fl + fieldW * 2.0f / 3.0f

        World.doStaticSprite "BlueLine1"
            [Entity.Position .= Coords.screenPos bl1x ((ft + fb) / 2.0f)
             Entity.Size .= Coords.nuSize 2.0f (fb - ft)
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.blue
             Entity.Elevation .= 0.15f] world |> ignore

        World.doStaticSprite "BlueLine2"
            [Entity.Position .= Coords.screenPos bl2x ((ft + fb) / 2.0f)
             Entity.Size .= Coords.nuSize 2.0f (fb - ft)
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.blue
             Entity.Elevation .= 0.15f] world |> ignore

        // Goal lines (red, thinner)
        World.doStaticSprite "GoalLine1"
            [Entity.Position .= Coords.screenPos (float32 (stripPx GoalLeftX)) ((ft + fb) / 2.0f)
             Entity.Size .= Coords.nuSize 1.0f (fb - ft)
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= color 0.706f 0.157f 0.157f 0.5f
             Entity.Elevation .= 0.15f] world |> ignore

        World.doStaticSprite "GoalLine2"
            [Entity.Position .= Coords.screenPos (float32 (stripPx GoalRightX)) ((ft + fb) / 2.0f)
             Entity.Size .= Coords.nuSize 1.0f (fb - ft)
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= color 0.706f 0.157f 0.157f 0.5f
             Entity.Elevation .= 0.15f] world |> ignore

    // ─── Draw Player ──────────────────────────────────────────────
    static member DrawPlayer (name : string, ent : Entity, teamColor : Color, helmetColor : Color, isActive : bool, isGoalie : bool, stickAnim : int, world : World) =
        let u = 1.2f  // base unit for proportions
        let s = Coords.scale
        let us = u * s  // offset unit in Nu world coords
        let basePos = Coords.nuPos ent.X ent.Y

        // Facing angle: rotate from default "up" (+Y in Nu) to entity direction
        // Game uses Y-down, so nuDirY = -dirY. Angle from +Y: atan2(-dirX, -dirY)
        let angleRad =
            if ent.DirX <> 0.0 || ent.DirY <> 0.0 then
                atan2 (float32 -ent.DirX) (float32 -ent.DirY)
            else 0.0f
        let rot = v3 0.0f 0.0f (angleRad * (180.0f / MathF.PI))
        let cosA = cos angleRad
        let sinA = sin angleRad

        /// Compute position with rotated offset from player center.
        /// (ox, oy) are offsets in Nu world units; +Y = forward (up when angle=0).
        let rpos ox oy =
            basePos + v3 (ox * cosA - oy * sinA) (ox * sinA + oy * cosA) 0.0f

        // Skating leg animation (half speed vs MonoGame since 60fps vs 30fps)
        let speedSq = float (ent.VelX * ent.VelX + ent.VelY * ent.VelY)
        let legOff =
            if speedSq > 16.0 then
                sin (float32 AppData.frameTick * 0.04f) * 0.36f * us
            else 0.0f

        // ─── Helmet ───
        World.doStaticSprite $"{name}Hlm"
            [Entity.Position @= rpos 0.0f (4.5f * us)
             Entity.Size .= Coords.nuSize (3.0f * u) (2.0f * u)
             Entity.Degrees @= rot
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= helmetColor
             Entity.Elevation .= 1.15f] world |> ignore

        // ─── Face (skin below helmet) ───
        World.doStaticSprite $"{name}Fce"
            [Entity.Position @= rpos 0.0f (3.0f * us)
             Entity.Size .= Coords.nuSize (2.0f * u) (1.0f * u)
             Entity.Degrees @= rot
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.skin
             Entity.Elevation .= 1.1f] world |> ignore

        // ─── Goalie mask ───
        if isGoalie then
            World.doStaticSprite $"{name}Msk"
                [Entity.Position @= rpos (0.5f * us) (3.75f * us)
                 Entity.Size .= Coords.nuSize (1.2f * u) (1.5f * u)
                 Entity.Degrees @= rot
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.goalieMask
                 Entity.Elevation .= 1.12f] world |> ignore

        // ─── Shoulders ───
        World.doStaticSprite $"{name}Shd"
            [Entity.Position @= rpos 0.0f (1.75f * us)
             Entity.Size .= Coords.nuSize (7.0f * u) (1.5f * u)
             Entity.Degrees @= rot
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= teamColor
             Entity.Elevation .= 1.0f] world |> ignore

        // ─── Torso (jersey) ───
        World.doStaticSprite $"{name}Bdy"
            [Entity.Position @= rpos 0.0f (-0.25f * us)
             Entity.Size .= Coords.nuSize (6.0f * u) (2.5f * u)
             Entity.Degrees @= rot
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= teamColor
             Entity.Elevation .= 1.0f] world |> ignore

        // ─── Jersey stripe ───
        World.doStaticSprite $"{name}Str"
            [Entity.Position @= rpos 0.0f (0.2f * us)
             Entity.Size .= Coords.nuSize (6.0f * u) (0.6f * u)
             Entity.Degrees @= rot
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= color 1.0f 1.0f 1.0f 0.3f
             Entity.Elevation .= 1.05f] world |> ignore

        // ─── Left arm ───
        World.doStaticSprite $"{name}LA"
            [Entity.Position @= rpos (-3.4f * us) (0.5f * us)
             Entity.Size .= Coords.nuSize (1.2f * u) (2.8f * u)
             Entity.Degrees @= rot
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= teamColor
             Entity.Elevation .= 1.0f] world |> ignore

        // ─── Left glove ───
        World.doStaticSprite $"{name}LG"
            [Entity.Position @= rpos (-3.4f * us) (-0.8f * us)
             Entity.Size .= Coords.nuSize (1.2f * u) (0.8f * u)
             Entity.Degrees @= rot
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.glove
             Entity.Elevation .= 1.02f] world |> ignore

        // ─── Right arm ───
        World.doStaticSprite $"{name}RA"
            [Entity.Position @= rpos (3.4f * us) (0.5f * us)
             Entity.Size .= Coords.nuSize (1.2f * u) (2.8f * u)
             Entity.Degrees @= rot
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= teamColor
             Entity.Elevation .= 1.0f] world |> ignore

        // ─── Right glove ───
        World.doStaticSprite $"{name}RG"
            [Entity.Position @= rpos (3.4f * us) (-0.8f * us)
             Entity.Size .= Coords.nuSize (1.2f * u) (0.8f * u)
             Entity.Degrees @= rot
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.glove
             Entity.Elevation .= 1.02f] world |> ignore

        // ─── Hips ───
        let legColor = if isGoalie then Colors.goaliePad else Colors.trousers
        let hipW = if isGoalie then 7.0f else 6.0f
        World.doStaticSprite $"{name}Hip"
            [Entity.Position @= rpos 0.0f (-2.1f * us)
             Entity.Size .= Coords.nuSize (hipW * u) (1.2f * u)
             Entity.Degrees @= rot
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= legColor
             Entity.Elevation .= 1.0f] world |> ignore

        // ─── Legs / Goalie pads ───
        if isGoalie then
            World.doStaticSprite $"{name}LPd"
                [Entity.Position @= rpos (-1.5f * us) (-3.7f * us)
                 Entity.Size .= Coords.nuSize (3.0f * u) (2.0f * u)
                 Entity.Degrees @= rot
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.goaliePad
                 Entity.Elevation .= 1.0f] world |> ignore
            World.doStaticSprite $"{name}RPd"
                [Entity.Position @= rpos (1.5f * us) (-3.7f * us)
                 Entity.Size .= Coords.nuSize (3.0f * u) (2.0f * u)
                 Entity.Degrees @= rot
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.goaliePad
                 Entity.Elevation .= 1.0f] world |> ignore
        else
            World.doStaticSprite $"{name}LL"
                [Entity.Position @= rpos (-1.4f * us) (-3.2f * us - legOff)
                 Entity.Size .= Coords.nuSize (2.2f * u) (1.0f * u)
                 Entity.Degrees @= rot
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.trousers
                 Entity.Elevation .= 1.0f] world |> ignore
            World.doStaticSprite $"{name}RL"
                [Entity.Position @= rpos (1.4f * us) (-3.2f * us + legOff)
                 Entity.Size .= Coords.nuSize (2.2f * u) (1.0f * u)
                 Entity.Degrees @= rot
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.trousers
                 Entity.Elevation .= 1.0f] world |> ignore

        // ─── Skate blades ───
        if isGoalie then
            World.doStaticSprite $"{name}LSk"
                [Entity.Position @= rpos (-1.25f * us) (-4.8f * us)
                 Entity.Size .= Coords.nuSize (1.5f * u) (0.4f * u)
                 Entity.Degrees @= rot
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.skate
                 Entity.Elevation .= 0.95f] world |> ignore
            World.doStaticSprite $"{name}RSk"
                [Entity.Position @= rpos (1.25f * us) (-4.8f * us)
                 Entity.Size .= Coords.nuSize (1.5f * u) (0.4f * u)
                 Entity.Degrees @= rot
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.skate
                 Entity.Elevation .= 0.95f] world |> ignore
        else
            World.doStaticSprite $"{name}LSk"
                [Entity.Position @= rpos (-1.15f * us) (-4.0f * us - legOff)
                 Entity.Size .= Coords.nuSize (1.7f * u) (0.4f * u)
                 Entity.Degrees @= rot
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.skate
                 Entity.Elevation .= 0.95f] world |> ignore
            World.doStaticSprite $"{name}RSk"
                [Entity.Position @= rpos (1.15f * us) (-4.0f * us + legOff)
                 Entity.Size .= Coords.nuSize (1.7f * u) (0.4f * u)
                 Entity.Degrees @= rot
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.skate
                 Entity.Elevation .= 0.95f] world |> ignore

        // ─── Stick ───
        let wobble = if stickAnim > 0 then sin (float32 stickAnim * 1.5f) * 2.5f else 0.0f
        World.doStaticSprite $"{name}Stk"
            [Entity.Position @= rpos (2.5f * us) ((3.5f + wobble * 0.3f) * us)
             Entity.Size .= Coords.nuSize (1.2f * u) (7.0f * u)
             Entity.Degrees @= rot
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.stickBrown
             Entity.Elevation .= 0.9f] world |> ignore

        // ─── Active player marker (NOT rotated, always above player) ───
        if isActive then
            World.doStaticSprite $"{name}Mrk"
                [Entity.Position @= basePos + v3 0.0f (8.0f * us) 0.0f
                 Entity.Size .= Coords.nuSize (3.0f * u) (2.0f * u)
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.white
                 Entity.Elevation .= 1.2f] world |> ignore

    // ─── Draw HUD ─────────────────────────────────────────────────
    static member DrawHud (gs : GameState, world : World) =
        // HUD background below the rink
        let hudY = float32 (stripPx FieldBottom) + 10.0f

        World.doStaticSprite "HudBg"
            [Entity.Position .= Coords.screenPos 160.0f (hudY + 20.0f)
             Entity.Size .= Coords.nuSize 320.0f 44.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.hudBg
             Entity.Elevation .= 2.0f] world |> ignore

        // Team 1 name + score (left)
        World.doText "HudT1Name"
            [Entity.Position .= Coords.screenPos 30.0f (hudY + 8.0f)
             Entity.Size .= Coords.nuSize 100.0f 14.0f
             Entity.Text @= teamNames[gs.Team1Idx]
             Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
             Entity.TextColor .= Colors.team1
             Entity.FontSizing .= Some 8.0f
             Entity.Elevation .= 3.0f] world

        World.doText "HudT1Score"
            [Entity.Position .= Coords.screenPos 30.0f (hudY + 22.0f)
             Entity.Size .= Coords.nuSize 60.0f 20.0f
             Entity.Text @= $"{gs.Team1Score}"
             Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
             Entity.TextColor .= Colors.team1
             Entity.Elevation .= 3.0f] world

        // Team 2 name + score (right)
        World.doText "HudT2Name"
            [Entity.Position .= Coords.screenPos 280.0f (hudY + 8.0f)
             Entity.Size .= Coords.nuSize 100.0f 14.0f
             Entity.Text @= teamNames[gs.Team2Idx]
             Entity.Justification .= Justified (JustifyRight, JustifyMiddle)
             Entity.TextColor .= Colors.team2
             Entity.FontSizing .= Some 8.0f
             Entity.Elevation .= 3.0f] world

        World.doText "HudT2Score"
            [Entity.Position .= Coords.screenPos 280.0f (hudY + 22.0f)
             Entity.Size .= Coords.nuSize 60.0f 20.0f
             Entity.Text @= $"{gs.Team2Score}"
             Entity.Justification .= Justified (JustifyRight, JustifyMiddle)
             Entity.TextColor .= Colors.team2
             Entity.Elevation .= 3.0f] world

        // Clock (center)
        let secs = int gs.ClockSeconds
        let mins = secs / 60
        let secR = secs % 60
        let clockStr = $"{mins}:{secR:D2}"
        World.doText "HudClock"
            [Entity.Position .= Coords.screenPos 160.0f (hudY + 8.0f)
             Entity.Size .= Coords.nuSize 80.0f 20.0f
             Entity.Text @= clockStr
             Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
             Entity.TextColor .= Colors.hudText
             Entity.Elevation .= 3.0f] world

        // Period info
        if gs.NumPeriods > 1 then
            World.doText "HudPeriod"
                [Entity.Position .= Coords.screenPos 160.0f (hudY + 22.0f)
                 Entity.Size .= Coords.nuSize 120.0f 14.0f
                 Entity.Text @= $"PERIOD {gs.CurrentPeriod + 1} of {gs.NumPeriods}"
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.hudText
                 Entity.FontSizing .= Some 8.0f
                 Entity.Elevation .= 3.0f] world

    // ─── LEAGUE MATCHUP SCREEN ────────────────────────────────────
    static member RenderMatchup (game : Game, world : World) =
        // Background
        World.doStaticSprite "MatchupBg"
            [Entity.Position .= v3 0.0f 0.0f 0.0f
             Entity.Size .= v3 720.0f 558.0f 0.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.darkBg
             Entity.Elevation .= -1.0f] world |> ignore

        match AppData.league with
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

            World.doText "MatchupInstr"
                [Entity.Position .= v3 0.0f -100.0f 0.0f
                 Entity.Size .= v3 400.0f 32.0f 0.0f
                 Entity.Text .= "Press SPACE to start match"
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.gray
                 Entity.Elevation .= 1.0f] world

        | None -> ()

        // Input
        if world.Advancing then
            if World.isKeyboardKeyPressed KeyboardKey.Space world then
                Helpers.startLeagueMatch ()
                game.SetHockeyMode HockeyLeaguePlaying world

            if World.isKeyboardKeyPressed KeyboardKey.Escape world then
                AppData.league <- None
                game.SetHockeyMode HockeyMenu world

    // ─── STANDINGS SCREEN ─────────────────────────────────────────
    static member RenderStandings (game : Game, world : World, isFinal : bool) =
        // Background
        World.doStaticSprite "StandingsBg"
            [Entity.Position .= v3 0.0f 0.0f 0.0f
             Entity.Size .= v3 720.0f 558.0f 0.0f
             Entity.StaticImage .= Assets.Default.White
             Entity.Color .= Colors.darkBg
             Entity.Elevation .= -1.0f] world |> ignore

        match AppData.league with
        | Some league ->
            let title = if isFinal then "FINAL STANDINGS" else "LEAGUE STANDINGS"
            World.doText "StandingsTitle"
                [Entity.Position .= v3 0.0f 200.0f 0.0f
                 Entity.Size .= v3 300.0f 32.0f 0.0f
                 Entity.Text @= title
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.goalFlash
                 Entity.Elevation .= 1.0f] world

            // Column header
            World.doText "StandingsHeader"
                [Entity.Position .= v3 -200.0f 165.0f 0.0f
                 Entity.Size .= v3 500.0f 24.0f 0.0f
                 Entity.Text .= "   TEAM                 W   L   D  PTS  GF  GA"
                 Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                 Entity.TextColor .= Colors.gray
                 Entity.FontSizing .= Some 8.0f
                 Entity.Elevation .= 1.0f] world

            // Separator line
            World.doStaticSprite "StandingsSep"
                [Entity.Position .= v3 0.0f 152.0f 0.0f
                 Entity.Size .= v3 450.0f 1.5f 0.0f
                 Entity.StaticImage .= Assets.Default.White
                 Entity.Color .= Colors.board
                 Entity.Elevation .= 1.0f] world |> ignore

            let standings = getSortedStandings league
            let rowH = 28.0f

            for rank in 0 .. standings.Length - 1 do
                let teamIdx, stats = standings[rank]
                let ry = 140.0f - float32 rank * rowH
                let isHuman = (teamIdx = league.HumanTeam)

                if isHuman then
                    World.doStaticSprite $"StHL{rank}"
                        [Entity.Position .= v3 0.0f ry 0.0f
                         Entity.Size .= v3 450.0f (rowH - 2.0f) 0.0f
                         Entity.StaticImage .= Assets.Default.White
                         Entity.Color .= color 0.118f 0.196f 0.314f 1.0f
                         Entity.Elevation .= 0.5f] world |> ignore

                let nameStr = teamNames[teamIdx]
                let rowText = $"{rank + 1,2}. {nameStr,-20} {stats.Wins,2}  {stats.Losses,2}  {stats.Draws,2}  {stats.Points,3}  {stats.GoalsFor,3}  {stats.GoalsAgainst,3}"

                World.doText $"StRow{rank}"
                    [Entity.Position .= v3 -200.0f ry 0.0f
                     Entity.Size .= v3 500.0f 24.0f 0.0f
                     Entity.Text @= rowText
                     Entity.Justification .= Justified (JustifyLeft, JustifyMiddle)
                     Entity.TextColor @= (if isHuman then Colors.goalFlash else Colors.white)
                     Entity.FontSizing .= Some 8.0f
                     Entity.Elevation .= 1.0f] world

            // Winner announcement for final
            if isFinal && standings.Length > 0 then
                let winnerIdx, winnerStats = standings[0]
                World.doText "StWinner"
                    [Entity.Position .= v3 0.0f -160.0f 0.0f
                     Entity.Size .= v3 500.0f 32.0f 0.0f
                     Entity.Text @= $"{teamNames[winnerIdx]} WINS THE LEAGUE!  ({winnerStats.Points} pts)"
                     Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                     Entity.TextColor .= Colors.goalFlash
                     Entity.Elevation .= 1.0f] world

            let instrStr =
                if isFinal then "Press SPACE to return to menu"
                else "Press SPACE to continue"

            World.doText "StInstr"
                [Entity.Position .= v3 0.0f -200.0f 0.0f
                 Entity.Size .= v3 400.0f 32.0f 0.0f
                 Entity.Text @= instrStr
                 Entity.Justification .= Justified (JustifyCenter, JustifyMiddle)
                 Entity.TextColor .= Colors.gray
                 Entity.Elevation .= 1.0f] world

        | None -> ()

        // Input
        if world.Advancing then
            if World.isKeyboardKeyPressed KeyboardKey.Space world then
                if isFinal then
                    AppData.league <- None
                    game.SetHockeyMode HockeyMenu world
                else
                    game.SetHockeyMode HockeyLeagueMatchup world

            if World.isKeyboardKeyPressed KeyboardKey.Escape world then
                AppData.league <- None
                game.SetHockeyMode HockeyMenu world
