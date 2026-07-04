/// THE FS HOCKEY LEAGUE — Game Logic
/// Entity update, AI, human input, collision, ball physics, scoring.
/// Taking influence from Solar Hockey by Galifir Developments (Harm Hanemaayer & John Remyn, 1990-1992)
module HockeyDemo.Game

open System
open HockeyDemo.Physics

// ─── Types ─────────────────────────────────────────────────────────────

type Entity =
    { mutable X: float<px>
      mutable Y: float<px>
      mutable VelX: float<subpx / tick>
      mutable VelY: float<subpx / tick>
      mutable DirX: float
      mutable DirY: float
      mutable MaxSpeed: float<subpx / tick>
      mutable Accel: float<subpx / tick>
      mutable ShotPower: float<subpx / tick> }

[<Struct>]
type BallState =
    | Free
    | HeldBy of entityIdx: int

/// Which team scored (for goal-flash display)
[<Struct>]
type GoalScoredBy =
    | NoGoal
    | Team1Scored
    | Team2Scored

/// A skate mark left on the ice during a tight turn
type TrailMark =
    { mutable X: float<px>
      mutable Y: float<px>
      mutable Life: int<tick> }

/// Player role for 5-player mode AI dispatch
[<Struct>]
type PlayerRole =
    | Goalie
    | Forward
    | Wing

/// One player's directional + fire input for the current tick.
[<Struct>]
type Input =
    { Left: bool; Right: bool; Up: bool; Down: bool; Fire: bool }

module Input =
    let none = { Left = false; Right = false; Up = false; Down = false; Fire = false }

// ─── League / Tournament Types ─────────────────────────────────────────

type TeamStats =
    { mutable Wins: int
      mutable Losses: int
      mutable Draws: int
      mutable Points: int
      mutable GoalsFor: int
      mutable GoalsAgainst: int }

type LeagueState =
    { Stats: TeamStats array
      /// Full round-robin: Schedule[round] = array of (team1, team2) matchups
      Schedule: (int * int) array array
      Rng: Random
      mutable CurrentRound: int
      mutable Finished: bool
      HumanTeam: int }

type GameState =
    { Entities: Entity array
      Rng: Random
      mutable Team1Score: int
      mutable Team2Score: int
      mutable ClockSeconds: int<sec>
      mutable ClockTick: int<tick>
      mutable BallState: BallState
      mutable PossessionTimer: int<tick>
      // Shot re-capture cooldown: after a player releases the ball, that
      // player (and only that player) cannot re-capture it for a short
      // window, so you cannot pass to yourself.
      mutable LastReleaser: int
      mutable RecaptureBlockTicks: int<tick>
      mutable StalemateCounter: int<tick>
      mutable PrevBallState: BallState
      mutable ActivePlayer1: int
      mutable ActivePlayer2: int
      mutable BallAnimFrame: int
      mutable BallFrictionCounter: int
      mutable GameTick: int<tick>
      mutable Playing: bool
      mutable GoalFlashTimer: int<tick>
      mutable GoalScoredBy: GoalScoredBy
      mutable Team1Idx: int
      mutable Team2Idx: int
      mutable Team1Human: bool
      mutable Team2Human: bool
      mutable PeriodLength: int<sec>
      mutable CurrentPeriod: int
      mutable NumPeriods: int
      // Dynamic player layout
      mutable PlayersPerTeam: int
      mutable FivePlayerMode: bool
      // Keyboard state (one input snapshot per player)
      mutable Input1: Input
      mutable Input2: Input
      // Shoot charge
      mutable FireHoldTicks1: int<tick>
      mutable FireHoldTicks2: int<tick>
      // Stick animation timer per entity
      mutable StickAnimTimers: int array
      // Ice trail: skate marks from tight turns
      TrailMarks: TrailMark array
      mutable TrailMarkCount: int
      mutable TrailMarkHead: int       // circular buffer write index
      PrevDirX: float array            // previous direction per entity
      PrevDirY: float array }

    // Layout derived from the team size. Computed members rather than stored
    // fields so they cannot drift out of sync with PlayersPerTeam.
    member s.NumPlayers = s.PlayersPerTeam * 2
    member s.Team2Start = s.PlayersPerTeam
    member s.BallIdx = s.PlayersPerTeam * 2
    member s.NumEntities = s.PlayersPerTeam * 2 + 1

// ─── Helpers ───────────────────────────────────────────────────────────

let inline private zero<[<Measure>] 'u> =
    LanguagePrimitives.FloatWithMeasure<'u> 0.0

let private zeroVel = 0.0<subpx / tick>

/// Determine the role of a local player index in the current mode
let playerRole (fivePlayer: bool) (localIdx: int) =
    match fivePlayer, localIdx with
    | false, _      -> Forward
    | true, 0       -> Goalie
    | true, (3 | 4) -> Wing
    | true, _       -> Forward          // indices 1, 2, 5 = forwards

/// Is the entity on team 1?
let inline isOnTeam1 (gs: GameState) idx = idx < gs.Team2Start

/// Does the given team own the ball?
let teamOwnsBall (gs: GameState) isTeam1 =
    match gs.BallState with
    | HeldBy owner ->
        if isTeam1 then
            isOnTeam1 gs owner
        else
            not (isOnTeam1 gs owner)
    | Free -> false

/// Does the opponent team own the ball?
let opponentOwnsBall (gs: GameState) isTeam1 = teamOwnsBall gs (not isTeam1)

/// Sign-based velocity from a direction component
let inline private dirToVel (dir: float) (power: float<subpx / tick>) =
    if dir > 0.0 then power
    elif dir < 0.0 then -power
    else zeroVel

// ─── Factory ───────────────────────────────────────────────────────────

let createEntity maxSpd accel shotPwr : Entity =
    { X = 0.0<px>
      Y = 0.0<px>
      VelX = zeroVel
      VelY = zeroVel
      DirX = 0.0
      DirY = 0.0
      MaxSpeed = maxSpd
      Accel = accel
      ShotPower = shotPwr }

let createGameState () : GameState =
    let ppt = PlayersPerTeam3

    let ents =
        Array.init MaxEntities (fun i ->
            if i < MaxPlayersPerTeam * 2 then
                createEntity zeroVel ForwardAccel zeroVel
            else
                createEntity BallMaxSpeed zeroVel zeroVel)

    { Entities = ents
      Rng = Random()
      Team1Score = 0
      Team2Score = 0
      ClockSeconds = 0<sec>
      ClockTick = 0<tick>
      BallState = Free
      PossessionTimer = 0<tick>
      LastReleaser = -1
      RecaptureBlockTicks = 0<tick>
      StalemateCounter = 0<tick>
      PrevBallState = Free
      ActivePlayer1 = 0
      ActivePlayer2 = ppt
      BallAnimFrame = BallAnimFrames
      BallFrictionCounter = BallAnimFrames
      GameTick = 0<tick>
      Playing = false
      GoalFlashTimer = 0<tick>
      GoalScoredBy = NoGoal
      Team1Idx = 0
      Team2Idx = 1
      Team1Human = true
      Team2Human = false
      PeriodLength = PeriodMinutes * 60 * 1<sec>
      CurrentPeriod = 0
      NumPeriods = ExhibitionPeriods
      PlayersPerTeam = ppt
      FivePlayerMode = false
      Input1 = Input.none
      Input2 = Input.none
      FireHoldTicks1 = 0<tick>
      FireHoldTicks2 = 0<tick>
      StickAnimTimers = Array.zeroCreate MaxEntities
      TrailMarks = Array.init MaxTrailMarks (fun _ -> { X = 0.0<px>; Y = 0.0<px>; Life = 0<tick> })
      TrailMarkCount = 0
      TrailMarkHead = 0
      PrevDirX = Array.zeroCreate MaxEntities
      PrevDirY = Array.zeroCreate MaxEntities }

// ─── Set Player Mode ──────────────────────────────────────────────────

let setPlayerMode (gs: GameState) fivePlayer =
    let ppt = if fivePlayer then PlayersPerTeam5 else PlayersPerTeam3
    gs.PlayersPerTeam <- ppt
    gs.FivePlayerMode <- fivePlayer
    // Entity indices change between modes — drop any stale re-capture block
    gs.LastReleaser <- -1
    gs.RecaptureBlockTicks <- 0<tick>
    // Ensure ball entity has correct stats
    let ball = gs.Entities[gs.BallIdx]
    ball.MaxSpeed <- BallMaxSpeed
    ball.Accel <- zeroVel
    ball.ShotPower <- zeroVel

// ─── Position Reset ────────────────────────────────────────────────────

let private resetTeamPositions (gs: GameState) startIdx (homeX: float<px> array) (homeY: float<px> array) dirX =
    for i in 0 .. gs.PlayersPerTeam - 1 do
        let e = gs.Entities[startIdx + i]
        e.X <- homeX[i]
        e.Y <- homeY[i]
        e.VelX <- zeroVel
        e.VelY <- zeroVel
        e.DirX <- dirX
        e.DirY <- 0.0

let resetPositions (gs: GameState) =
    if gs.FivePlayerMode then
        resetTeamPositions gs 0 team1HomeX5 team1HomeY5 1.0
        resetTeamPositions gs gs.Team2Start team2HomeX5 team2HomeY5 -1.0
    else
        resetTeamPositions gs 0 team1HomeX team1HomeY 1.0
        resetTeamPositions gs gs.Team2Start team2HomeX team2HomeY -1.0

    let ball = gs.Entities[gs.BallIdx]
    ball.X <- CenterX
    ball.Y <- CenterY
    ball.VelX <- zeroVel
    ball.VelY <- zeroVel
    gs.BallState <- Free
    gs.PossessionTimer <- 0<tick>
    gs.LastReleaser <- -1
    gs.RecaptureBlockTicks <- 0<tick>
    gs.StalemateCounter <- 0<tick>
    gs.PrevBallState <- Free
    gs.BallAnimFrame <- BallAnimFrames
    gs.BallFrictionCounter <- BallAnimFrames

// ─── Init Match ────────────────────────────────────────────────────────

let initMatch (gs: GameState) =
    gs.Team1Score <- 0
    gs.Team2Score <- 0
    gs.ClockSeconds <- 0<sec>
    gs.ClockTick <- 0<tick>
    gs.GameTick <- 0<tick>
    gs.GoalFlashTimer <- 0<tick>
    gs.CurrentPeriod <- 0
    gs.Playing <- true
    gs.Input1 <- Input.none
    gs.Input2 <- Input.none
    gs.FireHoldTicks1 <- 0<tick>
    gs.FireHoldTicks2 <- 0<tick>
    let skipGoalie = if gs.FivePlayerMode then 1 else 0
    gs.ActivePlayer1 <- skipGoalie
    gs.ActivePlayer2 <- gs.Team2Start + skipGoalie
    resetPositions gs

// ─── Find Nearest Player to Ball ───────────────────────────────────────

let findNearestToBall (gs: GameState) startIdx endIdx =
    let ball = gs.Entities[gs.BallIdx]
    let mutable bestDist = Double.MaxValue
    let mutable bestIdx = startIdx

    for i in startIdx..endIdx do
        let e = gs.Entities[i]
        let dx = float (e.X - ball.X)
        let dy = float (e.Y - ball.Y)
        let d = dx * dx + dy * dy

        if d < bestDist then
            bestDist <- d
            bestIdx <- i

    bestIdx

// ─── Release Ball (kick/shoot) ─────────────────────────────────────────

/// Ticks a player is blocked from re-capturing the ball after releasing it.
/// 18 ticks = 0.3 s at the ~60 Hz physics rate (30 FPS x 2 physics ticks
/// per frame). Prevents shooting and immediately picking the ball back up.
let RecaptureCooldownTicks = 18<tick>

/// Puck speed for a full-power release (subpx/tick). Every player shoots at
/// the same speed for a given charge level, regardless of team/player stats
/// or how fast the shooter is moving — matches the fastest teams' ShotPower.
let ShotReleaseSpeed = 64.0<subpx / tick>

/// powerFrac: 0.0..1.0 — fraction of ShotReleaseSpeed (pass vs full shot)
let releaseBall (gs: GameState) entityIdx (powerFrac: float) =
    let ent = gs.Entities[entityIdx]
    let ball = gs.Entities[gs.BallIdx]
    let power = ShotReleaseSpeed * powerFrac
    ent.VelX <- zeroVel
    ent.VelY <- zeroVel
    ball.VelX <- dirToVel ent.DirX power
    ball.VelY <- dirToVel ent.DirY power
    gs.BallState <- Free
    gs.BallAnimFrame <- BallAnimFrames
    gs.BallFrictionCounter <- BallAnimFrames
    // Block the releaser (and only them) from re-capturing for a short
    // window, starting the very tick the ball leaves the stick.
    gs.LastReleaser <- entityIdx
    gs.RecaptureBlockTicks <- RecaptureCooldownTicks
    gs.StickAnimTimers[entityIdx] <- 10

// ─── Apply Friction ────────────────────────────────────────────────────
// Constant +/-1 per tick toward zero (NOT multiplicative)

let applyFriction (ent: Entity) =
    let decel v =
        if v > zeroVel then max zeroVel (v - FrictionRate)
        elif v < zeroVel then min zeroVel (v + FrictionRate)
        else v

    ent.VelX <- decel ent.VelX
    ent.VelY <- decel ent.VelY

// ─── Clamp Velocity ────────────────────────────────────────────────────

let clampVel (ent: Entity) =
    ent.VelX <- clamp -ent.MaxSpeed ent.MaxSpeed ent.VelX
    ent.VelY <- clamp -ent.MaxSpeed ent.MaxSpeed ent.VelY

// ─── Wall Bounce + Goal Check ──────────────────────────────────────────
// Returns true if a goal was scored

let checkWallsAndGoals (gs: GameState) idx =
    let ent = gs.Entities[idx]
    let isBall = (idx = gs.BallIdx)
    let mutable scored = false

    let inGoalY () = ent.Y >= GoalTop && ent.Y <= GoalBottom

    // Left wall / left goal
    if ent.VelX < zeroVel && ent.X <= FieldLeft then
        if isBall && inGoalY () then
            gs.Team2Score <- gs.Team2Score + 1
            gs.GoalFlashTimer <- 90<tick>
            gs.GoalScoredBy <- Team2Scored
            scored <- true
        else
            ent.X <- FieldLeft
            ent.VelX <- abs ent.VelX
    elif ent.X <= FieldLeft && not isBall then
        ent.X <- FieldLeft

        if ent.VelX < zeroVel then
            ent.VelX <- abs ent.VelX

    // Right wall / right goal
    if ent.VelX > zeroVel && ent.X >= FieldRight then
        if isBall && inGoalY () then
            gs.Team1Score <- gs.Team1Score + 1
            gs.GoalFlashTimer <- 90<tick>
            gs.GoalScoredBy <- Team1Scored
            scored <- true
        else
            ent.X <- FieldRight
            ent.VelX <- -(abs ent.VelX)
    elif ent.X >= FieldRight && not isBall then
        ent.X <- FieldRight

        if ent.VelX > zeroVel then
            ent.VelX <- -(abs ent.VelX)

    // Top/bottom walls
    if ent.VelY < zeroVel && ent.Y <= FieldTop then
        ent.Y <- FieldTop
        ent.VelY <- abs ent.VelY

    if ent.VelY > zeroVel && ent.Y >= FieldBottom then
        ent.Y <- FieldBottom
        ent.VelY <- -(abs ent.VelY)

    // Clamp safety
    if not isBall || not scored then
        ent.X <- clamp FieldLeft FieldRight ent.X

    ent.Y <- clamp FieldTop FieldBottom ent.Y

    scored

// ─── Human Input ───────────────────────────────────────────────────────

let applyHumanInput (gs: GameState) idx (input: Input) (fireHoldTicks: int<tick> byref) =
    let ent = gs.Entities[idx]
    let mutable dx = 0.0
    let mutable dy = 0.0

    if input.Left then
        ent.VelX <- ent.VelX - ent.Accel
        dx <- -1.0

    if input.Right then
        ent.VelX <- ent.VelX + ent.Accel
        dx <- 1.0

    if input.Up then
        ent.VelY <- ent.VelY - ent.Accel
        dy <- -1.0

    if input.Down then
        ent.VelY <- ent.VelY + ent.Accel
        dy <- 1.0

    clampVel ent

    if dx <> 0.0 || dy <> 0.0 then
        ent.DirX <- dx
        ent.DirY <- dy

    // Charge mechanic: hold fire key for harder shot, release to fire
    match gs.BallState with
    | HeldBy owner when owner = idx ->
        if input.Fire then
            fireHoldTicks <- fireHoldTicks + 1<tick>
        elif fireHoldTicks > 0<tick> then
            let t = float (int fireHoldTicks) / float (int ChargeTicksForFull)
            let chargeFrac = PassPowerFraction + (1.0 - PassPowerFraction) * (min 1.0 t)
            fireHoldTicks <- 0<tick>
            releaseBall gs idx chargeFrac
    | _ -> fireHoldTicks <- 0<tick>

// ─── AI: Move toward target ────────────────────────────────────────────

let aiMoveToward (ent: Entity) (targetX: float<px>) (targetY: float<px>) =
    if ent.X > targetX then
        ent.VelX <- ent.VelX - ent.Accel
    elif ent.X < targetX then
        ent.VelX <- ent.VelX + ent.Accel

    if ent.Y > targetY then
        ent.VelY <- ent.VelY - ent.Accel
    elif ent.Y < targetY then
        ent.VelY <- ent.VelY + ent.Accel

    clampVel ent
    let dx = float (targetX - ent.X)
    let dy = float (targetY - ent.Y)

    if abs dx > 2.0 || abs dy > 2.0 then
        ent.DirX <- float (sign dx)
        ent.DirY <- float (sign dy)

// ─── AI: Active Player Logic ───────────────────────────────────────────

let aiActivePlayer (gs: GameState) idx isTeam1 =
    let ent = gs.Entities[idx]
    let ball = gs.Entities[gs.BallIdx]
    let goalDir = if isTeam1 then 1.0 else -1.0

    match gs.BallState with
    | Free -> aiMoveToward ent ball.X ball.Y

    | HeldBy owner when owner = idx ->
        let inShootZone =
            if isTeam1 then
                ent.X > FieldRight - AiShootZoneX
            else
                ent.X < AiShootZoneX

        if inShootZone && ent.Y >= GoalTop && ent.Y <= GoalBottom then
            ent.DirX <- goalDir
            ent.DirY <- 0.0
            releaseBall gs idx 1.0
        elif gs.PossessionTimer |> AiShootCheckpoints.Contains then
            let rndY = float (gs.Rng.Next(int AiRandomShot * 2 + 1)) - AiRandomShot
            ent.DirX <- goalDir

            ent.DirY <-
                if rndY > 3.0 then 1.0
                elif rndY < -3.0 then -1.0
                else 0.0

            releaseBall gs idx 1.0
        else
            let targetX = if isTeam1 then FieldRight else FieldLeft
            let targetY = clamp (GoalTop + 10.0<px>) (GoalBottom - 10.0<px>) ent.Y
            aiMoveToward ent targetX targetY

    | HeldBy _ ->
        if opponentOwnsBall gs isTeam1 then
            aiMoveToward ent ball.X ball.Y
        else
            let supportX = if isTeam1 then ball.X - 30.0<px> else ball.X + 30.0<px>
            aiMoveToward ent (clamp FieldLeft FieldRight supportX) ball.Y

// ─── AI: Defender Logic ────────────────────────────────────────────────

let aiDefender (gs: GameState) idx isTeam1 =
    let ent = gs.Entities[idx]
    let localIdx = if isTeam1 then idx else idx - gs.Team2Start
    let hasBall = teamOwnsBall gs isTeam1

    let homeX, homeY =
        if gs.FivePlayerMode then
            let hx =
                if hasBall then
                    (if isTeam1 then team1HomeX5Attack else team2HomeX5Attack)[localIdx]
                else
                    (if isTeam1 then team1HomeX5 else team2HomeX5)[localIdx]

            let hy = (if isTeam1 then team1HomeY5 else team2HomeY5)[localIdx]
            hx, hy
        else
            let hx =
                if hasBall then
                    (if isTeam1 then team1HomeXAttack else team2HomeXAttack)[localIdx]
                else
                    (if isTeam1 then team1HomeX else team2HomeX)[localIdx]

            let hy = (if isTeam1 then team1HomeY else team2HomeY)[localIdx]
            hx, hy

    aiMoveToward ent homeX homeY

// ─── AI: Goalie Logic (5-player mode, index 0 per team) ──────────────
// Goalie patrols a square zone in front of the goal (not just a line).
// When holding the puck, immediately passes forward to nearest teammate.

let private goalieAutoPass (gs: GameState) goalieIdx isTeam1 =
    let goalie = gs.Entities[goalieIdx]
    let ppt = gs.PlayersPerTeam
    let startEnt = if isTeam1 then 0 else gs.Team2Start

    // Find nearest non-goalie teammate to pass to
    let mutable bestDist = Double.MaxValue
    let mutable bestIdx = -1
    for i in 1 .. ppt - 1 do       // skip index 0 (goalie itself)
        let ei = startEnt + i
        let mate = gs.Entities[ei]
        let dx = float (mate.X - goalie.X)
        let dy = float (mate.Y - goalie.Y)
        let d = dx * dx + dy * dy
        if d < bestDist then
            bestDist <- d
            bestIdx <- ei

    if bestIdx >= 0 then
        let mate = gs.Entities[bestIdx]
        let dx = float (mate.X - goalie.X)
        let dy = float (mate.Y - goalie.Y)
        let len = sqrt (dx * dx + dy * dy)
        if len > 1.0 then
            goalie.DirX <- float (sign dx)
            goalie.DirY <- float (sign dy)
        else
            goalie.DirX <- if isTeam1 then 1.0 else -1.0
            goalie.DirY <- 0.0
        releaseBall gs goalieIdx PassPowerFraction

let aiGoalie (gs: GameState) idx isTeam1 =
    let ent = gs.Entities[idx]
    let ball = gs.Entities[gs.BallIdx]

    // Auto-pass when holding puck (pass immediately, no delay)
    match gs.BallState with
    | HeldBy owner when owner = idx ->
        goalieAutoPass gs idx isTeam1
    | _ -> ()

    // Movement: square zone in front of goal (like real crease)
    // Goalie tracks puck Y, but allowed forward shift depends on game situation:
    //   - Opponent has puck: stay very close to goal line (minimal forward shift)
    //   - Ball free: moderate forward shift
    //   - Team has puck: can come out a bit more
    let baseX = if isTeam1 then GoaliePatrolXLeft else GoaliePatrolXRight
    let forwardShift =
        if opponentOwnsBall gs isTeam1 then 6.0<px>     // stay deep
        elif teamOwnsBall gs isTeam1 then 14.0<px>       // come out a bit
        else 10.0<px>                                     // moderate
    let goalieMinX, goalieMaxX =
        if isTeam1 then baseX, baseX + forwardShift
        else baseX - forwardShift, baseX

    // Move toward puck but clamped within the crease square
    let targetX = clamp goalieMinX goalieMaxX ball.X
    let targetY = clamp (GoalTop + 4.0<px>) (GoalBottom - 4.0<px>) ball.Y
    aiMoveToward ent targetX targetY

// ─── AI: Wing Logic (5-player mode, indices 3-4 per team) ────────────

let aiWing (gs: GameState) idx isTeam1 =
    let ent = gs.Entities[idx]
    let ball = gs.Entities[gs.BallIdx]
    let localIdx = if isTeam1 then idx else idx - gs.Team2Start

    if teamOwnsBall gs isTeam1 then
        let targetX =
            if isTeam1 then
                clamp (FieldLeft + 40.0<px>) (FieldRight - 20.0<px>) (ball.X + 40.0<px>)
            else
                clamp (FieldLeft + 20.0<px>) (FieldRight - 40.0<px>) (ball.X - 40.0<px>)

        let baseY = (if isTeam1 then team1HomeY5 else team2HomeY5)[localIdx]
        let targetY = clamp FieldTop FieldBottom baseY
        aiMoveToward ent targetX targetY
    elif opponentOwnsBall gs isTeam1 then
        let retreatX =
            if isTeam1 then
                clamp FieldLeft (CenterX - 20.0<px>) (ball.X - 50.0<px>)
            else
                clamp (CenterX + 20.0<px>) FieldRight (ball.X + 50.0<px>)

        let targetY = clamp (GoalTop - 10.0<px>) (GoalBottom + 10.0<px>) ball.Y
        aiMoveToward ent retreatX targetY
    else
        let homeX = (if isTeam1 then team1HomeX5 else team2HomeX5)[localIdx]
        let homeY = (if isTeam1 then team1HomeY5 else team2HomeY5)[localIdx]
        aiMoveToward ent ((homeX + ball.X) / 2.0) ((homeY + ball.Y) / 2.0)

// ─── Move Ball When Possessed ──────────────────────────────────────────

let moveBallPossessed (gs: GameState) =
    match gs.BallState with
    | HeldBy owner ->
        let ent = gs.Entities[owner]
        let ball = gs.Entities[gs.BallIdx]
        ball.X <- ent.X + ent.DirX * 8.0<px>
        ball.Y <- ent.Y + ent.DirY * 8.0<px>
        ball.VelX <- zeroVel
        ball.VelY <- zeroVel
    | Free -> ()

// ─── Ball Pickup Collision ─────────────────────────────────────────────

let checkBallPickup (gs: GameState) =
    match gs.BallState with
    | HeldBy _ -> ()
    | Free ->
        let ball = gs.Entities[gs.BallIdx]

        let rec tryPickup i =
            if i < gs.NumPlayers then
                let ent = gs.Entities[i]
                // The player who just released the ball cannot re-capture it
                // during the cooldown; teammates and opponents still can.
                let blocked = i = gs.LastReleaser && gs.RecaptureBlockTicks > 0<tick>

                if not blocked
                   && abs (ent.X - ball.X) < CollisionDist
                   && abs (ent.Y - ball.Y) < CollisionDist then
                    gs.BallState <- HeldBy i
                    gs.PossessionTimer <- PossessionTimer
                    ball.VelX <- zeroVel
                    ball.VelY <- zeroVel
                else
                    tryPickup (i + 1)

        tryPickup 0

// ─── Stalemate Detection ──────────────────────────────────────────────

let checkStalemate (gs: GameState) =
    match gs.PrevBallState, gs.BallState with
    | Free, HeldBy _ -> gs.StalemateCounter <- 0<tick>
    | _, Free -> gs.StalemateCounter <- gs.StalemateCounter + 1<tick>
    | HeldBy a, HeldBy b when a <> b -> gs.StalemateCounter <- 0<tick>
    | _ -> gs.StalemateCounter <- gs.StalemateCounter + 1<tick>

    gs.PrevBallState <- gs.BallState
    gs.StalemateCounter >= StalemateFaceoff

// ─── Game Clock ────────────────────────────────────────────────────────

let updateClock (gs: GameState) =
    gs.ClockTick <- gs.ClockTick + 1<tick>

    if gs.ClockTick >= ClockTicksPerSec * 1<sec> then
        gs.ClockTick <- 0<tick>
        gs.ClockSeconds <- gs.ClockSeconds + 1<sec>

// ─── Process One Team ──────────────────────────────────────────────────

let private processTeam
    (gs: GameState)
    isTeam1
    isHuman
    activeIdx
    (input: Input)
    (holdTicks: int<tick> byref)
    =
    let ppt = gs.PlayersPerTeam
    let t2s = gs.Team2Start
    let startEnt = if isTeam1 then 0 else t2s

    for i in 0 .. ppt - 1 do
        let ei = startEnt + i
        let role = playerRole gs.FivePlayerMode i

        match role with
        | Goalie -> aiGoalie gs ei isTeam1
        | _ when ei = activeIdx ->
            if isHuman then
                applyHumanInput gs ei input &holdTicks
            else
                aiActivePlayer gs ei isTeam1
        | Wing -> aiWing gs ei isTeam1
        | _ -> aiDefender gs ei isTeam1

// ─── Main Game Tick ────────────────────────────────────────────────────

let gameTick (gs: GameState) =
    if not gs.Playing then
        ()
    else

        gs.GameTick <- gs.GameTick + 1<tick>

        // Decrement stick animation timers
        for i in 0 .. gs.NumEntities - 1 do
            let t = gs.StickAnimTimers[i]

            if t > 0 then
                gs.StickAnimTimers[i] <- t - 1

        // Goal flash countdown
        if gs.GoalFlashTimer > 0<tick> then
            gs.GoalFlashTimer <- gs.GoalFlashTimer - 1<tick>

            if gs.GoalFlashTimer = 0<tick> then
                resetPositions gs
        else

            let ppt = gs.PlayersPerTeam
            let t2s = gs.Team2Start

            // Re-capture cooldown countdown. Decremented before the teams are
            // processed, so a release during this tick keeps the full window:
            // the releaser is blocked on the release tick plus the 17 ticks
            // after it, and may re-capture 18 ticks (0.3 s) after the release.
            if gs.RecaptureBlockTicks > 0<tick> then
                gs.RecaptureBlockTicks <- gs.RecaptureBlockTicks - 1<tick>

            // Active player: the holder while a skater has the ball,
            // otherwise nearest to ball (skip goalie in 5-player mode)
            let skipGoalie = if gs.FivePlayerMode then 1 else 0

            let activeFor startIdx =
                match gs.BallState with
                | HeldBy owner when owner >= startIdx + skipGoalie && owner < startIdx + ppt -> owner
                | _ -> findNearestToBall gs (startIdx + skipGoalie) (startIdx + ppt - 1)

            gs.ActivePlayer1 <- activeFor 0
            gs.ActivePlayer2 <- activeFor t2s

            // Process both teams
            processTeam
                gs
                true
                gs.Team1Human
                gs.ActivePlayer1
                gs.Input1
                &gs.FireHoldTicks1

            processTeam
                gs
                false
                gs.Team2Human
                gs.ActivePlayer2
                gs.Input2
                &gs.FireHoldTicks2

            // Possession timer — auto-shoot when it expires
            match gs.BallState with
            | HeldBy owner ->
                gs.PossessionTimer <- gs.PossessionTimer - 1<tick>

                if gs.PossessionTimer <= 0<tick> then
                    let ent = gs.Entities[owner]
                    let vx = ent.VelX
                    let vy = ent.VelY
                    releaseBall gs owner 1.0
                    ent.VelX <- -vx
                    ent.VelY <- -vy
            | Free -> ()

            // Ball friction: only every 8th tick (when BallFrictionCounter resets)
            let mutable applyBallFric = false

            match gs.BallState with
            | HeldBy _ -> moveBallPossessed gs
            | Free ->
                gs.BallFrictionCounter <- gs.BallFrictionCounter - 1

                if gs.BallFrictionCounter <= 0 then
                    gs.BallFrictionCounter <- BallAnimFrames
                    applyBallFric <- true

            // Friction: every tick for players, every 8th tick for the free ball
            for i in 0 .. gs.NumEntities - 1 do
                if i = gs.BallIdx then
                    if applyBallFric then
                        applyFriction gs.Entities[i]
                else
                    applyFriction gs.Entities[i]

            // Teammate separation: push same-team players apart when too close (6v6 only)
            if gs.FivePlayerMode then
                let sepDist = float TeammateSeparationDist
                let sepDistSq = sepDist * sepDist
                let sepForce = TeammateSeparationForce

                let pushApart startIdx count =
                    for i in startIdx .. startIdx + count - 2 do
                        for j in i + 1 .. startIdx + count - 1 do
                            let ei = gs.Entities[i]
                            let ej = gs.Entities[j]
                            let dx = float (ei.X - ej.X)
                            let dy = float (ei.Y - ej.Y)
                            let distSq = dx * dx + dy * dy
                            if distSq < sepDistSq && distSq > 0.01 then
                                let dist = sqrt distSq
                                let nx = dx / dist
                                let ny = dy / dist
                                ei.VelX <- ei.VelX + nx * sepForce
                                ei.VelY <- ei.VelY + ny * sepForce
                                ej.VelX <- ej.VelX - nx * sepForce
                                ej.VelY <- ej.VelY - ny * sepForce

                pushApart 0 ppt
                pushApart t2s ppt

            // When a player is (near-)stationary, face toward the puck
            let ball = gs.Entities[gs.BallIdx]
            for i in 0 .. gs.NumEntities - 1 do
                if i <> gs.BallIdx then
                    let ent = gs.Entities[i]
                    let speedSq = float ent.VelX * float ent.VelX + float ent.VelY * float ent.VelY
                    if speedSq < 4.0 then  // effectively stopped
                        let dx = float (ball.X - ent.X)
                        let dy = float (ball.Y - ent.Y)
                        if abs dx > 2.0 || abs dy > 2.0 then
                            ent.DirX <- float (sign dx)
                            ent.DirY <- float (sign dy)

            // Ice trail: detect tight turns and leave skate marks
            // A tight turn = direction changed significantly while moving fast
            for i in 0 .. gs.NumPlayers - 1 do
                let ent = gs.Entities[i]
                let speedSq = float ent.VelX * float ent.VelX + float ent.VelY * float ent.VelY
                let prevDx = gs.PrevDirX[i]
                let prevDy = gs.PrevDirY[i]
                // Dot product of previous and current direction: < 0 means >90 degree turn
                let dot = ent.DirX * prevDx + ent.DirY * prevDy
                if speedSq > 100.0 && dot < 0.1 && (prevDx <> 0.0 || prevDy <> 0.0) then
                    let markIdx = gs.TrailMarkHead
                    let mark = gs.TrailMarks[markIdx]
                    mark.X <- ent.X
                    mark.Y <- ent.Y
                    mark.Life <- TrailMarkLifetime
                    gs.TrailMarkHead <- (markIdx + 1) % MaxTrailMarks
                    if gs.TrailMarkCount < MaxTrailMarks then
                        gs.TrailMarkCount <- gs.TrailMarkCount + 1
                // Update previous direction
                gs.PrevDirX[i] <- ent.DirX
                gs.PrevDirY[i] <- ent.DirY

            // Decay trail marks
            for i in 0 .. gs.TrailMarkCount - 1 do
                let mark = gs.TrailMarks[i]
                if mark.Life > 0<tick> then
                    mark.Life <- mark.Life - 1<tick>

            // Move entities and check walls/goals
            let mutable goalScored = false

            for i in 0 .. gs.NumEntities - 1 do
                let ent = gs.Entities[i]
                ent.X <- ent.X + ent.VelX * 1.0<tick> / SubPixelUnit
                ent.Y <- ent.Y + ent.VelY * 1.0<tick> / SubPixelUnit

                if checkWallsAndGoals gs i then
                    goalScored <- true

            if not goalScored then
                checkBallPickup gs

                if checkStalemate gs then
                    resetPositions gs

            // Clock
            updateClock gs

            // Period end check (deferred while a goal flash is showing)
            if gs.ClockSeconds >= gs.PeriodLength && gs.GoalFlashTimer <= 0<tick> then
                gs.CurrentPeriod <- gs.CurrentPeriod + 1

                if gs.CurrentPeriod >= gs.NumPeriods then
                    gs.Playing <- false
                else
                    gs.ClockSeconds <- 0<sec>
                    gs.ClockTick <- 0<tick>
                    resetPositions gs

// ─── League Mode ───────────────────────────────────────────────────────

/// Generate full round-robin schedule for N teams (N must be even).
/// Returns an array of rounds; each round is an array of (team1, team2) matchups.
/// Uses the standard circle method: fix team 0, rotate the rest.
let generateSchedule (numTeams: int) =
    let n = numTeams
    // teams list excluding the fixed pivot (team index 0 in rotation, not team-idx 0)
    let rotating = [| 0 .. n - 1 |]
    // We use indices 0..n-1 directly; fix index 0 as pivot
    let rot = Array.init (n - 1) id // rotation slots: 1 .. n-1
    let rounds = Array.init (n - 1) (fun _ -> Array.zeroCreate<int * int> (n / 2))

    for r in 0 .. n - 2 do
        // Build current arrangement: pivot (index 0) + rotated list
        let arrangement = Array.zeroCreate n
        arrangement[0] <- 0

        for i in 0 .. n - 2 do
            arrangement[i + 1] <- rot[(i + r) % (n - 1)] + 1

        // Pair first with last, second with second-to-last, etc.
        for m in 0 .. (n / 2) - 1 do
            rounds[r][m] <- (arrangement[m], arrangement[n - 1 - m])

    rounds

let createTeamStats () =
    { Wins = 0
      Losses = 0
      Draws = 0
      Points = 0
      GoalsFor = 0
      GoalsAgainst = 0 }

let createLeagueState humanTeam =
    { Stats = Array.init NumTeams (fun _ -> createTeamStats ())
      Schedule = generateSchedule NumTeams
      Rng = Random()
      CurrentRound = 0
      Finished = false
      HumanTeam = humanTeam }

/// Record match result for both teams
let recordMatchResult (league: LeagueState) team1Idx team2Idx team1Goals team2Goals =
    let s1 = league.Stats[team1Idx]
    let s2 = league.Stats[team2Idx]
    s1.GoalsFor <- s1.GoalsFor + team1Goals
    s1.GoalsAgainst <- s1.GoalsAgainst + team2Goals
    s2.GoalsFor <- s2.GoalsFor + team2Goals
    s2.GoalsAgainst <- s2.GoalsAgainst + team1Goals

    if team1Goals > team2Goals then
        s1.Wins <- s1.Wins + 1
        s1.Points <- s1.Points + 2
        s2.Losses <- s2.Losses + 1
    elif team2Goals > team1Goals then
        s2.Wins <- s2.Wins + 1
        s2.Points <- s2.Points + 2
        s1.Losses <- s1.Losses + 1
    else
        s1.Draws <- s1.Draws + 1
        s1.Points <- s1.Points + 1
        s2.Draws <- s2.Draws + 1
        s2.Points <- s2.Points + 1

/// Simulate a single CPU-vs-CPU match.
/// Each team's expected goals = baseGoals * strength; actual goals are Poisson-sampled.
/// Scores clamped to 0..10.
let simulateCpuGoals (rng: Random) (strength: float) =
    // Expected goals: ranges from ~1.5 (weakest) to ~5.0 (strongest)
    let lambda = 1.5 + strength * 3.5
    // Poisson sampling via Knuth's method
    let mutable k = 0
    let mutable p = 1.0
    let l = exp (-lambda)

    while p > l do
        k <- k + 1
        p <- p * rng.NextDouble()

    min 10 (max 0 (k - 1))

/// Simulate all CPU-vs-CPU matches for the given round and record results.
let simulateCpuRound (league: LeagueState) (roundIdx: int) =
    let round = league.Schedule[roundIdx]

    for (t1, t2) in round do
        // Skip the matchup involving the human team (already played live)
        if t1 <> league.HumanTeam && t2 <> league.HumanTeam then
            let goals1 = simulateCpuGoals league.Rng teamStrength[t1]
            let goals2 = simulateCpuGoals league.Rng teamStrength[t2]
            recordMatchResult league t1 t2 goals1 goals2

/// Sort standings by points descending, goal difference as tiebreak
let getSortedStandings (league: LeagueState) =
    [| for i in 0 .. NumTeams - 1 -> (i, league.Stats[i]) |]
    |> Array.sortByDescending (fun (_, s) -> s.Points, s.GoalsFor - s.GoalsAgainst)

/// Advance to next round; returns true if league is complete
let advanceRound (league: LeagueState) =
    league.CurrentRound <- league.CurrentRound + 1

    if league.CurrentRound >= league.Schedule.Length then
        league.Finished <- true

    league.Finished

/// Get the human team's matchup for the current round (human always returned as t1)
let currentMatchup (league: LeagueState) =
    let round = league.Schedule[league.CurrentRound]
    let (a, b) =
        round
        |> Array.find (fun (t1, t2) -> t1 = league.HumanTeam || t2 = league.HumanTeam)
    if a = league.HumanTeam then (a, b) else (b, a)
