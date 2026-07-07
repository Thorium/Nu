/// THE FS HOCKEY LEAGUE — Game Logic (immutable, Nu/ImSim edition)
/// Entity update, AI, human input, collision, puck physics, scoring.
/// Taking influence from Solar Hockey by Galifir Developments (Harm Hanemaayer & John Remyn, 1990-1992)
///
/// Unlike the WinForms/MonoGame/FableWeb branches (which mutate a shared
/// GameState in place), this edition models the whole match as an immutable
/// value: `gameTick` is a pure `Input -> Input -> Match -> Match` function and
/// even the PRNG is a value carried inside the state. Storing the Match in a
/// Nu simulant property therefore gives live gameplay undo/redo in the editor
/// for free — rewinding restores the exact simulation state, RNG included.
module HockeyDemo.Game

open HockeyDemo.Physics

// ─── Pure PRNG ─────────────────────────────────────────────────────────
// xorshift64 carried as a value in the match state, so randomness is part
// of the immutable simulation state (deterministic, undo/redo-safe).

[<Struct>]
type Rand = Rand of uint64

[<RequireQualifiedAccess>]
module Rand =

    let make (seed: uint64) =
        Rand (if seed = 0UL then 0x9E3779B97F4A7C15UL else seed)

    let next (Rand x) =
        let x = x ^^^ (x <<< 13)
        let x = x ^^^ (x >>> 7)
        let x = x ^^^ (x <<< 17)
        Rand x

    /// Advance and return a float in [0, 1).
    let nextFloat r =
        let (Rand x) as r' = next r
        r', float (x >>> 11) * (1.0 / 9007199254740992.0)

    /// Advance and return an int in [0, n).
    let nextInt (n: int) r =
        let r', f = nextFloat r
        r', int (f * float n)

// ─── Types ─────────────────────────────────────────────────────────────

type Entity =
    { X: float<px>
      Y: float<px>
      VelX: float<subpx / tick>
      VelY: float<subpx / tick>
      DirX: float
      DirY: float
      MaxSpeed: float<subpx / tick>
      Accel: float<subpx / tick>
      ShotPower: float<subpx / tick> }

[<Struct>]
type PuckState =
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
    { MarkX: float<px>
      MarkY: float<px>
      Life: int<tick> }

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

// ─── Match State (immutable) ───────────────────────────────────────────

type Match =
    { Entities: Entity array          // never mutated in place: copy-on-write
      Rand: Rand
      Team1Score: int
      Team2Score: int
      ClockSeconds: int<sec>
      ClockTick: int<tick>
      PuckState: PuckState
      PossessionTimer: int<tick>
      // Shot re-capture cooldown: after a player releases the puck, that
      // player (and only that player) cannot re-capture it for a short
      // window, so you cannot pass to yourself.
      LastReleaser: int
      RecaptureBlockTicks: int<tick>
      StalemateCounter: int<tick>
      PrevPuckState: PuckState
      ActivePlayer1: int
      ActivePlayer2: int
      PuckFrictionCounter: int
      GameTick: int<tick>
      Playing: bool
      GoalFlashTimer: int<tick>
      // "PERIOD X" banner countdown; play holds while it shows
      PeriodFlashTimer: int<tick>
      GoalScoredBy: GoalScoredBy
      Team1Idx: int
      Team2Idx: int
      Team1Human: bool
      Team2Human: bool
      ShotSpeed: float<subpx / tick>
      PeriodLength: int<sec>
      CurrentPeriod: int
      NumPeriods: int
      PlayersPerTeam: int
      FivePlayerMode: bool
      // Shoot charge (hold fire for a harder shot)
      FireHoldTicks1: int<tick>
      FireHoldTicks2: int<tick>
      // Stick animation timer per entity
      StickAnimTimers: int array
      // AI wander: per-player random target offset, re-rolled periodically
      WanderX: float<px> array
      WanderY: float<px> array
      WanderTimer: int<tick>
      // Ice trail: skate marks from tight turns (circular buffer)
      TrailMarks: TrailMark array
      TrailMarkCount: int
      TrailMarkHead: int
      PrevDirX: float array
      PrevDirY: float array }

    // Layout derived from the team size.
    member m.NumPlayers = m.PlayersPerTeam * 2
    member m.Team2Start = m.PlayersPerTeam
    member m.PuckIdx = m.PlayersPerTeam * 2
    member m.NumEntities = m.PlayersPerTeam * 2 + 1

/// Everything needed to set up a match; the UI builds this from its settings.
type MatchConfig =
    { Team1Idx: int
      Team2Idx: int
      Team1Human: bool
      Team2Human: bool
      FivePlayer: bool
      FastHuman: bool
      HardMode: bool
      NumPeriods: int
      Seed: uint64 }

// ─── Helpers ───────────────────────────────────────────────────────────

let private zeroVel = 0.0<subpx / tick>

/// Determine the role of a local player index in the current mode
let playerRole (fivePlayer: bool) (localIdx: int) =
    match fivePlayer, localIdx with
    | false, _      -> Forward
    | true, 0       -> Goalie
    | true, (3 | 4) -> Wing
    | true, _       -> Forward          // indices 1, 2, 5 = forwards

/// Is the entity on team 1?
let inline isOnTeam1 (m: Match) idx = idx < m.Team2Start

/// Does the given team own the puck?
let teamOwnsPuck (m: Match) isTeam1 =
    match m.PuckState with
    | HeldBy owner -> if isTeam1 then isOnTeam1 m owner else not (isOnTeam1 m owner)
    | Free -> false

/// Does the opponent team own the puck?
let opponentOwnsPuck (m: Match) isTeam1 = teamOwnsPuck m (not isTeam1)

/// Sign-based velocity from a direction component
let private dirToVel (dir: float) (power: float<subpx / tick>) =
    if dir > 0.0 then power
    elif dir < 0.0 then -power
    else zeroVel

/// Replace entity `i` with `f entity` (copy-on-write).
let inline private withEnt i f (m: Match) =
    let ents = Array.copy m.Entities
    ents[i] <- f ents[i]
    { m with Entities = ents }

// ─── Position Reset (faceoff) ──────────────────────────────────────────

let resetPositions (m: Match) =
    let homesX1, homesY1, homesX2, homesY2 =
        if m.FivePlayerMode
        then team1HomeX5, team1HomeY5, team2HomeX5, team2HomeY5
        else team1HomeX, team1HomeY, team2HomeX, team2HomeY

    let placeTeam startIdx (hx: float<px> array) (hy: float<px> array) dirX (ents: Entity array) =
        for i in 0 .. m.PlayersPerTeam - 1 do
            ents[startIdx + i] <-
                { ents[startIdx + i] with
                    X = hx[i]; Y = hy[i]
                    VelX = zeroVel; VelY = zeroVel
                    DirX = dirX; DirY = 0.0 }

    // Small random jitter so faceoff races aren't decided the same way
    // every time (with exact spawn + symmetric positions it's always a tie)
    let rand, jx = Rand.nextFloat m.Rand
    let rand, jy = Rand.nextFloat rand

    let ents = Array.copy m.Entities
    placeTeam 0 homesX1 homesY1 1.0 ents
    placeTeam m.Team2Start homesX2 homesY2 -1.0 ents
    ents[m.PuckIdx] <-
        { ents[m.PuckIdx] with
            X = CenterX + (jx * 2.0 - 1.0) * 6.0<px>
            Y = CenterY + (jy * 2.0 - 1.0) * 6.0<px>
            VelX = zeroVel; VelY = zeroVel }

    { m with
        Entities = ents
        Rand = rand
        PuckState = Free
        PossessionTimer = 0<tick>
        LastReleaser = -1
        RecaptureBlockTicks = 0<tick>
        StalemateCounter = 0<tick>
        PrevPuckState = Free
        PuckFrictionCounter = PuckAnimFrames }

// ─── Match Creation ────────────────────────────────────────────────────

let createMatch (cfg: MatchConfig) : Match =
    let ppt = if cfg.FivePlayer then PlayersPerTeam5 else PlayersPerTeam3

    // Per-player stats, mirroring the classic setTeamSpeeds: team 0 is the
    // human-stats team ("fast human" borrows Moon's stats); CPU teams get the
    // hard-mode multiplier when enabled.
    let mkPlayer teamIdx localIdx =
        let isHumanTeam = teamIdx = 0
        let srcIdx = if cfg.FastHuman && isHumanTeam then humanFastTeamIdx else teamIdx
        let mult = if not isHumanTeam && cfg.HardMode then HardModeSpeedMult else 1.0
        let statIdx =
            if cfg.FivePlayer then
                match localIdx with
                | 0 -> 0
                | i when i <= 2 -> min i 2
                | _ -> 2
            else min localIdx 2
        let isGoalie = localIdx = 0 && cfg.FivePlayer
        let maxSpeed = teamMaxSpeed[srcIdx][statIdx] * mult
        { X = 0.0<px>; Y = 0.0<px>
          VelX = zeroVel; VelY = zeroVel
          DirX = 0.0; DirY = 0.0
          MaxSpeed = (if isGoalie then min maxSpeed GoalieMaxSpeed else maxSpeed)
          Accel = (if isGoalie then GoalieAccel else ForwardAccel) * mult
          ShotPower = teamShotPower[srcIdx][statIdx] * mult }

    let entities =
        [| for i in 0 .. ppt - 1 do mkPlayer cfg.Team1Idx i
           for i in 0 .. ppt - 1 do mkPlayer cfg.Team2Idx i
           // the puck
           { X = 0.0<px>; Y = 0.0<px>
             VelX = zeroVel; VelY = zeroVel
             DirX = 0.0; DirY = 0.0
             MaxSpeed = PuckMaxSpeed; Accel = zeroVel; ShotPower = zeroVel } |]

    let skipGoalie = if cfg.FivePlayer then 1 else 0
    let numEntities = ppt * 2 + 1

    { Entities = entities
      Rand = Rand.make cfg.Seed
      Team1Score = 0
      Team2Score = 0
      ClockSeconds = 0<sec>
      ClockTick = 0<tick>
      PuckState = Free
      PossessionTimer = 0<tick>
      LastReleaser = -1
      RecaptureBlockTicks = 0<tick>
      StalemateCounter = 0<tick>
      PrevPuckState = Free
      ActivePlayer1 = skipGoalie
      ActivePlayer2 = ppt + skipGoalie
      PuckFrictionCounter = PuckAnimFrames
      GameTick = 0<tick>
      Playing = true
      GoalFlashTimer = 0<tick>
      PeriodFlashTimer = PeriodFlashTicks
      GoalScoredBy = NoGoal
      Team1Idx = cfg.Team1Idx
      Team2Idx = cfg.Team2Idx
      Team1Human = cfg.Team1Human
      Team2Human = cfg.Team2Human
      ShotSpeed = (if cfg.HardMode then HardShotReleaseSpeed else ShotReleaseSpeed)
      PeriodLength = PeriodMinutes * 60 * 1<sec>
      CurrentPeriod = 0
      NumPeriods = cfg.NumPeriods
      PlayersPerTeam = ppt
      FivePlayerMode = cfg.FivePlayer
      FireHoldTicks1 = 0<tick>
      FireHoldTicks2 = 0<tick>
      StickAnimTimers = Array.zeroCreate numEntities
      WanderX = Array.zeroCreate numEntities
      WanderY = Array.zeroCreate numEntities
      WanderTimer = 0<tick>
      TrailMarks = Array.init MaxTrailMarks (fun _ -> { MarkX = 0.0<px>; MarkY = 0.0<px>; Life = 0<tick> })
      TrailMarkCount = 0
      TrailMarkHead = 0
      PrevDirX = Array.zeroCreate numEntities
      PrevDirY = Array.zeroCreate numEntities }
    |> resetPositions

/// Is the match over? (not playing, clock expired)
let matchOver (m: Match) =
    not m.Playing && m.ClockSeconds >= m.PeriodLength

// ─── Find Nearest Player to Puck ───────────────────────────────────────

let findNearestToPuck (m: Match) startIdx endIdx =
    let puck = m.Entities[m.PuckIdx]
    let mutable bestDist = System.Double.MaxValue
    let mutable bestIdx = startIdx
    for i in startIdx .. endIdx do
        let e = m.Entities[i]
        let dx = float (e.X - puck.X)
        let dy = float (e.Y - puck.Y)
        let d = dx * dx + dy * dy
        if d < bestDist then
            bestDist <- d
            bestIdx <- i
    bestIdx

// ─── Release Puck (kick/shoot) ─────────────────────────────────────────

/// Ticks a player is blocked from re-capturing the puck after releasing it.
let RecaptureCooldownTicks = 18<tick>

/// powerFrac: 0.0..1.0 — fraction of the match's ShotSpeed (pass vs full shot)
let releasePuck idx (powerFrac: float) (m: Match) =
    let ent = m.Entities[idx]
    let power = m.ShotSpeed * powerFrac
    let ents = Array.copy m.Entities
    ents[idx] <- { ent with VelX = zeroVel; VelY = zeroVel }
    ents[m.PuckIdx] <-
        { ents[m.PuckIdx] with
            VelX = dirToVel ent.DirX power
            VelY = dirToVel ent.DirY power }
    let sticks = Array.copy m.StickAnimTimers
    sticks[idx] <- 10
    { m with
        Entities = ents
        PuckState = Free
        PuckFrictionCounter = PuckAnimFrames
        LastReleaser = idx
        RecaptureBlockTicks = RecaptureCooldownTicks
        StickAnimTimers = sticks }

// ─── Friction / Clamping (pure entity transforms) ─────────────────────

let applyFriction (e: Entity) =
    let decel v =
        if v > zeroVel then max zeroVel (v - FrictionRate)
        elif v < zeroVel then min zeroVel (v + FrictionRate)
        else v
    { e with VelX = decel e.VelX; VelY = decel e.VelY }

let clampVel (e: Entity) =
    { e with
        VelX = clamp -e.MaxSpeed e.MaxSpeed e.VelX
        VelY = clamp -e.MaxSpeed e.MaxSpeed e.VelY }

// ─── Wall Bounce + Goal Check ──────────────────────────────────────────

/// Returns the updated match and whether a goal was scored by this entity.
let private checkWallsAndGoals idx (m: Match) =
    let isPuck = idx = m.PuckIdx
    let mutable e = m.Entities[idx]
    let mutable team1Score = m.Team1Score
    let mutable team2Score = m.Team2Score
    let mutable goalScoredBy = m.GoalScoredBy
    let mutable scored = false

    let inGoalY (e: Entity) = e.Y >= GoalTop && e.Y <= GoalBottom

    // Left wall / left goal
    if e.VelX < zeroVel && e.X <= FieldLeft then
        if isPuck && inGoalY e then
            team2Score <- team2Score + 1
            goalScoredBy <- Team2Scored
            scored <- true
        else
            e <- { e with X = FieldLeft; VelX = abs e.VelX }
    elif e.X <= FieldLeft && not isPuck then
        e <- { e with X = FieldLeft }
        if e.VelX < zeroVel then e <- { e with VelX = abs e.VelX }

    // Right wall / right goal
    if e.VelX > zeroVel && e.X >= FieldRight then
        if isPuck && inGoalY e then
            team1Score <- team1Score + 1
            goalScoredBy <- Team1Scored
            scored <- true
        else
            e <- { e with X = FieldRight; VelX = -(abs e.VelX) }
    elif e.X >= FieldRight && not isPuck then
        e <- { e with X = FieldRight }
        if e.VelX > zeroVel then e <- { e with VelX = -(abs e.VelX) }

    // Top/bottom walls
    if e.VelY < zeroVel && e.Y <= FieldTop then
        e <- { e with Y = FieldTop; VelY = abs e.VelY }
    if e.VelY > zeroVel && e.Y >= FieldBottom then
        e <- { e with Y = FieldBottom; VelY = -(abs e.VelY) }

    // Clamp safety
    if not isPuck || not scored then
        e <- { e with X = clamp FieldLeft FieldRight e.X }
    e <- { e with Y = clamp FieldTop FieldBottom e.Y }

    let final = e
    let m = withEnt idx (fun _ -> final) m
    let m =
        if scored then
            { m with
                Team1Score = team1Score
                Team2Score = team2Score
                GoalScoredBy = goalScoredBy
                GoalFlashTimer = 90<tick> }
        else m
    m, scored

// ─── Human Input ───────────────────────────────────────────────────────

let private applyHumanInput idx isTeam1 (input: Input) (m: Match) =
    let e = m.Entities[idx]
    let mutable dx = 0.0
    let mutable dy = 0.0
    let mutable velX = e.VelX
    let mutable velY = e.VelY

    if input.Left then velX <- velX - e.Accel; dx <- -1.0
    if input.Right then velX <- velX + e.Accel; dx <- 1.0
    if input.Up then velY <- velY - e.Accel; dy <- -1.0
    if input.Down then velY <- velY + e.Accel; dy <- 1.0

    let e = clampVel { e with VelX = velX; VelY = velY }
    let e = if dx <> 0.0 || dy <> 0.0 then { e with DirX = dx; DirY = dy } else e
    let m = withEnt idx (fun _ -> e) m

    let holdTicks = if isTeam1 then m.FireHoldTicks1 else m.FireHoldTicks2
    let setHold t (m: Match) =
        if isTeam1 then { m with FireHoldTicks1 = t } else { m with FireHoldTicks2 = t }

    // Charge mechanic: hold fire key for harder shot, release to fire
    match m.PuckState with
    | HeldBy owner when owner = idx ->
        if input.Fire then
            setHold (holdTicks + 1<tick>) m
        elif holdTicks > 0<tick> then
            let t = float (int holdTicks) / float (int ChargeTicksForFull)
            let chargeFrac = PassPowerFraction + (1.0 - PassPowerFraction) * (min 1.0 t)
            m |> setHold 0<tick> |> releasePuck idx chargeFrac
        else m
    | _ -> setHold 0<tick> m

// ─── AI: Move toward target (pure entity transform) ───────────────────

/// Move toward a target with top speed capped to `speedFrac` of MaxSpeed
/// (1.0 = full speed; lower for unhurried repositioning)
let aiMoveTowardCapped (targetX: float<px>) (targetY: float<px>) (speedFrac: float) (e: Entity) =
    let velX =
        if e.X > targetX then e.VelX - e.Accel
        elif e.X < targetX then e.VelX + e.Accel
        else e.VelX
    let velY =
        if e.Y > targetY then e.VelY - e.Accel
        elif e.Y < targetY then e.VelY + e.Accel
        else e.VelY
    let cap = e.MaxSpeed * speedFrac
    let e = { e with VelX = clamp -cap cap velX; VelY = clamp -cap cap velY }
    let dx = float (targetX - e.X)
    let dy = float (targetY - e.Y)
    if abs dx > 2.0 || abs dy > 2.0 then
        { e with DirX = float (sign dx); DirY = float (sign dy) }
    else e

let aiMoveToward (targetX: float<px>) (targetY: float<px>) (e: Entity) =
    aiMoveTowardCapped targetX targetY 1.0 e

// ─── AI: Active Player Logic ───────────────────────────────────────────

/// Nearest opposing skater to entity `idx` (goalie excluded in 5-player
/// mode). Returns (opponent index, distance in px).
let private nearestOpponent (m: Match) idx isTeam1 =
    let ent = m.Entities[idx]
    let skipGoalie = if m.FivePlayerMode then 1 else 0
    let oppStart = if isTeam1 then m.Team2Start else 0
    let mutable bestDistSq = System.Double.MaxValue
    let mutable bestIdx = oppStart + skipGoalie
    for i in oppStart + skipGoalie .. oppStart + m.PlayersPerTeam - 1 do
        let o = m.Entities[i]
        let dx = float (o.X - ent.X)
        let dy = float (o.Y - ent.Y)
        let d = dx * dx + dy * dy
        if d < bestDistSq then
            bestDistSq <- d
            bestIdx <- i
    bestIdx, sqrt bestDistSq

/// Pick a teammate worth passing to: within pass range, unmarked, and not
/// far behind the carrier; prefer the most forward-positioned candidate.
let private tryFindPassMate (m: Match) idx isTeam1 =
    let ent = m.Entities[idx]
    let goalDir = if isTeam1 then 1.0 else -1.0
    let skipGoalie = if m.FivePlayerMode then 1 else 0
    let startEnt = if isTeam1 then 0 else m.Team2Start
    let mutable best = -1
    let mutable bestForward = -20.0 // allow a slight drop pass, nothing deeper
    for i in skipGoalie .. m.PlayersPerTeam - 1 do
        let ei = startEnt + i
        if ei <> idx then
            let mate = m.Entities[ei]
            let dx = float (mate.X - ent.X)
            let dy = float (mate.Y - ent.Y)
            let dist = sqrt (dx * dx + dy * dy)
            if dist >= float AiPassMinDist && dist <= float AiPassMaxDist then
                let _, oppDist = nearestOpponent m ei isTeam1
                if oppDist > float AiMateOpenDist then
                    let forward = dx * goalDir
                    if forward > bestForward then
                        bestForward <- forward
                        best <- ei
    if best >= 0 then Some best else None

/// Aim at a teammate (8-way, since the puck leaves along DirX/DirY) and pass.
let private aiPassTo idx mateIdx (m: Match) =
    let ent = m.Entities[idx]
    let mate = m.Entities[mateIdx]
    let dx = float (mate.X - ent.X)
    let dy = float (mate.Y - ent.Y)
    let adx = abs dx
    let ady = abs dy
    // Closest of the 8 directions: pure axis when within ~22.5° of it
    let dirX, dirY =
        if adx > 2.414 * ady then float (sign dx), 0.0
        elif ady > 2.414 * adx then 0.0, float (sign dy)
        else float (sign dx), float (sign dy)
    m
    |> withEnt idx (fun e -> { e with DirX = dirX; DirY = dirY })
    |> releasePuck idx AiPassPowerFraction

let private aiActivePlayer idx isTeam1 (m: Match) =
    let ent = m.Entities[idx]
    let puck = m.Entities[m.PuckIdx]
    let goalDir = if isTeam1 then 1.0 else -1.0

    match m.PuckState with
    | Free -> withEnt idx (aiMoveToward puck.X puck.Y) m

    | HeldBy owner when owner = idx ->
        let oppIdx, oppDist = nearestOpponent m idx isTeam1
        let opp = m.Entities[oppIdx]
        // Blocking = close AND on the goal side of the carrier
        let goalSide = float (opp.X - ent.X) * goalDir > -6.0
        let blocked = oppDist < float AiBlockDist && goalSide
        let pressured = oppDist < float AiPressureDist

        let inShootZone =
            if isTeam1 then ent.X > FieldRight - AiShootZoneX
            else ent.X < FieldLeft + AiShootZoneX

        let alignedWithGoal = ent.Y >= GoalTop && ent.Y <= GoalBottom

        // Fresh possession: for the first couple of seconds the carrier
        // rushes toward the opponent goal instead of passing or backing off
        let rushing = m.PossessionTimer > PossessionTimer - AiInitialRushTicks

        // The defending goalie (5-player mode): don't shoot straight into
        // its pads — skate around it instead
        let goalieBlocking, goalieY =
            if m.FivePlayerMode then
                let g = m.Entities[if isTeam1 then m.Team2Start else 0]
                let linedUp = abs (float (g.Y - ent.Y)) < float AiGoalieAvoidY
                let goalSideOfCarrier = float (g.X - ent.X) * goalDir > 0.0
                linedUp && goalSideOfCarrier, g.Y
            else false, 0.0<px>

        if inShootZone && alignedWithGoal && not blocked && not goalieBlocking then
            // Clear look at the goal: shoot — but not with perfect aim; a
            // fair share of shots go off diagonally and miss from range
            let rand, r = Rand.nextInt 100 m.Rand
            let dirY =
                if r < 20 then 1.0
                elif r < 40 then -1.0
                else 0.0
            { m with Rand = rand }
            |> withEnt idx (fun e -> { e with DirX = goalDir; DirY = dirY })
            |> releasePuck idx 1.0
        elif inShootZone && goalieBlocking then
            // Deke: cut sideways around the goalie to open a shooting angle
            // (fast skaters naturally pull this off better)
            let side = if ent.Y >= goalieY then 1.0 else -1.0
            let targetY =
                clamp (GoalTop + 4.0<px>) (GoalBottom - 4.0<px>)
                    (goalieY + side * AiGoalieDekeOffset)
            withEnt idx
                (aiMoveToward (clamp FieldLeft FieldRight (ent.X + goalDir * 10.0<px>)) targetY) m
        elif rushing then
            let targetX =
                if isTeam1 then FieldRight - AiCarryTargetMargin
                else FieldLeft + AiCarryTargetMargin
            let lane =
                if blocked || pressured then
                    // swerve around the defender while still advancing
                    if opp.Y > ent.Y then ent.Y - 28.0<px> else ent.Y + 28.0<px>
                else
                    ent.Y + m.WanderY[idx] * 1.5
            let targetY =
                if inShootZone then clamp (GoalTop + 8.0<px>) (GoalBottom - 8.0<px>) lane
                else clamp (FieldTop + 12.0<px>) (FieldBottom - 12.0<px>) lane
            withEnt idx (aiMoveToward targetX targetY) m
        elif pressured then
            // Opponent right on us: pass if someone is open, otherwise
            // skate a bit backwards and sideways to find a better spot
            match tryFindPassMate m idx isTeam1 with
            | Some mateIdx -> aiPassTo idx mateIdx m
            | None ->
                let backX = ent.X - goalDir * 20.0<px>
                let sideY = if opp.Y > ent.Y then ent.Y - 24.0<px> else ent.Y + 24.0<px>
                withEnt idx
                    (aiMoveToward (clamp FieldLeft FieldRight backX) (clamp FieldTop FieldBottom sideY)) m
        elif m.PossessionTimer <= AiForcedShotTimer then
            // Held long enough — get a shot away before the possession
            // timer force-releases the puck in a random direction
            let rand, r = Rand.nextInt (int AiRandomShot * 2 + 1) m.Rand
            let rndY = float r - AiRandomShot
            let dirY =
                if rndY > 3.0 then 1.0
                elif rndY < -3.0 then -1.0
                else 0.0
            { m with Rand = rand }
            |> withEnt idx (fun e -> { e with DirX = goalDir; DirY = dirY })
            |> releasePuck idx 1.0
        elif blocked then
            // Blocker ahead but not on us yet: pass if a mate is open,
            // otherwise dodge laterally around the blocker, keeping the puck
            match tryFindPassMate m idx isTeam1 with
            | Some mateIdx -> aiPassTo idx mateIdx m
            | None ->
                let sideY = if opp.Y > ent.Y then ent.Y - 28.0<px> else ent.Y + 28.0<px>
                withEnt idx
                    (aiMoveToward
                        (clamp FieldLeft FieldRight (ent.X + goalDir * 8.0<px>))
                        (clamp FieldTop FieldBottom sideY)) m
        else
            // Open ice: carry toward the opponent end, weaving a random
            // route via the wander offset; funnel toward the goal mouth
            // once inside the shooting zone
            let targetX =
                if isTeam1 then FieldRight - AiCarryTargetMargin
                else FieldLeft + AiCarryTargetMargin
            let lane = ent.Y + m.WanderY[idx] * 1.5
            let targetY =
                if inShootZone then clamp (GoalTop + 8.0<px>) (GoalBottom - 8.0<px>) lane
                else clamp (FieldTop + 12.0<px>) (FieldBottom - 12.0<px>) lane
            withEnt idx (aiMoveToward targetX targetY) m

    | HeldBy _ ->
        if opponentOwnsPuck m isTeam1 then
            withEnt idx (aiMoveToward puck.X puck.Y) m
        else
            let supportX =
                (if isTeam1 then puck.X - 30.0<px> else puck.X + 30.0<px>) + m.WanderX[idx]
            let supportY = clamp FieldTop FieldBottom (puck.Y + m.WanderY[idx])
            withEnt idx (aiMoveToward (clamp FieldLeft FieldRight supportX) supportY) m

// ─── AI: Defender Logic ────────────────────────────────────────────────

let private aiDefender idx isTeam1 (m: Match) =
    let localIdx = if isTeam1 then idx else idx - m.Team2Start
    let hasPuck = teamOwnsPuck m isTeam1

    if not m.FivePlayerMode && opponentOwnsPuck m isTeam1 then
        // 3v3 has no goalie: non-active players collapse in front of their
        // own goal (staggered depths) to block the shooting lane.
        // Stand off the crease like defensemen — challenge the shooter,
        // don't stand in the net.
        let puck = m.Entities[m.PuckIdx]
        let guardX =
            if isTeam1 then FieldLeft + 24.0<px> + float localIdx * 12.0<px>
            else FieldRight - 24.0<px> - float localIdx * 12.0<px>
        // Track the puck's Y exactly — this is net-minding duty
        let guardY = clamp (GoalTop + 4.0<px>) (GoalBottom - 4.0<px>) puck.Y
        withEnt idx (aiMoveToward guardX guardY) m
    else
        let homeX, homeY =
            if m.FivePlayerMode then
                let hx =
                    if hasPuck then (if isTeam1 then team1HomeX5Attack else team2HomeX5Attack)[localIdx]
                    else (if isTeam1 then team1HomeX5 else team2HomeX5)[localIdx]
                hx, (if isTeam1 then team1HomeY5 else team2HomeY5)[localIdx]
            else
                let hx =
                    if hasPuck then (if isTeam1 then team1HomeXAttack else team2HomeXAttack)[localIdx]
                    else (if isTeam1 then team1HomeX else team2HomeX)[localIdx]
                hx, (if isTeam1 then team1HomeY else team2HomeY)[localIdx]

        // Wander offset so players don't park on exactly the same spot every time
        let targetX = clamp FieldLeft FieldRight (homeX + m.WanderX[idx])
        let targetY = clamp FieldTop FieldBottom (homeY + m.WanderY[idx])

        // While the puck is loose nobody needs to sprint back to position —
        // drift home at reduced speed so the play doesn't reset so abruptly
        match m.PuckState with
        | Free -> withEnt idx (aiMoveTowardCapped targetX targetY AiReturnSpeedFrac) m
        | HeldBy _ -> withEnt idx (aiMoveToward targetX targetY) m

// ─── AI: Goalie Logic (5-player mode, index 0 per team) ──────────────

let private goalieAutoPass goalieIdx isTeam1 (m: Match) =
    let goalie = m.Entities[goalieIdx]
    let startEnt = if isTeam1 then 0 else m.Team2Start

    // Find nearest non-goalie teammate to pass to
    let mutable bestDist = System.Double.MaxValue
    let mutable bestIdx = -1
    for i in 1 .. m.PlayersPerTeam - 1 do
        let ei = startEnt + i
        let mate = m.Entities[ei]
        let dx = float (mate.X - goalie.X)
        let dy = float (mate.Y - goalie.Y)
        let d = dx * dx + dy * dy
        if d < bestDist then
            bestDist <- d
            bestIdx <- ei

    if bestIdx >= 0 then
        let mate = m.Entities[bestIdx]
        let dx = float (mate.X - goalie.X)
        let dy = float (mate.Y - goalie.Y)
        let len = sqrt (dx * dx + dy * dy)
        let dirX, dirY =
            if len > 1.0 then float (sign dx), float (sign dy)
            else (if isTeam1 then 1.0 else -1.0), 0.0
        m
        |> withEnt goalieIdx (fun e -> { e with DirX = dirX; DirY = dirY })
        |> releasePuck goalieIdx PassPowerFraction
    else m

let private aiGoalie idx isTeam1 (m: Match) =
    // Auto-pass when holding puck (pass immediately, no delay)
    let m =
        match m.PuckState with
        | HeldBy owner when owner = idx -> goalieAutoPass idx isTeam1 m
        | _ -> m

    // Movement: square zone in front of goal; allowed forward shift depends
    // on the game situation
    let puck = m.Entities[m.PuckIdx]
    let baseX = if isTeam1 then GoaliePatrolXLeft else GoaliePatrolXRight
    let forwardShift =
        if opponentOwnsPuck m isTeam1 then 6.0<px>      // stay deep
        elif teamOwnsPuck m isTeam1 then 14.0<px>       // come out a bit
        else 10.0<px>                                    // moderate
    let goalieMinX, goalieMaxX =
        if isTeam1 then baseX, baseX + forwardShift
        else baseX - forwardShift, baseX

    let targetX = clamp goalieMinX goalieMaxX puck.X
    let targetY = clamp (GoalTop + 4.0<px>) (GoalBottom - 4.0<px>) puck.Y
    withEnt idx (aiMoveToward targetX targetY) m

// ─── AI: Wing Logic (5-player mode, indices 3-4 per team) ────────────

let private aiWing idx isTeam1 (m: Match) =
    let puck = m.Entities[m.PuckIdx]
    let localIdx = if isTeam1 then idx else idx - m.Team2Start
    let wx = m.WanderX[idx]
    let wy = m.WanderY[idx]

    if teamOwnsPuck m isTeam1 then
        let targetX =
            if isTeam1 then clamp (FieldLeft + 40.0<px>) (FieldRight - 20.0<px>) (puck.X + 40.0<px> + wx)
            else clamp (FieldLeft + 20.0<px>) (FieldRight - 40.0<px>) (puck.X - 40.0<px> + wx)
        let baseY = (if isTeam1 then team1HomeY5 else team2HomeY5)[localIdx]
        let targetY = clamp FieldTop FieldBottom (baseY + wy)
        withEnt idx (aiMoveToward targetX targetY) m
    elif opponentOwnsPuck m isTeam1 then
        let retreatX =
            if isTeam1 then clamp FieldLeft (CenterX - 20.0<px>) (puck.X - 50.0<px> + wx)
            else clamp (CenterX + 20.0<px>) FieldRight (puck.X + 50.0<px> + wx)
        let targetY = clamp (GoalTop - 10.0<px>) (GoalBottom + 10.0<px>) (puck.Y + wy)
        withEnt idx (aiMoveToward retreatX targetY) m
    else
        // puck is loose: drift toward position at reduced speed
        let homeX = (if isTeam1 then team1HomeX5 else team2HomeX5)[localIdx]
        let homeY = (if isTeam1 then team1HomeY5 else team2HomeY5)[localIdx]
        let targetX = clamp FieldLeft FieldRight ((homeX + puck.X) / 2.0 + wx)
        let targetY = clamp FieldTop FieldBottom ((homeY + puck.Y) / 2.0 + wy)
        withEnt idx (aiMoveTowardCapped targetX targetY AiReturnSpeedFrac) m

// ─── Move Puck When Possessed ──────────────────────────────────────────

let private movePuckPossessed (m: Match) =
    match m.PuckState with
    | HeldBy owner ->
        let ent = m.Entities[owner]
        withEnt m.PuckIdx
            (fun b ->
                { b with
                    X = ent.X + ent.DirX * 8.0<px>
                    Y = ent.Y + ent.DirY * 8.0<px>
                    VelX = zeroVel; VelY = zeroVel }) m
    | Free -> m

// ─── Puck Pickup Collision ─────────────────────────────────────────────

let private checkPuckPickup (m: Match) =
    match m.PuckState with
    | HeldBy _ -> m
    | Free ->
        let puck = m.Entities[m.PuckIdx]
        // Alternate which team's players are checked first, so that when two
        // opponents reach the puck on the same tick the tie doesn't always
        // break toward team 1.
        let offset = if int m.GameTick % 2 = 0 then 0 else m.Team2Start

        let rec tryPickup n =
            if n < m.NumPlayers then
                let i = (n + offset) % m.NumPlayers
                let ent = m.Entities[i]
                let blocked = i = m.LastReleaser && m.RecaptureBlockTicks > 0<tick>
                if not blocked
                   && abs (ent.X - puck.X) < CollisionDist
                   && abs (ent.Y - puck.Y) < CollisionDist then
                    { m with
                        PuckState = HeldBy i
                        PossessionTimer = PossessionTimer }
                    |> withEnt m.PuckIdx (fun b -> { b with VelX = zeroVel; VelY = zeroVel })
                else tryPickup (n + 1)
            else m

        tryPickup 0

// ─── Stalemate Detection ──────────────────────────────────────────────

let private checkStalemate (m: Match) =
    let counter =
        match m.PrevPuckState, m.PuckState with
        | Free, HeldBy _ -> 0<tick>
        | _, Free -> m.StalemateCounter + 1<tick>
        | HeldBy a, HeldBy b when a <> b -> 0<tick>
        | _ -> m.StalemateCounter + 1<tick>
    let m = { m with StalemateCounter = counter; PrevPuckState = m.PuckState }
    m, counter >= StalemateFaceoff

// ─── Game Clock ────────────────────────────────────────────────────────

let private updateClock (m: Match) =
    let clockTick = m.ClockTick + 1<tick>
    if clockTick >= ClockTicksPerSec * 1<sec> then
        { m with ClockTick = 0<tick>; ClockSeconds = m.ClockSeconds + 1<sec> }
    else
        { m with ClockTick = clockTick }

// ─── Process One Team ──────────────────────────────────────────────────

let private processTeam isTeam1 isHuman activeIdx (input: Input) (m: Match) =
    let startEnt = if isTeam1 then 0 else m.Team2Start
    let mutable acc = m
    for i in 0 .. m.PlayersPerTeam - 1 do
        let ei = startEnt + i
        acc <-
            match playerRole acc.FivePlayerMode i with
            | Goalie -> aiGoalie ei isTeam1 acc
            | _ when ei = activeIdx ->
                if isHuman then applyHumanInput ei isTeam1 input acc
                else aiActivePlayer ei isTeam1 acc
            | Wing -> aiWing ei isTeam1 acc
            | _ -> aiDefender ei isTeam1 acc
    acc

// ─── Main Game Tick (pure: Match in, Match out) ───────────────────────

let gameTick (input1: Input) (input2: Input) (m: Match) : Match =
    if not m.Playing then m else

    let m = { m with GameTick = m.GameTick + 1<tick> }

    // Decrement stick animation timers
    let m =
        if Array.exists (fun t -> t > 0) m.StickAnimTimers then
            { m with StickAnimTimers = Array.map (fun t -> max 0 (t - 1)) m.StickAnimTimers }
        else m

    // Goal flash countdown (the rest of the tick pauses while it shows)
    if m.GoalFlashTimer > 0<tick> then
        let m = { m with GoalFlashTimer = m.GoalFlashTimer - 1<tick> }
        if m.GoalFlashTimer = 0<tick> then resetPositions m else m
    // "PERIOD X" banner: hold play while it shows
    elif m.PeriodFlashTimer > 0<tick> then
        { m with PeriodFlashTimer = m.PeriodFlashTimer - 1<tick> }
    else

    // Re-capture cooldown countdown
    let m =
        if m.RecaptureBlockTicks > 0<tick> then
            { m with RecaptureBlockTicks = m.RecaptureBlockTicks - 1<tick> }
        else m

    // Re-roll each player's AI wander offset periodically
    let m =
        let timer = m.WanderTimer - 1<tick>
        if timer <= 0<tick> then
            let wx = Array.copy m.WanderX
            let wy = Array.copy m.WanderY
            let mutable rand = m.Rand
            for i in 0 .. m.NumPlayers - 1 do
                let r1, fx = Rand.nextFloat rand
                let r2, fy = Rand.nextFloat r1
                rand <- r2
                wx[i] <- (fx * 2.0 - 1.0) * AiWanderRange
                wy[i] <- (fy * 2.0 - 1.0) * AiWanderRange
            { m with WanderTimer = AiWanderIntervalTicks; WanderX = wx; WanderY = wy; Rand = rand }
        else { m with WanderTimer = timer }

    // Active player: the holder while a skater has the puck, otherwise
    // nearest to puck (skip goalie in 5-player mode). Human-controlled
    // teams get hysteresis: the marker only jumps to a teammate clearly
    // closer to the puck, so the player being steered isn't handed over
    // to the AI on every micro-difference.
    let skipGoalie = if m.FivePlayerMode then 1 else 0
    let activeFor startIdx currentActive isHuman =
        match m.PuckState with
        | HeldBy owner when owner >= startIdx + skipGoalie && owner < startIdx + m.PlayersPerTeam -> owner
        | _ ->
            let nearest = findNearestToPuck m (startIdx + skipGoalie) (startIdx + m.PlayersPerTeam - 1)
            if not isHuman
               || currentActive < startIdx + skipGoalie
               || currentActive >= startIdx + m.PlayersPerTeam then
                nearest
            else
                let puck = m.Entities[m.PuckIdx]
                let distToPuck i =
                    let e = m.Entities[i]
                    let dx = float (e.X - puck.X)
                    let dy = float (e.Y - puck.Y)
                    sqrt (dx * dx + dy * dy)
                if distToPuck nearest < distToPuck currentActive - float AiActiveSwitchMargin then nearest
                else currentActive
    let m =
        { m with
            ActivePlayer1 = activeFor 0 m.ActivePlayer1 m.Team1Human
            ActivePlayer2 = activeFor m.Team2Start m.ActivePlayer2 m.Team2Human }

    // Process both teams
    let m = processTeam true m.Team1Human m.ActivePlayer1 input1 m
    let m = processTeam false m.Team2Human m.ActivePlayer2 input2 m

    // Possession timer — auto-shoot when it expires (carrier recoils)
    let m =
        match m.PuckState with
        | HeldBy owner ->
            let m = { m with PossessionTimer = m.PossessionTimer - 1<tick> }
            if m.PossessionTimer <= 0<tick> then
                let vx = m.Entities[owner].VelX
                let vy = m.Entities[owner].VelY
                m
                |> releasePuck owner 1.0
                |> withEnt owner (fun e -> { e with VelX = -vx; VelY = -vy })
            else m
        | Free -> m

    // Puck friction cadence: only every 8th tick while free
    let m, applyPuckFric =
        match m.PuckState with
        | HeldBy _ -> movePuckPossessed m, false
        | Free ->
            let counter = m.PuckFrictionCounter - 1
            if counter <= 0 then { m with PuckFrictionCounter = PuckAnimFrames }, true
            else { m with PuckFrictionCounter = counter }, false

    // Friction: every tick for players, every 8th tick for the free puck
    let m =
        { m with
            Entities =
                m.Entities
                |> Array.mapi (fun i e ->
                    if i = m.PuckIdx then (if applyPuckFric then applyFriction e else e)
                    else applyFriction e) }

    // Teammate separation: push same-team players apart when too close (6v6 only)
    let m =
        if m.FivePlayerMode then
            let sepDist = float TeammateSeparationDist
            let sepDistSq = sepDist * sepDist
            let ents = Array.copy m.Entities
            let pushApart startIdx count =
                for i in startIdx .. startIdx + count - 2 do
                    for j in i + 1 .. startIdx + count - 1 do
                        let ei = ents[i]
                        let ej = ents[j]
                        let dx = float (ei.X - ej.X)
                        let dy = float (ei.Y - ej.Y)
                        let distSq = dx * dx + dy * dy
                        if distSq < sepDistSq && distSq > 0.01 then
                            let dist = sqrt distSq
                            let nx = dx / dist
                            let ny = dy / dist
                            ents[i] <- { ei with VelX = ei.VelX + nx * TeammateSeparationForce; VelY = ei.VelY + ny * TeammateSeparationForce }
                            ents[j] <- { ej with VelX = ej.VelX - nx * TeammateSeparationForce; VelY = ej.VelY - ny * TeammateSeparationForce }
            pushApart 0 m.PlayersPerTeam
            pushApart m.Team2Start m.PlayersPerTeam
            { m with Entities = ents }
        else m

    // When a player is (near-)stationary, face toward the puck
    let m =
        let puck = m.Entities[m.PuckIdx]
        { m with
            Entities =
                m.Entities
                |> Array.mapi (fun i e ->
                    if i = m.PuckIdx then e
                    else
                        let speedSq = float e.VelX * float e.VelX + float e.VelY * float e.VelY
                        if speedSq < 4.0 then
                            let dx = float (puck.X - e.X)
                            let dy = float (puck.Y - e.Y)
                            if abs dx > 2.0 || abs dy > 2.0 then
                                { e with DirX = float (sign dx); DirY = float (sign dy) }
                            else e
                        else e) }

    // Ice trail: detect tight turns and leave skate marks, then decay marks
    let m =
        let marks = Array.copy m.TrailMarks
        let prevDirX = Array.copy m.PrevDirX
        let prevDirY = Array.copy m.PrevDirY
        let mutable head = m.TrailMarkHead
        let mutable count = m.TrailMarkCount
        for i in 0 .. m.NumPlayers - 1 do
            let e = m.Entities[i]
            let speedSq = float e.VelX * float e.VelX + float e.VelY * float e.VelY
            let dot = e.DirX * prevDirX[i] + e.DirY * prevDirY[i]
            if speedSq > 100.0 && dot < 0.1 && (prevDirX[i] <> 0.0 || prevDirY[i] <> 0.0) then
                marks[head] <- { MarkX = e.X; MarkY = e.Y; Life = TrailMarkLifetime }
                head <- (head + 1) % MaxTrailMarks
                if count < MaxTrailMarks then count <- count + 1
            prevDirX[i] <- e.DirX
            prevDirY[i] <- e.DirY
        for i in 0 .. count - 1 do
            if marks[i].Life > 0<tick> then
                marks[i] <- { marks[i] with Life = marks[i].Life - 1<tick> }
        { m with TrailMarks = marks; TrailMarkHead = head; TrailMarkCount = count; PrevDirX = prevDirX; PrevDirY = prevDirY }

    // Move entities and check walls/goals
    let m =
        { m with
            Entities =
                m.Entities
                |> Array.map (fun e ->
                    { e with
                        X = e.X + e.VelX * 1.0<tick> / SubPixelUnit
                        Y = e.Y + e.VelY * 1.0<tick> / SubPixelUnit }) }

    let m, goalScored =
        let mutable acc = m
        let mutable scoredAny = false
        for i in 0 .. m.NumEntities - 1 do
            let next, scored = checkWallsAndGoals i acc
            acc <- next
            if scored then scoredAny <- true
        acc, scoredAny

    // Skaters bounce off a goalie's body instead of skating through it:
    // push out along the contact normal and reflect the inbound velocity
    // component (dampened). The goalie holds its ground.
    let m =
        if m.FivePlayerMode then
            let mutable acc = m
            for gIdx in [| 0; m.Team2Start |] do
                let goalie = acc.Entities[gIdx]
                for i in 0 .. acc.NumPlayers - 1 do
                    if i <> gIdx then
                        let e = acc.Entities[i]
                        let dx = float (e.X - goalie.X)
                        let dy = float (e.Y - goalie.Y)
                        let distSq = dx * dx + dy * dy
                        let minD = float GoalieBodyRadius
                        if distSq < minD * minD && distSq > 0.01 then
                            let dist = sqrt distSq
                            let nx = dx / dist
                            let ny = dy / dist
                            let vDotN = float e.VelX * nx + float e.VelY * ny
                            acc <-
                                withEnt i
                                    (fun e ->
                                        { e with
                                            X = clamp FieldLeft FieldRight (goalie.X + nx * GoalieBodyRadius)
                                            Y = clamp FieldTop FieldBottom (goalie.Y + ny * GoalieBodyRadius)
                                            VelX = if vDotN < 0.0 then e.VelX - 1.5 * vDotN * nx * 1.0<subpx / tick> else e.VelX
                                            VelY = if vDotN < 0.0 then e.VelY - 1.5 * vDotN * ny * 1.0<subpx / tick> else e.VelY })
                                    acc
            acc
        else m

    let m =
        if not goalScored then
            let m = checkPuckPickup m
            let m, stalemate = checkStalemate m
            if stalemate then resetPositions m else m
        else m

    // Clock
    let m = updateClock m

    // Period end check (deferred while a goal flash is showing)
    if m.ClockSeconds >= m.PeriodLength && m.GoalFlashTimer <= 0<tick> then
        let period = m.CurrentPeriod + 1
        if period >= m.NumPeriods then
            { m with CurrentPeriod = period; Playing = false }
        else
            { m with
                CurrentPeriod = period
                ClockSeconds = 0<sec>
                ClockTick = 0<tick>
                PeriodFlashTimer = PeriodFlashTicks }
            |> resetPositions
    else m

// ─── League Mode (immutable) ───────────────────────────────────────────

type TeamStanding =
    { Wins: int
      Losses: int
      Draws: int
      Points: int
      GoalsFor: int
      GoalsAgainst: int }

module TeamStanding =
    let empty = { Wins = 0; Losses = 0; Draws = 0; Points = 0; GoalsFor = 0; GoalsAgainst = 0 }

type League =
    { Standings: TeamStanding array   // never mutated in place
      /// Full round-robin: Schedule[round] = array of (team1, team2) matchups
      Schedule: (int * int) array array
      Rand: Rand
      CurrentRound: int
      Finished: bool
      HumanTeam: int }

/// Generate full round-robin schedule for N teams (N must be even) using the
/// standard circle method: fix team 0, rotate the rest.
let generateSchedule (numTeams: int) =
    let n = numTeams
    let rounds = Array.init (n - 1) (fun _ -> Array.zeroCreate<int * int> (n / 2))
    for r in 0 .. n - 2 do
        let arrangement = Array.zeroCreate n
        arrangement[0] <- 0
        for i in 0 .. n - 2 do
            arrangement[i + 1] <- (i + r) % (n - 1) + 1
        for p in 0 .. (n / 2) - 1 do
            rounds[r][p] <- (arrangement[p], arrangement[n - 1 - p])
    rounds

let createLeague humanTeam (seed: uint64) =
    let schedule = generateSchedule NumTeams

    // Shuffle the round order (Fisher-Yates) so the human faces opponents in
    // a random order each league. Every round is still a full round.
    let mutable rand = Rand.make seed
    for i in schedule.Length - 1 .. -1 .. 1 do
        let r', j = Rand.nextInt (i + 1) rand
        rand <- r'
        let tmp = schedule[i]
        schedule[i] <- schedule[j]
        schedule[j] <- tmp

    { Standings = Array.init NumTeams (fun _ -> TeamStanding.empty)
      Schedule = schedule
      Rand = rand
      CurrentRound = 0
      Finished = false
      HumanTeam = humanTeam }

/// Record a match result for both teams (pure).
let recordMatchResult team1Idx team2Idx team1Goals team2Goals (league: League) =
    let standings = Array.copy league.Standings
    let s1 = standings[team1Idx]
    let s2 = standings[team2Idx]
    let s1 = { s1 with GoalsFor = s1.GoalsFor + team1Goals; GoalsAgainst = s1.GoalsAgainst + team2Goals }
    let s2 = { s2 with GoalsFor = s2.GoalsFor + team2Goals; GoalsAgainst = s2.GoalsAgainst + team1Goals }
    let s1, s2 =
        if team1Goals > team2Goals then
            { s1 with Wins = s1.Wins + 1; Points = s1.Points + 2 }, { s2 with Losses = s2.Losses + 1 }
        elif team2Goals > team1Goals then
            { s1 with Losses = s1.Losses + 1 }, { s2 with Wins = s2.Wins + 1; Points = s2.Points + 2 }
        else
            { s1 with Draws = s1.Draws + 1; Points = s1.Points + 1 },
            { s2 with Draws = s2.Draws + 1; Points = s2.Points + 1 }
    standings[team1Idx] <- s1
    standings[team2Idx] <- s2
    { league with Standings = standings }

/// Simulate a single CPU-vs-CPU team's goals: Poisson-sampled expected goals
/// from team strength, clamped to 0..10.
let simulateCpuGoals (strength: float) (rand: Rand) =
    let lambda = 2.2 + strength * 4.0
    let l = exp -lambda
    let mutable rand = rand
    let mutable k = 0
    let mutable p = 1.0
    while p > l do
        let r', f = Rand.nextFloat rand
        rand <- r'
        k <- k + 1
        p <- p * f
    rand, min 10 (max 0 (k - 1))

/// Simulate all CPU-vs-CPU matches for the given round and record results.
let simulateCpuRound (roundIdx: int) (league: League) =
    let mutable league' = league
    let mutable rand = league.Rand
    for t1, t2 in league.Schedule[roundIdx] do
        // Skip the matchup involving the human team (already played live)
        if t1 <> league.HumanTeam && t2 <> league.HumanTeam then
            let r1, goals1 = simulateCpuGoals teamStrength[t1] rand
            let r2, goals2 = simulateCpuGoals teamStrength[t2] r1
            rand <- r2
            league' <- recordMatchResult t1 t2 goals1 goals2 league'
    { league' with Rand = rand }

/// Sort standings by points descending, goal difference as tiebreak
let getSortedStandings (league: League) =
    league.Standings
    |> Array.indexed
    |> Array.sortByDescending (fun (_, s) -> s.Points, s.GoalsFor - s.GoalsAgainst)

/// Advance to next round (pure); Finished set when the schedule is exhausted.
let advanceRound (league: League) =
    let round = league.CurrentRound + 1
    { league with CurrentRound = round; Finished = round >= league.Schedule.Length }

/// Get the human team's matchup for the current round (human always returned as t1)
let currentMatchup (league: League) =
    let round = league.Schedule[league.CurrentRound]
    match round |> Array.tryFind (fun (t1, t2) -> t1 = league.HumanTeam || t2 = league.HumanTeam) with
    | Some (a, b) -> if a = league.HumanTeam then (a, b) else (b, a)
    // Unreachable with an even team count; fall back rather than throw
    | None -> round[0]
