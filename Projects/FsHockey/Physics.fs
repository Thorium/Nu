/// THE FS HOCKEY LEAGUE — Physics Constants
/// Taking influence from Solar Hockey by Galifir Developments (Harm Hanemaayer & John Remyn, 1990-1992)
module HockeyDemo.Physics

// ─── Units of Measure ──────────────────────────────────────────────────
// Compile-time dimensional safety for game physics

[<Measure>]
type px // pixels (game-coordinate space)

[<Measure>]
type subpx // sub-pixel units

[<Measure>]
type tick // game ticks

[<Measure>]
type sec // game-clock seconds

// ─── Entity Layout ─────────────────────────────────────────────────────
// Entities stored in array: Team 1 players, Team 2 players, Puck (last)
// 3-player mode: 0-2 = Team 1, 3-5 = Team 2, 6 = Puck (7 total)
// 5-player mode: 0-5 = Team 1 (0=goalie, 1-2=forwards, 3-4=wings, 5=extra fwd),
//                6-11 = Team 2, 12 = Puck (13 total)

[<Literal>]
let PlayersPerTeam3 = 3

[<Literal>]
let PlayersPerTeam5 = 6

[<Literal>]
let MaxPlayersPerTeam = 6

let MaxEntities = MaxPlayersPerTeam * 2 + 1 // 13

[<Literal>]
let NumTeams = 10

// ─── Field Boundaries (pixel coordinates) ──────────────
// Field occupies most of the screen

let FieldLeft = 9.0<px>
let FieldRight = 295.0<px>
let FieldTop = 8.0<px>
let FieldBottom = 152.0<px>
let GoalTop = 56.0<px>
let GoalBottom = 104.0<px>
let GoalLeftX = 10.0<px>
let GoalRightX = 294.0<px>
// True field center: (FieldLeft + FieldRight) / 2. The faceoff puck spawns
// here; if it is off-center one team is closer to it and wins every faceoff.
let CenterX = 152.0<px>
let CenterY = 80.0<px>
let GoalDepth = 12.0<px>

// ─── Sub-pixel / Physics ───────────────────────────────────────────────
// 32 sub-pixel units per pixel with integer arithmetic

let SubPixelUnit = 32.0<subpx / px>
let FrictionRate = 1.0<subpx / tick>

// ─── Entity Parameters ─────────────────────────────────────────────────

/// Per-team per-player max speed (subpx/tick).
/// Layout: [team][player: 0=goalie, 1=fwd, 2=fwd]
/// Teams: Human, Phobos, Titan, Pluto, Neptune, Saturn, Jupiter, Mars, Moon Minerals, Earth Mutants
let teamMaxSpeed =
    [| [| 16.0<subpx / tick>; 32.0<subpx / tick>; 32.0<subpx / tick> |] // 0  HUMAN (slow)
       [| 16.0<subpx / tick>; 32.0<subpx / tick>; 32.0<subpx / tick> |] // 1  PHOBOS
       [| 32.0<subpx / tick>; 32.0<subpx / tick>; 32.0<subpx / tick> |] // 2  TITAN
       [| 32.0<subpx / tick>; 32.0<subpx / tick>; 32.0<subpx / tick> |] // 3  PLUTO
       [| 32.0<subpx / tick>; 40.0<subpx / tick>; 40.0<subpx / tick> |] // 4  NEPTUNE
       [| 32.0<subpx / tick>; 40.0<subpx / tick>; 40.0<subpx / tick> |] // 5  SATURN
       [| 32.0<subpx / tick>; 48.0<subpx / tick>; 48.0<subpx / tick> |] // 6  JUPITER
       [| 32.0<subpx / tick>; 48.0<subpx / tick>; 48.0<subpx / tick> |] // 7  MARS
       [| 32.0<subpx / tick>; 48.0<subpx / tick>; 48.0<subpx / tick> |] // 8  MOON
       [| 48.0<subpx / tick>; 48.0<subpx / tick>; 48.0<subpx / tick> |] |] // 9  EARTH

/// Per-team per-player shot power (subpx/tick) — same layout
let teamShotPower =
    [| [| 48.0<subpx / tick>; 38.0<subpx / tick>; 38.0<subpx / tick> |] // 0  HUMAN (slow)
       [| 48.0<subpx / tick>; 38.0<subpx / tick>; 38.0<subpx / tick> |] // 1  PHOBOS
       [| 48.0<subpx / tick>; 38.0<subpx / tick>; 38.0<subpx / tick> |] // 2  TITAN
       [| 48.0<subpx / tick>; 48.0<subpx / tick>; 48.0<subpx / tick> |] // 3  PLUTO
       [| 48.0<subpx / tick>; 48.0<subpx / tick>; 48.0<subpx / tick> |] // 4  NEPTUNE
       [| 64.0<subpx / tick>; 48.0<subpx / tick>; 48.0<subpx / tick> |] // 5  SATURN
       [| 48.0<subpx / tick>; 48.0<subpx / tick>; 48.0<subpx / tick> |] // 6  JUPITER
       [| 64.0<subpx / tick>; 48.0<subpx / tick>; 48.0<subpx / tick> |] // 7  MARS
       [| 64.0<subpx / tick>; 64.0<subpx / tick>; 64.0<subpx / tick> |] // 8  MOON
       [| 64.0<subpx / tick>; 64.0<subpx / tick>; 64.0<subpx / tick> |] |] // 9  EARTH

/// Per-team overall strength (0.0 = weakest, 1.0 = strongest).
/// Used for simulating CPU-vs-CPU league results.
let teamStrength =
    [| 0.20 // 0  HUMAN
       0.20 // 1  PHOBOS
       0.30 // 2  TITAN
       0.35 // 3  PLUTO
       0.50 // 4  NEPTUNE
       0.55 // 5  SATURN
       0.70 // 6  JUPITER
       0.75 // 7  MARS
       0.85 // 8  MOON
       0.95 |] // 9  EARTH

/// "Human player fast" toggle copies Moon Minerals stats (team 8)
let humanFastTeamIdx = 8

/// Hard mode: CPU speed multiplier (applied to MaxSpeed and ShotPower)
let HardModeSpeedMult = 1.3

/// Acceleration is role-based, NOT team-based (static data)
let GoalieAccel = 2.0<subpx / tick>
let ForwardAccel = 2.0<subpx / tick>

/// Goalie top speed regardless of team stats (much slower than skaters)
let GoalieMaxSpeed = 12.0<subpx / tick>

let PuckMaxSpeed = 16.0<subpx / tick>
let PuckAnimFrames = 8

// ─── Shoot / Pass Charge ──────────────────────────────────────────────
// Hold fire key longer for a harder shot. Quick tap = pass (weaker).

let PassPowerFraction = 0.4
let ChargeTicksForFull = 18<tick>

/// Puck speed for a full-power release (subpx/tick). Every player shoots at
/// the same speed for a given charge level, regardless of team/player stats
/// or how fast the shooter is moving. Normal mode is a touch slower than
/// hard mode (which matches the fastest teams' ShotPower).
let ShotReleaseSpeed = 60.0<subpx / tick>
let HardShotReleaseSpeed = 64.0<subpx / tick>

// ─── Collision ─────────────────────────────────────────────────────────
// |x1-x2| <= 7 and |y1-y2| <= 7 (AABB half-size = 7 px).
// Using < 8.0 to match <= 7 for integer pixel coordinates.

let CollisionDist = 8.0<px>

// ─── Ice Trail (skate marks) ──────────────────────────────────────────

/// Maximum number of skate marks stored at once
let MaxTrailMarks = 120
/// Ticks before a skate mark fades away
let TrailMarkLifetime = 90<tick>

// ─── Possession ────────────────────────────────────────────────────────

let PossessionTimer = 200<tick>
let StalemateFaceoff = 500<tick>

// ─── AI Constants ──────────────────────────────────────────────────────

let AiShootZoneX = 69.0<px>
let AiRandomShot = 8.0

/// Minimum distance before same-team players start repelling each other
let TeammateSeparationDist = 18.0<px>
/// Velocity nudge applied per tick when teammates overlap
let TeammateSeparationForce = 2.0<subpx / tick>

// AI puck-carrier behaviour (CPU active player holding the puck)

/// Opponent this close counts as immediate pressure (pass or back off)
let AiPressureDist = 26.0<px>
/// Opponent within this distance on the goal side counts as blocking
let AiBlockDist = 42.0<px>
/// A teammate is "open" if the nearest opponent is further than this
let AiMateOpenDist = 24.0<px>
/// Pass range: don't pass to a teammate closer/further than this
let AiPassMinDist = 24.0<px>
let AiPassMaxDist = 130.0<px>
/// Puck speed fraction for an AI pass (between tap-pass and full shot)
let AiPassPowerFraction = 0.6
/// Forced shot when the possession timer runs this low (the timer starts
/// at PossessionTimer and force-releases at 0; shoot just before that)
let AiForcedShotTimer = 30<tick>
/// For this long after gaining the puck the CPU carrier rushes toward the
/// opponent goal (dodging around blockers) instead of passing or backing off
let AiInitialRushTicks = 120<tick>
/// How far short of the end boards the carrier aims while rushing
let AiCarryTargetMargin = 30.0<px>

// AI wander: random offset added to non-carrier AI target positions so
// players don't all skate to exactly the same spot every time.

/// Maximum wander offset in each axis
let AiWanderRange = 14.0<px>
/// Ticks between re-rolls of each player's wander offset
let AiWanderIntervalTicks = 40<tick>

/// Hysteresis for human-team active-player selection: the control marker
/// only jumps to a teammate at least this many px closer to the puck, so
/// control doesn't thrash between players and the player being steered is
/// never handed to the AI mid-move (CPU teams switch on exact nearest)
let AiActiveSwitchMargin = 42.0<px>

/// Top-speed fraction for non-active players drifting back to their base
/// position while the puck is loose (nobody owns it) — no need to sprint
let AiReturnSpeedFrac = 0.55

/// Skaters bounce off a goalie's body instead of skating through it
let GoalieBodyRadius = 9.0<px>

/// The carrier dekes around the goalie when the goalie's Y is within this
/// distance of the carrier's Y (goalie lined up to block the shot)
let AiGoalieAvoidY = 10.0<px>
/// Lateral offset (from the goalie) the carrier cuts to when deking
let AiGoalieDekeOffset = 16.0<px>

/// Ticks the "PERIOD X" banner shows (and play holds) at each period start
let PeriodFlashTicks = 60<tick>

/// Skating speed shown in the menu for a team, with the current fast-human
/// and hard-mode settings applied (same formula the match setup uses)
let displayedTeamSpeed (fastHuman: bool) (hardMode: bool) (teamIdx: int) =
    let srcIdx = if fastHuman && teamIdx = 0 then humanFastTeamIdx else teamIdx
    let mult = if hardMode && teamIdx <> 0 then HardModeSpeedMult else 1.0
    int (teamMaxSpeed.[srcIdx].[1] * mult)

// ─── Game Timing ───────────────────────────────────────────────────────
// Game loop runs at CGA vertical retrace rate (~60 Hz).
// We render at 30 FPS with 2 physics ticks per frame -> ~60 Hz effective.
// The game clock advances 1 clock-second every ClockTicksPerSec ticks.

[<Literal>]
let GameFps = 30

[<Literal>]
let PhysicsTicksPerFrame = 2

[<Literal>]
let PeriodMinutes = 1

/// Clock advances 1 clock-second every 30 game ticks; at ~60 Hz that is
/// 2 clock-seconds per real second (a 60-clock-second period lasts ~30 real seconds).
let ClockTicksPerSec = 30<tick / sec>

// ─── Periods ──────────────────────────────────────────────────────────
// Exhibition = 1 period, tournament/league = 3 periods per match

[<Literal>]
let ExhibitionPeriods = 1

[<Literal>]
let LeaguePeriods = 3

// ─── Team Names (like in Solar Hockey) ────────────────────────────────

let teamNames =
    [| "Human Player"
       "Phobos Lightning"
       "Titan Blackhawks"
       "Pluto Penguins"
       "Neptune Devils"
       "Saturn Rangers"
       "Jupiter Avalanche"
       "Mars Red Wings"
       "Moon Bruins"
       "Earth Oilers" |]

// ─── Home Positions (reconstructed from load_team_positions) ───────────
// 3-player mode: center, forward, defender per team
// 5-player mode adds: goalie (idx 0), wings (idx 3,4)
// Team 1 (left side), Team 2 (right side, mirrored)

// 3-player positions (indices 0-2 in 3-player mode).
// Team 2 X values are exact mirrors of team 1 (304 - x) so neither team
// starts closer to the faceoff spot.
let team1HomeX = [| 100.0<px>; 60.0<px>; 180.0<px> |]
let team1HomeY = [| 80.0<px>; 50.0<px>; 110.0<px> |]
let team2HomeX = [| 204.0<px>; 244.0<px>; 124.0<px> |]
let team2HomeY = [| 80.0<px>; 50.0<px>; 110.0<px> |]

// Shifted positions when team has puck (offset toward opponent goal).
// Team 1 attacks right (larger X), team 2 attacks left (smaller X):
// center pushes up to a forward spot, forward goes deep, defender holds
// around center ice as the safety valve.
let team1HomeXAttack = [| 190.0<px>; 235.0<px>; 150.0<px> |]
let team2HomeXAttack = [| 114.0<px>; 69.0<px>; 154.0<px> |]

// 5-player mode extra positions (5 skaters + goalie = 6 per team)
// Index layout: 0=goalie, 1=center, 2=forward, 3=wing-top, 4=wing-bottom, 5=extra-fwd
let team1HomeX5 = [| 20.0<px>; 100.0<px>; 60.0<px>; 140.0<px>; 140.0<px>; 80.0<px> |]
let team1HomeY5 = [| 80.0<px>; 80.0<px>; 50.0<px>; 40.0<px>; 120.0<px>; 110.0<px> |]
let team2HomeX5 = [| 284.0<px>; 204.0<px>; 244.0<px>; 164.0<px>; 164.0<px>; 224.0<px> |]
let team2HomeY5 = [| 80.0<px>; 80.0<px>; 50.0<px>; 40.0<px>; 120.0<px>; 110.0<px> |]

// 5-player attack positions (toward the opponent goal; goalie stays home,
// center mid pushes up to a forward spot alongside the forward)
let team1HomeX5Attack = [| 20.0<px>; 195.0<px>; 230.0<px>; 180.0<px>; 180.0<px>; 165.0<px> |]
let team2HomeX5Attack = [| 284.0<px>; 109.0<px>; 74.0<px>; 124.0<px>; 124.0<px>; 139.0<px> |]

// Goalie patrol area (stays near own goal)
let GoaliePatrolXLeft = 20.0<px>
let GoaliePatrolXRight = 284.0<px>

// ─── Utility ───────────────────────────────────────────────────────────

let inline clamp lo hi v = max lo (min hi v)

/// Strip unit of measure for interop (rendering, etc.)
let inline stripPx (v: float<px>) : float = float v
