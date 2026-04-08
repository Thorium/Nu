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
// Entities stored in array: Team 1 players, Team 2 players, Ball (last)
// 3-player mode: 0-2 = Team 1, 3-5 = Team 2, 6 = Ball (7 total)
// 5-player mode: 0-5 = Team 1 (0=goalie, 1-2=forwards, 3-4=wings, 5=extra fwd),
//                6-11 = Team 2, 12 = Ball (13 total)

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
let CenterX = 144.0<px>
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

let BallMaxSpeed = 16.0<subpx / tick>
let BallAnimFrames = 8

// ─── Shoot / Pass Charge ──────────────────────────────────────────────
// Hold fire key longer for a harder shot. Quick tap = pass (weaker).

let PassPowerFraction = 0.4
let ChargeTicksForFull = 18<tick>

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
let StalemateWarn = 400<tick>
let StalemateFaceoff = 500<tick>

// ─── AI Constants ──────────────────────────────────────────────────────

let AiShootZoneX = 69.0<px>
let AiDefenderShift = 28.0<px>
let AiRandomShot = 8.0

/// Minimum distance before same-team players start repelling each other
let TeammateSeparationDist = 18.0<px>
/// Velocity nudge applied per tick when teammates overlap
let TeammateSeparationForce = 2.0<subpx / tick>

/// AI shoot timing checkpoints (possession_timer == value triggers shot)
let AiShootCheckpoints =
    set [| 175<tick>; 150<tick>; 125<tick>; 100<tick>; 20<tick> |]

// ─── Game Timing ───────────────────────────────────────────────────────
// Game loop runs at CGA vertical retrace rate (~60 Hz).
// tick_countdown=2 halves the CLOCK (1 clock-second every 2 game ticks).
// We render at 30 FPS with 2 physics ticks per frame -> ~60 Hz effective.

[<Literal>]
let GameFps = 30

[<Literal>]
let PhysicsTicksPerFrame = 2

[<Literal>]
let PeriodMinutes = 1

/// Clock increments every 2 game ticks at ~60 Hz -> 30 clock-seconds/real-second.
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

// 3-player positions (indices 0-2 in 3-player mode)
let team1HomeX = [| 100.0<px>; 60.0<px>; 180.0<px> |]
let team1HomeY = [| 80.0<px>; 50.0<px>; 110.0<px> |]
let team2HomeX = [| 200.0<px>; 240.0<px>; 120.0<px> |]
let team2HomeY = [| 80.0<px>; 50.0<px>; 110.0<px> |]

// Shifted positions when team has ball (offset by AiDefenderShift toward opponent goal)
let team1HomeXAttack = [| 72.0<px>; 32.0<px>; 152.0<px> |]
let team2HomeXAttack = [| 228.0<px>; 268.0<px>; 148.0<px> |]

// 5-player mode extra positions (5 skaters + goalie = 6 per team)
// Index layout: 0=goalie, 1=center, 2=forward, 3=wing-top, 4=wing-bottom, 5=extra-fwd
let team1HomeX5 = [| 20.0<px>; 100.0<px>; 60.0<px>; 140.0<px>; 140.0<px>; 80.0<px> |]
let team1HomeY5 = [| 80.0<px>; 80.0<px>; 50.0<px>; 40.0<px>; 120.0<px>; 110.0<px> |]
let team2HomeX5 = [| 284.0<px>; 200.0<px>; 240.0<px>; 164.0<px>; 164.0<px>; 220.0<px> |]
let team2HomeY5 = [| 80.0<px>; 80.0<px>; 50.0<px>; 40.0<px>; 120.0<px>; 110.0<px> |]

// 5-player attack positions
let team1HomeX5Attack = [| 20.0<px>; 72.0<px>; 32.0<px>; 180.0<px>; 180.0<px>; 52.0<px> |]
let team2HomeX5Attack = [| 284.0<px>; 228.0<px>; 268.0<px>; 124.0<px>; 124.0<px>; 248.0<px> |]

// Goalie patrol area (stays near own goal)
let GoaliePatrolXLeft = 20.0<px>
let GoaliePatrolXRight = 284.0<px>

// ─── Utility ───────────────────────────────────────────────────────────

let inline clamp lo hi v = max lo (min hi v)

/// Strip unit of measure for interop (rendering, etc.)
let inline stripPx (v: float<px>) : float = float v
