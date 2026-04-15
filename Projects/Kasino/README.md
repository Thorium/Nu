# Kasino - Finnish Card Game

A digital version of the classic Finnish card game Kasino, built with the [Nu Game Engine](https://github.com/bryanedds/Nu).
There is also [MonoGame version](https://github.com/Thorium/Kasino) available.

## Game Modes

- **Standard Kasino** -- Capture cards to score points. First to 16 wins.
- **Laistokasino** -- Reverse rules. First to 16 *loses*.

Play with 2-4 players in any mix of human and AI opponents.

## How to Play

Capture cards from the table by playing a card from your hand whose value matches the sum of one or more table cards. Capture all table cards at once for a bonus sweep.

### Scoring (per round)

| Category | Points |
|---|---|
| Most cards | 1 |
| Most spades | 2 |
| Each Ace | 1 |
| 10 of Diamonds | 2 |
| 2 of Spades | 1 |
| Each Sweep | 1 |

Ties award no points in that category.

## Controls

| Input | Action |
|---|---|
| Click / Number keys | Select a card |
| Double-click / Enter | Play selected card |
| Drag & drop | Play a card directly |
| Escape | Cancel / Return to menu |
| F11 | Toggle fullscreen |

The table layout can be toggled between **Strict Grid** and **Random Scatter** via the in-game button.

Full rules are available in-game via the **How to Play** button.

## Requirements

- [.NET 10.0 SDK](https://dotnet.microsoft.com/download)
- The [Nu Game Engine](https://github.com/bryanedds/Nu) repository (Kasino is a project within it)

## Build & Run

```bash
dotnet build Kasino.fsproj
dotnet run --project Kasino.fsproj
```
