/// KASINO — MMCC port — the Scores screen.
///
/// Ported from the original /mnt/c/git/Kasino ScoreScreen. Shows the per-player
/// round breakdown plus cumulative standings, then continues to the next round
/// or (on game over) back to the menu. The match state lives in the Gameplay
/// screen's model; the top-level game copies the relevant slice into this
/// screen's model when routing here (see KasinoGame), so this screen stays a
/// pure function of its own model.
namespace Kasino
open System
open System.Numerics
open Prime
open Nu
open Kasino.Domain

type ScoresModel =
    { Breakdowns: (Player * Scoring.ScoreBreakdown) list
      Cumulative: Map<string, int>
      RoundNumber: int
      Variant: GameVariant
      IsGameOver: bool }

    static member empty =
        { Breakdowns = []
          Cumulative = Map.empty
          RoundNumber = 1
          Variant = StandardKasino
          IsGameOver = false }

type ScoresMessage =
    | ContinueMsg
    | MenuMsg
    | KeyInput of KeyboardKey
    | Nil
    interface Message

type ScoresCommand =
    | PublishContinue
    | PublishMenu
    interface Command

[<AutoOpen>]
module ScoresExtensions =
    type Screen with
        member this.GetScores world = this.GetModelGeneric<ScoresModel> world
        member this.SetScores value world = this.SetModelGeneric<ScoresModel> value world
        member this.Scores = this.ModelGeneric<ScoresModel> ()
        member this.ContinueEvent = Events.ScoresContinueEvent --> this
        member this.MenuEvent = Events.ScoresMenuEvent --> this

type ScoreDispatcher () =
    inherit ScreenDispatcher<ScoresModel, ScoresMessage, ScoresCommand> (ScoresModel.empty)

    override this.GetFallbackModel (_, _, _) = ScoresModel.empty

    override this.Definitions (_, _) =
        [Game.KeyboardKeyDownEvent =|> fun evt ->
            if not evt.Data.Repeated then KeyInput evt.Data.KeyboardKey else Nil]

    override this.Message (model, message, _, _) =
        match message with
        | ContinueMsg -> withSignal PublishContinue model
        | MenuMsg -> withSignal PublishMenu model
        | KeyInput key ->
            match key with
            | KeyboardKey.Enter | KeyboardKey.KpEnter ->
                if model.IsGameOver then withSignal PublishMenu model else withSignal PublishContinue model
            | KeyboardKey.Escape -> withSignal PublishMenu model
            | _ -> just model
        | Nil -> just model

    override this.Command (_, command, screen, world) =
        match command with
        | PublishContinue -> World.publish () screen.ContinueEvent screen world
        | PublishMenu -> World.publish () screen.MenuEvent screen world

    override this.Content (model, _) =

        let rowY i = 112.0f - float32 i * 15.0f
        let colX col = -120.0f + float32 col * 96.0f
        let categories =
            [ "Most cards"; "Most spades"; "Aces"; "10 of D"; "2 of S"; "Sweeps"; "Round total"; "Cumulative" ]
        let values (p: Player) (b: Scoring.ScoreBreakdown) =
            [ string b.MostCards; string b.MostSpades; string b.Aces; string b.DiamondTen
              string b.SpadeTwo; string b.Sweeps; string b.Total
              string (Map.tryFind p.Name model.Cumulative |> Option.defaultValue 0) ]
        let rowColor i = match i with
                         | 6 -> Clr.yellow
                         | 7 -> Clr.gold
                         | _ -> Clr.white

        let cell name (s: string) x y (col: Color) (size: single) just =
            Content.text name
                [Entity.Position == v3 x y 0.0f
                 Entity.Size == v3 150.0f 14.0f 0.0f
                 Entity.Text == s
                 Entity.Justification == Justified (just, JustifyMiddle)
                 Entity.TextColor == col
                 Entity.FontSizing == Some size
                 Entity.Elevation == 1.0f]

        let catContent =
            [ for i, label in List.indexed categories ->
                cell ("Cat" + string i) label -298.0f (rowY i) Clr.lightGray 10.0f JustifyLeft ]

        let colContent =
            [ for col, (player, bd) in List.indexed model.Breakdowns do
                cell ("PName" + string col) player.Name (colX col) 127.0f Clr.white 11.0f JustifyCenter
                for i, v in List.indexed (values player bd) do
                    cell ("P" + string col + "_" + string i) v (colX col) (rowY i) (rowColor i) 10.0f JustifyCenter ]

        let winnerContent =
            // An exact tie for the deciding score names every tied player
            // rather than an arbitrary one.
            if not model.IsGameOver then []
            else
                match model.Cumulative |> Map.toList with
                | [] -> []
                | xs ->
                    let bestScore =
                        match model.Variant with
                        | StandardKasino -> xs |> List.map snd |> List.max
                        | LaistoKasino   -> xs |> List.map snd |> List.min
                    let winners = xs |> List.filter (fun (_, s) -> s = bestScore) |> List.map fst
                    let text =
                        match winners with
                        | [ w ] -> $"{w} wins with {bestScore} points!"
                        | ws -> String.concat " & " ws + $" tie with {bestScore} points!"
                    [ cell "Winner" text 0.0f -118.0f Clr.gold 13.0f JustifyCenter ]

        let title =
            if model.IsGameOver then "Game Over!" else $"Round {model.RoundNumber} Results"
        let varName = match model.Variant with StandardKasino -> "Standard Kasino" | LaistoKasino -> "Laistokasino"

        [Content.group "Gui" []
            [Content.staticSprite "Bg"
                [Entity.Absolute == true
                 Entity.Position == v3 0.0f 0.0f 0.0f
                 Entity.Size == v3 640.0f 360.0f 0.0f
                 Entity.StaticImage == Assets.Default.White
                 Entity.Color == Clr.screenBg
                 Entity.Elevation == -1.0f]
             cell "Title" title 0.0f 160.0f Clr.gold 18.0f JustifyCenter
             cell "Variant" varName 0.0f 142.0f Clr.gray 11.0f JustifyCenter

             yield! catContent
             yield! colContent
             yield! winnerContent

             if model.IsGameOver then
                Content.button "BtnMenu"
                    [Entity.Position == v3 0.0f -152.0f 0.0f
                     Entity.Size == v3 220.0f Ly.btnH 0.0f
                     Entity.Text == "Back to Menu"
                     Entity.Elevation == 2.0f
                     Entity.ClickEvent => MenuMsg]
             else
                Content.button "BtnNext"
                    [Entity.Position == v3 0.0f -152.0f 0.0f
                     Entity.Size == v3 220.0f Ly.btnH 0.0f
                     Entity.Text == "Next Round"
                     Entity.Elevation == 2.0f
                     Entity.ClickEvent => ContinueMsg]]]
