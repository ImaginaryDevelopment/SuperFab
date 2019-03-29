// Copyright 2018 Fabulous contributors. See LICENSE.md for license.
namespace SuperFab

open System.Diagnostics
open Fabulous.Core
open Fabulous.DynamicViews
open Xamarin.Forms

open Reflective

module App =
    type Model =
      { Count : int
        Step : int
        TimerOn: bool
        LoadedRefs: RefInfo list
        }

    type Msg =
        | Increment
        | Decrement
        | Reset
        | SetStep of int
        | TimerToggled of bool
        | TimedTick
        | ShowRefs of RefInfo list

    let initModel = { Count = 0; Step = 1; TimerOn=false; LoadedRefs=List.empty}

    let init () = initModel, Cmd.none

    let timerCmd =
        async { do! Async.Sleep 200
                return TimedTick }
        |> Cmd.ofAsyncMsg

    let update msg model =
        match msg with
        | Increment -> { model with Count = model.Count + model.Step }, Cmd.none
        | Decrement -> { model with Count = model.Count - model.Step }, Cmd.none
        | Reset -> init ()
        | SetStep n -> { model with Step = n }, Cmd.none
        | TimerToggled on -> { model with TimerOn = on }, (if on then timerCmd else Cmd.none)
        | ShowRefs refs ->
            // let refs = Reflective.getRefsForScript (sprintf "%A" >> fError)
            {model with LoadedRefs=refs}, Cmd.none
        | TimedTick ->
            if model.TimerOn then
                { model with Count = model.Count + model.Step }, timerCmd
            else
                model, Cmd.none

    let view (model: Model) dispatch =
        printfn "let the viewing begin"
        let refGrid =
            let rows = model.LoadedRefs.Length
            let columns = 2
            let children=
                [
                    for row in 0..rows - 1 do
                    let lr = model.LoadedRefs.[row]
                    yield View.Label(text=lr.Name).GridColumn(1).GridRow(row)
                    yield View.TextCell(text=sprintf "%A" lr.Location).GridColumn(2).GridRow(row)
                ]
            printfn "Found %i children" children.Length
            View.Grid(rowdefs=(model.LoadedRefs |> List.map(fun _ -> box "auto")), coldefs=[box "auto"; box "auto"], children=children)
        View.ContentPage(
          content = View.StackLayout(padding = 20.0, verticalOptions = LayoutOptions.Center,
            children = [
                View.Label(text = sprintf "%d" model.Count, horizontalOptions = LayoutOptions.Center, widthRequest=200.0, horizontalTextAlignment=TextAlignment.Center)
                View.Button(text = "Increment", command = (fun () -> dispatch Increment), horizontalOptions = LayoutOptions.Center)
                View.Button(text = "Decrement", command = (fun () -> dispatch Decrement), horizontalOptions = LayoutOptions.Center)
                View.Label(text = "Timer", horizontalOptions = LayoutOptions.Center)
                View.Switch(isToggled = model.TimerOn, toggled = (fun on -> dispatch (TimerToggled on.Value)), horizontalOptions = LayoutOptions.Center)
                View.Slider(minimumMaximum = (0.0, 10.0), value = double model.Step, valueChanged = (fun args -> dispatch (SetStep (int (args.NewValue + 0.5)))), horizontalOptions = LayoutOptions.FillAndExpand)
                View.Label(text = sprintf "Step size: %d" model.Step, horizontalOptions = LayoutOptions.Center)
                View.Button(text = "Reset", horizontalOptions = LayoutOptions.Center, command = (fun () -> dispatch Reset), canExecute = (model <> initModel))
                View.Button(text= "GetRefs", command = fun () -> Reflective.getRefs() |> List.ofSeq |> Msg.ShowRefs |> dispatch )
                refGrid.MinimumHeightRequest 100.0
            ]))

    // Note, this declaration is needed if you enable LiveUpdate
    let program = Program.mkProgram init update view

type App () as app =
    inherit Application ()

    let runner =
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.runWithDynamicView app

#if DEBUG
    // Uncomment this line to enable live update in debug mode.
    // See https://fsprojects.github.io/Fabulous/tools.html for further  instructions.
    //
    //do runner.EnableLiveUpdate()
#endif

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/models.html for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() =

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() =
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) ->

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex ->
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() =
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif


