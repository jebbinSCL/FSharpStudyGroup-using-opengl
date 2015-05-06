module Program

open System
open OpenTK
open OpenTK.Graphics.OpenGL
open OpenTK.Input
open Helpers
open Exercise
open System.Drawing

type GameRunning =
    | Continue
    | Stop

type GameState = {
    Running : GameRunning
    Depth: int
    LineLength: int
    ChildAngle: float<degree> }

type StateChange = 
    | StartGame
    | ChangeLineLength of int
    | ChangeChildAngle of int
    | ChangeDepth of int
    | NoChange
    | EndGame

let (|IsPositive|IsZero|IsNegative|) (i: int) =
   if i > 0 then IsPositive i
   else if i < 0 then IsNegative i
   else IsZero i

let rgbValues = [
    ( 230,  97,   1 )
    ( 253, 184,  99 )
    ( 247, 247, 247 )
    ( 178, 171, 210 )
    (  94,  60, 153 ) ]

let colourPalette = seq { while true do yield! rgbValues } |> Seq.map Color.FromArgb

let updateGameState state change = 
    match change with
    | ChangeLineLength i ->
        match state.LineLength + i with
        | IsPositive value ->  { state with LineLength = value }
        | _ ->  state (* Do not change game state *)
    | ChangeChildAngle i ->
        { state with ChildAngle = state.ChildAngle + ((float i) * 0.1<degree>) }
    | ChangeDepth i ->
        match state.Depth + i with
        | IsPositive value -> 
             { state with Depth = value }
        | _ -> 
            state (* Do not change game state *)
    | NoChange -> state
    | StartGame -> state
    | EndGame -> {state with Running = Stop}

//[<STAThread>]
[<EntryPoint>]
let main _ =
    use game = new GameWindow(800, 600)

    let load _ = game.VSync <- VSyncMode.On

    let resize _ = GL.Viewport(0, 0, game.Width, game.Height)

    let updateFrame (state :GameState) =
        match state.Running with 
        | Continue -> ()
        | Stop -> game.Exit()

    let renderFrame state =
        ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit |> GL.Clear
        MatrixMode.Projection |> GL.MatrixMode
        GL.LoadIdentity()
        
        let rawAngle = state.ChildAngle / (1.0<degree>)
        game.Title <- sprintf "Width: %i, Height: %i, Child angle: %f, Line length: %i, Depth: %i" game.Width game.Height rawAngle state.LineLength state.Depth
        
        drawAndSaveFractalTree game.Width game.Height state.ChildAngle state.LineLength state.Depth colourPalette
        game.SwapBuffers()

    let keyDown (args: KeyboardKeyEventArgs) =
        let scale = if args.Shift then 10 else 1
        match args.Key with
        | Key.Left ->  StateChange.ChangeChildAngle scale
        | Key.Right -> StateChange.ChangeChildAngle -scale
        | Key.Up ->StateChange.ChangeLineLength scale
        | Key.Down ->  StateChange.ChangeLineLength -scale
        | Key.PageUp ->  StateChange.ChangeDepth scale
        | Key.PageDown ->  StateChange.ChangeDepth -scale
        | Key.Escape ->  StateChange.EndGame
        | _ -> StateChange.NoChange

    let loadSubscription = game.Load.Subscribe load
    let resizeSubscription = game.Resize.Subscribe resize

    let startGameSubscription = 

        let startGameObservable,triggerGameStart = 
            let internalEvent = new Event<StateChange>()
            let observable = internalEvent.Publish
            let trigger() = internalEvent.Trigger(StartGame)
            (observable,trigger)

        let initialState = 
            { Running = Continue
              LineLength = 50
              Depth = 5
              ChildAngle = 15.0<degree> }

        let updateGameStateStream = 
            game.KeyDown
            |> Observable.map keyDown
            |> Observable.merge startGameObservable
            |> Observable.scan updateGameState initialState  

        let currentGameState = ref initialState

        let updateCurrentStateSub = updateGameStateStream |> Observable.subscribe (fun state -> currentGameState := state)

        let renderFrameSub = 
            game.RenderFrame
            |> Observable.subscribe(fun event -> renderFrame !currentGameState)

        let updateFrameSub = 
            game.UpdateFrame
            |> Observable.subscribe(fun event -> updateFrame !currentGameState)
        triggerGameStart

    startGameSubscription()
    game.Run(60.0)

    0