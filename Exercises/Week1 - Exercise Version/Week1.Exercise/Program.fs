module Program

open Exercise

[<EntryPoint>]
let main args = 
    Exercise.ExecuteExercise()
    System.Console.ReadLine() |> ignore
    0