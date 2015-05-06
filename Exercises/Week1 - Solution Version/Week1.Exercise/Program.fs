module Program

open Exercise
open ExerciseOptional

[<EntryPoint>]
let main args = 
    ExerciseOptional.executeExercise()
    System.Console.ReadLine() |> ignore
    0