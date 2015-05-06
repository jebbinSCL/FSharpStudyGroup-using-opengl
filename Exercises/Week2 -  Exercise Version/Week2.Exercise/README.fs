module Introduction

open System

(*Exercise 2
    This weeks exercise is to generate a fractal Tree image. (See a square image version here : http://en.wikipedia.org/wiki/Pythagoras_tree_%28fractal%29) 
    Rather than giving an outline line in Exercise 1, I figured I would give some helpers and suggestions to get you started and let you take a run at it. 

    First off, Run the app to make sure it produces a tree of depth 2
    Goals
    1. Modify the generateTree function to be more flexible. It should deal with: 
        1. a bitmap of any width and height
        2. Line lengths of any size
        3. Child angles of any angle.

    2. modify your generateTree to generate a tree of possibly infinte depth. Things to note: 
        1. Number of branches at a depth is exponential (Sub sequence should have no limit as well)
        2. Test your code on multiple cases : 0, 1, 5.
        3. There are a number of ways to implement a potentially infinte sequence
*)

// a. Recursive sequence Example : http://stackoverflow.com/a/6291059
let numbers:bigint seq = 
    let rec loop n = seq { yield n; yield! loop (n+1I) }
    loop 0I

// b. While loop Example : 
let repeatedSequence = 
    seq { while true do yield! ["a";"b";"c";"d"] }

// c. Seq.initInfinite = (Take an index and generate a value based on it) : https://msdn.microsoft.com/en-us/library/ee370429.aspx
// Generates sequence 1/n^2 with alternatig signs
let seqInfinite = Seq.initInfinite (fun index ->
    let n = float( index + 1 )
    1.0 / (n * n * (if ((index + 1) % 2 = 0) then 1.0 else -1.0)))

// d. Seq.unfold https://msdn.microsoft.com/en-us/library/ee340363.aspx
// Generate a sequence based on an initial state. Then each state based on the previous state. 
//This can also have a cancel condition, by returning None at somepoint in the sequence. 
let daysStartingFrom (date : DateTime) =
    date |> Seq.unfold (fun d -> Some(d, d.AddDays 1.0))

(*
    3. rather than using the lines directly do the following:
        1. create a recursive data structure to store the tree nodes
        2. Remove the line data structure
        3. Remove the draw line function
        4. Replace drawLine with a function that will draw the branches to all node at a certain depth. 

    4. Final Optional Challenge:
        Modify the code to do the following: Given a bitmap of a certain size. THe tree should start at the bottom of the bitmap in the center. 
        Generate the rest of the tree such that the branches of the final depth all touch the top edge of the bitmap, and the far left and far right branches exactly touch the corner. 

    5. Bonuses: 
        1. Make the colour based on the depth and a colour relation ship (E.g. a colour gradient)
        2. Adjust the angle at each depth. 
        3. Chris has some older code that draws a rotating cube. It is hosted in this repo : https://github.com/ceejsmith/DirectX.git 
            In the Vertices.fs  file, there is a sequence generated that should generate the same as the hard coded cube data as below it, but it does not. 
            See whether you can fix it!
*)


