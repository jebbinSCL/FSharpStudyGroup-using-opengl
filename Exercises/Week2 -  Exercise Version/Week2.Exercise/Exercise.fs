module Exercise

open System.Drawing
open System
open Helpers
open OpenTK.Graphics.OpenGL

type ColouredLine = {Line : Line; Colour : Color}

let generateTree width (childAngle: float<degree>) (lineLength: int) = 
    let rec loop children = function
    | head :: tail -> seq {
        let rotateNewLine = subtractPoints head.EndPoint head.StartPoint |> addPoints head.EndPoint |> rotateWrtPoint
        let child1 = { StartPoint= head.EndPoint; EndPoint = rotateNewLine head.EndPoint childAngle }
        let child2 = { StartPoint= head.EndPoint; EndPoint = rotateNewLine head.EndPoint -childAngle }
        yield! loop (child1 :: child2 :: children) tail
        }
    | [] -> 
        match children with
        | [] -> Seq.empty (* There is no work to do, so finish.  Hopeully, this point is never reached. *)
        | _ -> seq { yield children; yield! loop [] children }
    loop [{ StartPoint= Point(width/2,0); EndPoint = Point(width/2,lineLength) }] []

let drawLine (color : Color) (line : Line) =
    color |> GL.Color3

    PrimitiveType.LineStrip |> GL.Begin
    (line.StartPoint.X, line.StartPoint.Y) |> GL.Vertex2
    (line.EndPoint.X, line.EndPoint.Y) |> GL.Vertex2
    GL.End()

let drawAndSaveFractalTree width height childAngle lineLength depth (colourPalette : seq<Color>) = 
    
    (0.0, (float width), 0.0, (float height), 0.0, 4.0) |> GL.Ortho
    
    let drawLine' { Line = line; Colour = colour } = drawLine colour line
    let branchSeq = generateTree width childAngle lineLength
    
    branchSeq
    |> Seq.take depth
    |> Seq.map2 (fun colour -> 
           Seq.map (fun line -> 
               { Line = line
                 Colour = colour })) colourPalette
    |> Seq.concat
    |> Seq.iter drawLine'

            
    

