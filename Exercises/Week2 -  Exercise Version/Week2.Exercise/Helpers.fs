module Helpers

open System.Drawing
open System

type Line = {StartPoint : Point; EndPoint : Point}

[<Measure>] type radian
[<Measure>] type degree

let addPoints (p1 : Point) (p2 : Point) = 
    Point(p1.X + p2.X, p1.Y + p2.Y)

//<summary>Returns a new point = p1 - p2</summary>
let subtractPoints (p1 : Point) (p2 : Point) = 
    Point(p1.X - p2.X, p1.Y - p2.Y)

let convertDegreeToRadian (angle : float<degree>) : float<radian> = 
    let radiansPerDegree  = (Math.PI * 1.0<radian>) / 180.0<degree>
    radiansPerDegree * angle

let rotateWrtOrigin (p : Point) (angle : float<degree>) = 
    let oldX = float p.X
    let oldY = float p.Y
    let radianAngle = float <| convertDegreeToRadian angle
    let newX = int (Math.Round(oldX * Math.Cos(radianAngle) - oldY * Math.Sin(radianAngle)))
    let newY = int (Math.Round(oldX * Math.Sin(radianAngle) + oldY * Math.Cos(radianAngle)))
    Point(newX,newY)

let rotateWrtPoint (p : Point) (aroundPoint : Point) (angle : float<degree>) = 
    addPoints aroundPoint <| rotateWrtOrigin (subtractPoints p aroundPoint) angle

