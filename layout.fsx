#I "packages/OpenTK/lib/NET40"
#r "proj.dll"
#r "OpenTK.dll"

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

open System
open System.Collections.Generic
open ProcCastle.Base

let r = new Random()
let block c (bb:BoundingBox) = Block(c, bb.BottomCenter, bb.Dimensions * 0.5f)
let tee f n = f n; n

let castlehash = new SpatialHash(0.5f * Vector3.One)
let courtyard =
    BoundingBox.FromBox (Vector3.Zero, 5.0f * r.NextV3(0.5f, 1.5f, 0.01f, 0.01f, 0.5f, 1.5f))
    // |> castlehash.PlaceNearest
    |> block Color4.Green

let keep i =
    // let pos = r.NextV3(-1.0f, 1.0f, 0.0f, 0.0f, -1.0f, 1.0f)
    // let dim = (1.5f * r.NextV3(0.5f, 1.5f, 0.2f, 0.8f, 0.5f, 1.5f))
    let pos = Vector3.Zero
    let dim = Vector3.One

    let debug (bb:BoundingBox) =
        printfn "Placing %O" bb
        ()
    BoundingBox.FromBox(pos, dim)
    |> tee (fun bb ->
            printfn "Placing %O" bb)
    |> castlehash.PlaceNearest
    |> tee (fun bb ->
            printfn "  Placed at %O" bb)
    |> block [Color4.Gold; Color4.Silver; Color4.Brown].[r.Next(3)]
    |> tee (fun block ->
            match block with
            | Block(c,p,d) -> printfn "  block: %O" (p, d))
List.init 2 keep |> List.scan (fun s t -> t::s) []
// |> List.iter (fun x -> ProcCastle.Renderer._tempdisplay x 2.0)

// let data = [Offset(Matrix4.CreateScale(0.3f), (courtyard :: keeps))]
// ProcCastle.Renderer.
