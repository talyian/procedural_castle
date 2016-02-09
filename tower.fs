module ProcCastle.Towers
open ProcCastle.Base
open OpenTK
open OpenTK.Graphics
open System

type V3 = Vector3

type SegmentStyle =
    | Window
    | PutHole
    | ArrowSlit
    | Archway
    | Arches    
    | Door

type SpireStyle = | Onion | Cone | FlagOnly

type TowerSegment =
    | Segment of (int * int * SegmentStyle)
    | Top of int * float32
    | Spire of float32 * SpireStyle

type Tower =
    | Buttressed of TowerSegment list
    | Tiered of TowerSegment list
    | Normal of TowerSegment list

let col = Color4.Gray

let generateSegments num = 
    let rec f pos lst = function
        | 0 -> lst
        | n -> let dim = Vector3.One
               let block = TowerSegment.Segment(num, n, Window)
               f (pos + dim * Vector3.UnitY) (block::lst) (n-1)
    f Vector3.Zero []

let rec singleFace w h = function
    | PutHole
    | ArrowSlit -> [ Block(col, V3(0.f, 0.f, 1.f), V3(1.f, 1.f, 0.1f)) ]
    | Archway
    | Arches
    | Door
    | Window -> 
        [ Block(col, V3(0.f, 0.f, 1.f), V3(1.f, 0.1f, 0.1f))
          Block(col, V3(0.f, 0.9f, 1.f), V3(1.f, 0.1f, 0.1f))
          Block(col, V3(0.f, 0.1f, 1.f), V3(0.6f, 0.1f, 0.2f))
          Block(col, V3(0.f, 0.8f, 1.f), V3(0.6f, 0.1f, 0.2f))
          Block(col, V3(-0.7f, 0.f, 1.f), V3(0.3f, 1.0f, 0.1f))
          Block(col, V3(0.7f, 0.f, 1.f), V3(0.3f, 1.0f, 0.1f)) ]

let toPolySegments (pos:Vector3) = function
    | Top(d,e) -> [ ]
    | Spire(f,g) -> [ ]
    | Segment(n, h, style) ->
        [ for i in 0 .. n - 1 do
            let l = Matrix4.CreateTranslation pos
            let m = Matrix4.CreateScale(tan (3.1416f / float32 n), 1.0f, 1.0f)
            let n = Matrix4.CreateRotationY(float32 i * float32 Math.PI * 2.0f / float32 n)
            let f = singleFace 1 1 [Window;ArrowSlit].[(i + h) % 2]
            yield Offset(m * n * l, f) ]
    
let toPolygons = function
    | Normal(segments)
    | Buttressed(segments)
    | Tiered(segments) ->
        [ let mutable p = Vector3.Zero
          printfn "%A" (segments.Length)
          for s in segments do
              yield! toPolySegments p s
              p <- p + Vector3.UnitY ]

let mutable m = 2
let create n =
    m <- m + 1
    generateSegments m 5 |> Normal |> toPolygons |> List.map (scale 0.5f)

