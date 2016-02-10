module ProcCastle.Towers
open ProcCastle.Base
open OpenTK
open OpenTK.Graphics
open System

type V3 = Vector3

type SegmentStyle = | Window | PutHole | ArrowSlit | Archway | Arches | Door
type SpireStyle = | Onion | Cone | FlagOnly | Top

type TowerSegment =
    | Segment of (int * int * SegmentStyle)
    | Top of int * float32
    | Spire of float32 * SpireStyle

type Tower =
    | Capped of TowerSegment list * SpireStyle
    | Foundation of TowerSegment list
    | Buttress of TowerSegment list * Tower
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
    | Archway
    | Arches
    | Door
    | ArrowSlit -> [ Block(col, V3(0.f, 0.f, 1.f), V3(1.f, 1.f, 0.1f)) ]
    | Window ->
        [ Block(col, V3(0.f, 0.f, 1.f), V3(1.f, 0.1f, 0.1f))
          Block(col, V3(0.f, 0.9f, 1.f), V3(1.f, 0.1f, 0.1f))
          Block(col, V3(0.f, 0.1f, 1.f), V3(0.6f, 0.1f, 0.2f))
          Block(col, V3(0.f, 0.8f, 1.f), V3(0.6f, 0.1f, 0.2f))
          Block(col, V3(-0.7f, 0.f, 1.f), V3(0.3f, 1.0f, 0.1f))
          Block(col, V3(0.7f, 0.f, 1.f), V3(0.3f, 1.0f, 0.1f)) ]

let toPolySegments (pos:Vector3) =
    let makepoly n h style =
        [ for i in 0 .. n - 1 do
            let l = Matrix4.CreateTranslation pos
            let m = Matrix4.CreateScale(tan (3.1416f / float32 n), 1.0f, 1.0f)
            let n = Matrix4.CreateRotationY(float32 i * float32 Math.PI * 2.0f / float32 n)
            let f = singleFace 1 1 [Window;ArrowSlit].[(i + h) % 2]
            yield Offset(m * n * l, f) ]
    function
    | Top(n, h) ->
            let c = Cylinder(col, pos, h * 0.5f, h * 0.5f, 0.6f)
            c :: []
    | Spire(f,g) -> [for i, (r1,r2) in Seq.pairwise [2.f; 3.f; 3.f; 2.7f; 2.f; 1.5f; 1.f; 0.6f; 0.3f; 0.1f; 0.01f] |> Seq.indexed -> Cylinder(col, pos + Vector3(0.f, float32 i / 3.f, 0.f), 0.6f * r1, 0.6f * r2, 0.33333f)]
    | Segment(n, h, style) -> makepoly n h style

let rec toPolygons =
    let foldwith f start = List.mapFold f start >> fst >> List.concat
    let buildtower = foldwith (fun p s -> toPolySegments p s, p + Vector3.UnitY) Vector3.Zero
    function
    | Normal(segments) -> buildtower segments
    | Tiered(segments) -> let do_offset p x = let rr = 0.9f ** (p - 1.0f) in scale3 rr 1.f rr x |> offset 0.f p 0.f
                          foldwith (fun p s -> toPolySegments Vector3.Zero s |> List.map (do_offset p), p + 1.0f) 1.0f segments
    | Buttress(segments, butt) -> buildtower segments
    | Foundation(segments) -> buildtower segments
    | Capped(segments, style) -> buildtower (segments @ [Spire(0.3f, Onion)])
let create n =
    generateSegments n 5 |> fun a -> Capped(a, SpireStyle.Onion) |> toPolygons |> List.map (scale 0.5f)
