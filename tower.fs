module ProcCastle.Towers
open ProcCastle.Base
open OpenTK
open OpenTK.Graphics
open System

type V3 = Vector3

type SegmentStyle = | Window | PutHole | ArrowSlit | Archway | Arches | Door
type SpireStyle = | Onion = 0 | Cap = 4 | Cone =1 | FlagOnly = 2 | Top = 3 | ConeNoFlag = 4

type TowerSegment =
    | Segment of (int * int * SegmentStyle)
    | Spire of int * float32 * SpireStyle

type Tower =
    | Capped of TowerSegment list * SpireStyle
    | Tiered of TowerSegment list
    | Normal of TowerSegment list
    | Foundation of Tower
    | Buttress of Tower * Tower list

let generateSegments num =
    let rec f pos lst = function
        | 0 -> lst
        | n -> let dim = Vector3.One
               let block = TowerSegment.Segment(num, n, Window)
               f (pos + dim * Vector3.UnitY) (block::lst) (n-1)
    f Vector3.Zero []

module Options =
    let mutable sides = 3
    let mutable radius = 0.1f
    let mutable color = Color4.Gray
    let mutable color2 = Color4.Blue
    let topstyle () = r.NextEnum<SpireStyle>()
    let reroll () =
        sides <- [4; 6; 10; 16].[r.Next(4)]
        radius <- r.NextF(1.0f, 1.4f)
        color <- Color4.Gray.Mult(r.NextDouble() * 0.8 + 0.5)
        color2 <- Color4.Blue.Mult(r.NextDouble() * 0.3 + 0.5) // top color
    reroll()
let col = Options.color

let rec singleFace w h = function
    | ArrowSlit -> [ Block(col, V3(0.f, 0.f, 1.f), V3(1.f, 1.f, 0.1f)) ]
    | _ ->
        [ Block(col, V3(0.f, 0.f, 1.f), V3(1.f, 0.1f, 0.1f))
          Block(col, V3(0.f, 0.9f, 1.f), V3(1.f, 0.1f, 0.1f))
          Block(col, V3(0.f, 0.1f, 1.f), V3(0.6f, 0.1f, 0.2f))
          Block(col, V3(0.f, 0.8f, 1.f), V3(0.6f, 0.1f, 0.2f))
          Block(col, V3(-0.7f, 0.f, 1.f), V3(0.3f, 1.0f, 0.1f))
          Block(col, V3(0.7f, 0.f, 1.f), V3(0.3f, 1.0f, 0.1f)) ]


let flag pos = [
    yield Block(Color4.Black, pos, Vector3(0.03f, 1.f, 0.03f))
    yield Polygon(
       let a = [ Color4.Red, pos + Vector3(0.f, 0.5f, 0.f), Vector3.UnitZ
                 Color4.Red, pos + Vector3(0.7f, 0.5f, 0.f), Vector3.UnitZ
                 Color4.Red, pos + Vector3(0.f, 1.0f, 0.f), Vector3.UnitZ ] in a @ a) ]

let toPolySegments (pos:Vector3) =
    let makepoly n h style =
        [ for i in 0 .. n - 1 do
            let l = Matrix4.CreateTranslation pos
            let m = Matrix4.CreateScale(tan (3.1416f / float32 n), 1.0f, 1.0f)
            let n = Matrix4.CreateRotationY(float32 i * float32 Math.PI * 2.0f / float32 n)
            let f = singleFace 1 1 [Window;ArrowSlit].[(i + h) % 2]
            yield Offset(m * n * l, f) ]
    function
    | Spire(n, height, style) ->
        let flag = match style with | SpireStyle.ConeNoFlag -> (fun a -> []) | _ -> flag
        let outline = match style with
            | SpireStyle.Cap -> [2.f; 1.f]
            | SpireStyle.Cone -> [2.f; 1.6f; 1.2f; 0.8f; 0.4f; 0.01f]
            | SpireStyle.Onion -> [2.f; 3.f; 3.f; 2.7f; 2.f; 1.5f; 1.f; 0.6f; 0.3f; 0.1f; 0.01f]
            | _ -> [2.f; 2.f]
        let outline = List.pairwise outline
        if style = SpireStyle.Top then
            let width, rr = 1.4f * tan (3.14159f / float32 n), 1.4f / cos (3.1416f / float32 n)
            [ yield! flag (pos + V3(0.f, 1.2f, 0.f))
              yield NCylinder(col, pos, n, rr, rr, 1.2f)
              for angle in 0..n-1 do
                  let points = [for i in 0.2f - width .. 0.4f .. width -> Block(col, pos + V3(i, 1.2f, 1.4f), V3(0.15f, 0.25f, 0.1f))]
                  yield Offset(Matrix4.CreateRotationY(6.2832f * float32 angle / float32 n), points) ]
        else flag (pos + V3(0.f, (float32 (outline.Length) / 3.f), 0.f)) @ [for i, (r1,r2) in Seq.indexed outline -> NCylinder(Options.color2, pos + Vector3(0.f, float32 i / 3.f, 0.f), n, 0.6f * r1, 0.6f * r2, 0.3333f)]
    | Segment(n, h, style) -> makepoly n h style

let rec toPolygons =
    let foldwith f start = List.mapFold f start >> fst >> List.concat
    let buildtower = foldwith (fun p s -> toPolySegments p s, p + Vector3.UnitY) Vector3.Zero
    function
    | Normal(segments) -> buildtower segments
    | Tiered(segments) -> let do_offset p x = let rr = 0.9f ** (p - 1.0f) in scale3 rr 1.f rr x |> offset 0.f p 0.f
                          foldwith (fun p s -> toPolySegments Vector3.Zero s |> List.map (do_offset p), p + 1.0f) 1.0f segments
    | Capped(segments, style) ->
        let n = match segments.[0] with Segment(n,_,_) -> n | _ -> 8
        let top = Spire(n, 0.3f, style)
        buildtower (segments @ [top])
    | Buttress(tower, butts) -> [
            for i in [1.f;-1.f] do for j in [1.f;-1.f] do
                yield Offset(Matrix4.CreateScale(0.2f, 1.0f, 0.2f) * Matrix4.CreateTranslation(i, 0.f, j), toPolygons butts.[0])  ] @ toPolygons tower
    | Foundation(tower) ->
          Offset(Matrix4.CreateTranslation(0.f, 1.f, 0.f), toPolygons tower) ::
          Offset(Matrix4.CreateScale(1.5f, 2.0f, 1.5f), (Block(col, Vector3.Zero, Vector3.One) |> Base.crenelate2 (0.2f, 0.07f, 0.2f))) :: []


let rec create foobar =
    let stories = r.Next(4, 8)
    let segments = generateSegments (Options.sides) stories
    let tower = match r.Next(3) with
        | 20 -> Tiered(segments)
        | 10 -> Normal(segments)
        | _ -> Capped(segments, Options.topstyle ())
    let tower = match r.Next(4) with
        | 0 -> Buttress(tower, [Capped(generateSegments 4 3, SpireStyle.ConeNoFlag)])
        | 1 -> Foundation(tower)
        | _ -> tower
    tower |> toPolygons |> List.map (scale 0.08f >> scale3 Options.radius 1.f Options.radius)
