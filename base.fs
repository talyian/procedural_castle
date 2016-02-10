module ProcCastle.Base

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

open System
open System.Runtime.InteropServices

module Seq1 =
    let pairwisec seq = Seq.zip seq (Seq.concat [seq; seq] |> Seq.tail)

type Primitives =
    | Block of Color4 * Vector3 * Vector3
    | Sphere of Color4 * Vector3 * float32
    | Cone of Color4 * Vector3 * float32 * float32
    | Extruded of Color4 * Vector3 list * Vector3
    | Bridged of Color4 * Vector3 list * Vector3 list
    | RidgedCylinder of Color4 * Vector3 * float32 * float32
    | Cylinder of Color4 * Vector3 * float32 * float32 * float32
    | NCylinder of Color4 * Vector3 * int * float32 * float32 * float32
    | Polygon of (Color4 * Vector3 * Vector3) list
    | Offset of Matrix4 * Primitives list

[<StructLayout(LayoutKind.Sequential)>]
type Vertex = struct
    val mutable col:Color4
    val mutable pos:Vector3
    val mutable nor:Vector3
    val mutable uv:Vector2
end

module Primitive =
    let ngon n = [
        let pipi = 6.28318f
        for angle in 0.0f .. 1.0f/float32 n .. 1.0f do
            let angle = 6.24318f * (angle  + 0.5f / float32 n)
            yield Vector3(sin angle, 0.0f, cos angle) ]

    let rec toPolygons = function
        | Block(c, pos, dim) ->
            let verts = [| for i in [-1.0f; 1.0f] do
                           for j in [0.0f; 1.0f] do
                           for k in [-1.0f; 1.0f] -> pos + Vector3(i,j,k) * dim |]
            let nor = [| -Vector3.UnitX; Vector3.UnitX;
                         -Vector3.UnitZ; Vector3.UnitZ;
                         -Vector3.UnitY; Vector3.UnitY |]
            let indices = [|0; 1; 2; 2; 3; 1; // bit 2 = 0
                           4; 5; 6; 6; 7; 5; // bit 2 = 1
                           0; 2; 4; 4; 6; 2; // bit 0 = 0
                           1; 3; 5; 5; 7; 3; // bit 0 = 1
                           0; 1; 4; 4; 5; 1; // bit 1 = 0
                           2; 3; 6; 6; 7; 3; // bit 1 = 1
                           |];
            [for ni, vi in Array.indexed indices -> c, verts.[vi], nor.[ni / 6]]
        | Sphere(c, pos, dim) -> toPolygons (Block(c, pos, Vector3(dim, dim, dim)))
        | Extruded(col, pts, vec) ->
            [  for a, b in Seq1.pairwisec pts do
               let nor = Vector3.Cross(b-a, vec).Normalized()
               yield! [col, a, nor;col, b, nor; col, a + vec, nor;
                      col, b, nor;col, a + vec, nor;col, b + vec, nor ]
            ]
        | Bridged(col, pts1, pts2) ->
            [ for (a,b),(c,d) in Seq.zip (Seq1.pairwisec pts1) (Seq1.pairwisec pts2) do
              let nor = if ((b:Vector3) - a).Length < 0.0001f then Vector3.UnitY else Vector3.Cross(b-a, c-a)
              yield! [col,a,nor;col,b,nor;col,c,nor;
                      col,b,nor;col,c,nor;col,d,nor]
            ]
        | RidgedCylinder(col, pos, r, h) ->
            let ratio = 0.3f
            let pts = [for i,a in List.indexed (ngon 20) -> pos + r * a * (0.8f + ratio * float32 (i % 2))]
            toPolygons (Extruded(col, pts, Vector3(0.f, h, 0.f)))
        | Cylinder(col, pos, r1, r2, h) -> toPolygons(NCylinder(col, pos, 8, r1, r2, h))
        | NCylinder(col, pos, n, r1, r2, h) ->
            let pts = [for p in ngon n -> pos + p * r1]
            let pts2 = [for p in ngon n -> pos + p * r2 + Vector3(0.f, h, 0.f)]
            [   let nor = -Vector3.UnitY
                yield! [for a,b in Seq1.pairwisec pts do yield! [col,a,nor;col,b,nor;col,pts.[0],nor]]
                yield! toPolygons (Bridged(col, pts, pts2))
                let nor = Vector3.UnitY
                yield! [for a,b in Seq1.pairwisec pts2 do yield! [col,a,nor;col,b,nor;col,pts2.[0],nor]]
                ]
        | Cone(col, pos, r, h) -> toPolygons (Cylinder(col, pos, r, 0.01f, h))
        | Polygon(x) -> x
        | Offset(matrix, p) ->[ for p in p do
                                for c, v, n in toPolygons p ->
                                  c, Vector3.Transform(v, matrix),
                                  Vector3.TransformNormal(n.Normalized(), matrix) ]

    let rec toVertices x = [| for c,v,n in toPolygons x -> new Vertex(col=c,pos=v,nor=n) |]

let inline boundedBlock c (min:Vector3) (max:Vector3) =
    let center = (min+max) / 2.0f
    Block(c, center * Vector3(1.0f, 0.0f, 1.0f), (max - min) / 2.0f)
let inline matrix m polygon = Offset(m, [polygon])
let inline offset x y z polygon = Offset(Matrix4.CreateTranslation(float32 x, float32 y, float32 z), [polygon])
let inline scale3 f g h = matrix (Matrix4.CreateScale(f, g, h))
let inline scale f = scale3 f f f

type Color4 with
    member this.Mult (m:float) = let n = float32 m in new Color4(this.R * n, this.G * n, this.B * n, this.A * n)
    member this.Add (o:Color4) = new Color4(this.R + o.R, this.G + o.G, this.B + o.B, (this.A + o.A) * 0.5f)
type System.Random with
    member this.NextF () = float32 (this.NextDouble ())
    member this.NextF (min, max) = float32 (this.NextDouble ()) * (max - min) + min
    member this.NextV3 () = Vector3(this.NextF(), this.NextF(), this.NextF())
    member this.NextV3 (xmin, xmax, ymin, ymax, zmin, zmax) = Vector3(this.NextF(xmin, xmax), this.NextF(ymin, ymax), this.NextF(zmin, zmax))
    member this.NextEnum<'T>() = let a = Enum.GetValues(typeof<'T>) in a.GetValue(this.Next() % a.Length) :?> 'T

type ConvexHull () =
    static member FromPoints (pts:Vector3 list) =
        let mutable firstpt = pts |> Seq.minBy (fun v -> v.X)
        let inline turn (a:Vector3) (b:Vector3) (c:Vector3) = (b.X - a.X) * (c.Z - a.Z) - (c.X - a.X) * (b.Z - a.Z)
        let nextpt pt =
            List.filter ((<>) pt) pts
            |> List.reduce (fun p q -> if turn pt p q < 0.0f then q else p)
        firstpt :: List.unfold (fun p -> if p = firstpt then None else Some(p, nextpt p)) (nextpt firstpt)

[<Struct>]
type BoundingBox (min: Vector3, max: Vector3, b:bool) =
    static let minf a b = Vector3.ComponentMin(a,b)
    static let maxf a b = Vector3.ComponentMax(a,b)

    static member FromBox(center:Vector3, dimensions:Vector3) =
        let dim = Vector3(abs dimensions.X, abs dimensions.Y, abs dimensions.Z)
        let min = center - dimensions * Vector3(1.0f, 0.0f, 1.0f)
        let max = min + 2.0f * dimensions
        BoundingBox(min, max)
    static member FromPoints (pts:seq<Vector3>) = BoundingBox(Seq.reduce minf pts, Seq.reduce maxf pts)
    static member (|||) (a:BoundingBox, b:BoundingBox) = BoundingBox(minf (a.Minimum) (b.Minimum), maxf (a.Maximum) (b.Maximum))
    static member (|||) (a:BoundingBox, p:Vector3) = BoundingBox(minf (a.Minimum) (p), maxf (a.Maximum) (p))
    static member (&&&) (a:BoundingBox, p:Vector3) = BoundingBox(maxf (a.Minimum) (p), minf (a.Maximum) (p))
    static member (&&&) (a:BoundingBox, b:BoundingBox) = BoundingBox(maxf (a.Minimum) (b.Minimum), minf (a.Maximum) (b.Maximum))
    new(a:Vector3 ,b:Vector3) = BoundingBox(minf a b, maxf a b, true)
    member this.Volume = let v:Vector3 = max - min in v.X * v.Y * v.Z
    member this.Minimum = min
    member this.Maximum = max
    member this.Center = (min + max) * 0.5f
    member this.BottomCenter = Vector3((min.X + max.X) * 0.5f, min.Y, (min.Z + max.Z) * 0.5f)
    member this.Dimensions = max - min
    override this.ToString() =
        sprintf "[%8x] %O - %O" (hash this) (this.Minimum) (this.Maximum)
module BoundingBoxExtensions =
    type BoundingBox with
        member this.Split ratio axis =
            let min = this.Minimum
            let max = this.Maximum
            let p, q, w = ratio, 1.0f - ratio, max - min
            let mutable min1, min2, max1, max2 = min, min, max, max
            match axis with
                | 0 -> max1.X <- p * w.X; min2.X <- p*w.X
                | 1 -> max1.Y <- p * w.X; min2.Y <- p*w.Y
                | _ -> max1.Z <- p * w.Z; min2.Z <- p*w.Z
            [BoundingBox(min1, max1); BoundingBox(min2, max2)]
open BoundingBoxExtensions

type SpatialHash (blocksize:Vector3) =
    let dx, dy, dz = blocksize.X, blocksize.Y, blocksize.Z
    let sectors = new Collections.Generic.Dictionary<Vector3, _>()

    member this.GetSectors (bb:BoundingBox) = seq {
        let range a b d = seq { floor (a / d) .. 1.0f .. (b / d) + 0.99999f }
        for x in range bb.Minimum.X bb.Maximum.X dx do
        for y in 0.f .. 0.f do
        for z in range bb.Minimum.Z bb.Maximum.Z dz do
            yield Vector3(dx * x, dy * y, dz * z) }

    member this.GetPossibleIntersections (bb:BoundingBox) = seq {
         for sector in this.GetSectors bb do
         match sectors.TryGetValue(sector) with
         | true, v -> yield 1
         | false, _ -> ()
    }

    member this.Occupied (bb:BoundingBox) =
        let mutable n, m = bb.Minimum, bb.Maximum
        n.Y <- 0.f
        m.Y <- 0.f
        let rec loopX x y z =
            if x > m.X / dx + 0.999f then loopX (floor(n.X / dx)) (y + 1.0f) z
            elif y > m.Y / dy + 0.999f then loopX x (floor (n.Y / dy)) (z + 1.0f)
            elif z > m.Z / dz + 0.999f then false
            elif sectors.ContainsKey (Vector3(dx * x,dy * y,dz * z)) then true
            else loopX (x + 1.0f) y z
        loopX (floor(n.X/dx)) (floor(n.Y/dy)) (floor(n.Z/dz))

    member this.Put (bb:BoundingBox) =
        for s in this.GetSectors bb do
            match sectors.TryGetValue(s) with
                | false, _ -> sectors.[s] <- 1
                | true, x -> sectors.[s] <- sectors.[s] + 1

    member this.PlaceNearest (bb:BoundingBox) =
        let pos = bb.Minimum
        let candidates = seq {
            for radius in Seq.initInfinite id do
            for x in -radius .. radius do
            for z in [radius - abs x;abs x - radius] do
            yield Vector3(pos.X + dx * float32 x, pos.Y, pos.Z + dz * float32 z) }
        let tryPlace p =
            let _bb = BoundingBox(p, p + bb.Dimensions)
            if this.Occupied _bb then None
            else this.Put _bb; Some(_bb)
        Seq.pick tryPlace candidates

module OBJ =
    let writecolors (output:System.IO.TextWriter) (colors:seq<Color4>) =
        for c in colors do
            output.WriteLine("newmtl m{0:x}", c.ToArgb())
            output.WriteLine("Kd {0} {1} {2}", c.R, c.G, c.B)
        output.Flush()

    let write (output:System.IO.TextWriter) primitives =
        let vertices = new ResizeArray<Vector3>()
        let colors = new ResizeArray<_>()
        for i,(c,v,n) in Seq.indexed (Seq.collect Primitive.toPolygons primitives) do
            vertices.Add(v)
            colors.Add((c, i))
        for v in vertices do output.WriteLine("v {0} {1} {2}", v.X, v.Y, v.Z);
        for color, faces in colors |> Seq.groupBy fst do
            let faces = Seq.toArray (Seq.map snd faces)
            output.WriteLine("usemtl m{0:x}", color.ToArgb())
            for i in 0..3..faces.Length-1 do
                output.WriteLine("f {0} {1} {2}", faces.[i]+1, faces.[i+1]+1, faces.[i+2]+1)
        output.Flush()
        Seq.map fst colors |> Seq.groupBy id |> Seq.map fst |> Array.ofSeq

    let writeFile str prims =
        use a = IO.StreamWriter(str + ".obj")
        use b = IO.StreamWriter(str + ".mtl")
        write a prims |> writecolors b

let r = new Random()
let crenelate2 (dx,wx,h) block = [
    let col, pos, vol = match block with | Block(c, p, v) -> c, p, v | _ -> failwith "?"
    yield Block(col, pos, vol)
    for u,v in [for v in [pos.Z - vol.Z; pos.Z + vol.Z] do
                for u in pos.X - vol.X + wx .. dx .. (pos.X + vol.X - wx) -> u, v] @
               [for u in [pos.X - vol.X; pos.X + vol.X] do
                for v in pos.Z - vol.Z + wx .. dx .. (pos.Z + vol.Z - wx) -> u, v] do
        yield Block(col, Vector3(u, pos.Y + vol.Y, v), Vector3(wx, h, wx))
]
let crenelate = crenelate2 (0.06f, 0.018f, 0.03f)
