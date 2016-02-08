module ProcCastle.Main

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

open System
open ProcCastle.Base

let r, xz = new Random(), Vector3(1.0f, 0.0f, 1.0f)

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

let stack n ratio block = [
    let mutable col, pos, vol = match block with | Block(c, p, v) -> c, p, v | _ -> failwith "?"
    for i in 1..n do
        yield Block(col, pos, vol)
        pos.Y <- pos.Y + vol.Y
        vol.X <- vol.X * ratio
        vol.Z <- vol.Z * ratio
]

module Columns =
    let dimensions = function | Block(c,a,b) -> a,b | _ -> failwith "x"
    let r = new Random()
    let cylcap  c wr hr pos (dim:Vector3) func = [
        let s, p, d = 1.2f, pos + Vector3(0.0f, hr, 0.0f), Vector3(dim.X * wr, dim.Y - 2.0f * hr, dim.Z * wr)
        yield Cylinder(c, pos, s * dim.X, s * dim.Z, hr)
        yield Cylinder(c, pos + Vector3(0.f, dim.Y - hr, 0.f), s * dim.X, s * dim.Z, hr)
        yield! func p d ]
    let cubecap c wr hr pos (dim:Vector3) func = [
        let p, d = pos + Vector3(0.0f, hr, 0.0f), Vector3(dim.X * wr, dim.Y - 2.0f * hr, dim.Z * wr)
        yield Block(c, pos + Vector3(0.0f, dim.Y - hr, 0.0f), Vector3(dim.X, hr, dim.Z))
        yield Block(c, pos, Vector3(dim.X, hr, dim.Z))
        yield! func p d ]
    let cap a = match r.Next(0, 2) with | 0 -> cubecap a | _ -> cylcap a

    let rec column c pos (dim:Vector3) n = [
        match 0 with
        | 0 -> yield Cylinder(c, pos, dim.X, dim.X, dim.Y)
        // | 1 -> yield RidgedCylinder(c, pos, dim.X, dim.Y)
        // | 3 -> // double-capped cylinder
        //     yield! cap c 0.8f 0.2f pos dim (fun p d -> column c p d 1)
        | _ -> // capped ridged cylinder
            yield! cap c 0.8f 0.2f pos dim (fun p d -> column c p d 0)
    ]

    let wall c pos (dim:Vector3) spacing = [
        yield Offset(Matrix4.CreateScale(0.1f, 0.1f, 0.1f),
                     [ let s = r.Next(2, 5)
                       for i in 0.0f .. spacing .. dim.X do
                       let pp = pos + Vector3(i, 0.f, 0.f)
                       let d = Vector3(0.3f, dim.Y, 0.3f)
                       yield! column c pp d s ])
    ]

let towerCell c pos (dim:Vector3) = [
    let bb = BoundingBox.FromBox (pos, dim * Vector3(1.f, 0.5f, 1.f))
    let dy = bb.Dimensions.Y * 0.2f
    let dx = bb.Dimensions.X * 0.3333f
    let dim2 = Vector3(dim.X, dy* 0.5f, dim.Z)
    let dim3 = Vector3(dy * 0.5f, dim.Y, dim.Z)
    for y in bb.Minimum.Y .. dy .. bb.Maximum.Y do
        yield Block(c, Vector3(pos.X, y, pos.Z), dim2)
    for x in bb.Minimum.X .. dx .. bb.Maximum.X do
        yield Block(c, Vector3(x, pos.Y, pos.Z), dim3)
]
let rec towerlevel style pos (dim:Vector3) = [
    let col = Color4.Gray
    let cc = Vector3(0.03f, dim.Y, 0.03f)
    match style with
    // columnar layout
    | 1 -> let pts = [for p in Primitive.ngon 6 -> pos + p * dim.X]
           for pt in pts do yield! Columns.column col pt cc 3
           yield Cylinder(col, pos, dim.X, dim.X, dim.Y * 0.1f)
           yield Cylinder(col, pos + Vector3(0.f,dim.Y * 0.9f, 0.f), dim.X, dim.X, dim.Y * 0.1f)
    // Blocky Layout
    | 2 -> yield! towerCell col pos dim
    | 3 -> yield! towerCell col pos dim
    | 4
    | 5 -> yield! towerCell col pos dim
           yield! Columns.column col (pos + Vector3(dim.X, 0.f, dim.Z)) cc 3
           yield! Columns.column col (pos + Vector3(-dim.X, 0.f, dim.Z)) cc 3
           yield! Columns.column col (pos + Vector3(dim.X, 0.f, -dim.Z)) cc 3
           yield! Columns.column col (pos + Vector3(-dim.X, 0.f, -dim.Z)) cc 3
    | _ -> yield! Block(col, pos, dim)|> crenelate  //|> crenelate2 (0.2f, 0.06f, 0.1f))
]

let placeCap color capstyle (pos:Vector3) (dim:Vector3) = [
    let makespire pts = [
        for i, (r1,r2) in Seq.indexed <| Seq.pairwise pts do
            yield Cylinder(color, (Vector3(0.f, float32 (i) / 6.0f, 0.f) * dim), r1 * dim.X, r2 * dim.X, dim.Y / 6.0f) |> scale 1.2f |> offset pos.X pos.Y pos.Z
    ]
    match capstyle with
    | 0 -> yield! crenelate (Block(Color4.DarkGray, pos, dim))
    | 1 -> yield! makespire [0.8f; 1.0f; 0.8f; 0.6f; 0.5f; 0.35f; 0.25f; 0.12f; 0.01f]
    | _ -> yield! makespire [0.6f; 0.9f; 1.0f; 0.9f; 0.7f; 0.5f; 0.3f; 0.1f; 0.01f]
]
let flag pos = [
    yield Block(Color4.Black, pos, Vector3(0.03f, 1.f, 0.03f))
    yield Polygon(
       [ Color4.Red, pos + Vector3(0.f, 0.5f, 0.f), Vector3.UnitZ
         Color4.Red, pos + Vector3(0.7f, 0.5f, 0.f), Vector3.UnitZ
         Color4.Red, pos + Vector3(0.f, 1.0f, 0.f), Vector3.UnitZ
         Color4.Red, pos + Vector3(0.f, 0.5f, 0.f), Vector3.UnitZ
         Color4.Red, pos + Vector3(0.7f, 0.5f, 0.f), Vector3.UnitZ
         Color4.Red, pos + Vector3(0.f, 1.0f, 0.f), Vector3.UnitZ ])
]

let placeTower (c:Random) color style segments pos =
    [
    let mutable pos = pos
    let mutable dim = Vector3(0.13f, 0.22f, 0.13f)
    for i in 1..segments do
        yield! towerlevel (c.Next(0, 6)) pos dim
        pos <- pos + Vector3(0.f, dim.Y, 0.f)
        dim <- Vector3(0.9f * dim.X, dim.Y, 0.9f * dim.Z)
    yield! placeCap color style pos dim
    pos <- pos + Vector3(0.f, dim.Y, 0.f)
    dim <- Vector3(0.9f * dim.X, dim.Y, 0.9f * dim.Z)
    yield! flag Vector3.Zero |> Seq.map (scale 0.2f)
                             |> Seq.map(offset pos.X pos.Y pos.Z) ]

// Place a wall between two points
let placeWall (r:Random) h d (start,stop) = [
    let v = Vector3.Subtract(stop, start)
    let p = Vector3(v.Length, h, d)
    let c = Color4.DarkSalmon.Mult (0.7 + r.NextDouble() * 0.3)
    let angle = atan2 v.Z v.X
    yield (Offset(Matrix4.Identity, crenelate (boundedBlock c Vector3.Zero p))
           |> matrix (Matrix4.CreateRotationY(-angle))
           |> offset start.X start.Y start.Z)]

let placeTree (r:Random) (pos:Vector3) (vol:Vector3) = [
    yield Block(Color4.Brown.Mult(0.3), pos, Vector3(0.1f, 1.f, 0.1f) * vol)
    for i in 3..9 do
        let dim = r.NextV3() * vol * 0.2f + vol * 0.3f
        let center = pos + r.NextV3() * (vol - dim) - (vol - dim) * 0.5f + Vector3(0.f, dim.Y * 0.8f, 0.f)
        yield Block(Color4.Green.Mult(r.NextDouble()), center, dim) ]

let placeField (r:Random) (pos:Vector3) (vol:Vector3) = [Block(Color4.DarkGreen, pos, vol * Vector3(1.0f, 0.1f, 1.0f))]
let placeGarden (r:Random) (pos:Vector3) (vol:Vector3) = [Block(Color4.LightGreen, pos, vol * Vector3(1.0f, 0.1f, 1.0f))]
let placeHut (r:Random) (pos:Vector3) (vol:Vector3) = [
    let color () = Color4.Brown.Mult(0.3).Add(Color4.Red.Mult(r.NextDouble()*0.3)).Add(Color4.Yellow.Mult(r.NextDouble()*0.3))
    let roof (pos:Vector3) (vol:Vector3) = Block((color()), pos + vol * Vector3.UnitY, vol * Vector3(1.0f, 0.1f, 1.0f)) |> stack 4 0.8f
    let body pos (vol:Vector3) = Block(Color4.Gray.Mult(0.3), pos, 0.95f * vol)
    let beroof s = s |> Seq.collect (fun (p,v) -> [yield body p v; yield! roof p v])
    match r.Next(3) with
    | 0 -> yield! beroof [pos, 0.7f * vol; pos + Vector3(1.0f, 0.f, 0.f) * vol, vol * 0.5f]
    | 1 -> yield! beroof [pos, vol; pos + Vector3(0.1f, 0.f, 1.f) * vol, vol * 0.4f]    
    | _ -> yield! beroof [pos, vol]
]

let placeKeep (r:Random) (pos:Vector3) = [
    let vol = r.NextV3(0.2f, 0.3f, 0.3f, 0.5f, 0.2f, 0.3f)
    let col = Color4.Beige.Mult(0.4).Add(Color4.Red.Mult(r.NextDouble() * 0.1)).Add(Color4.Green.Mult(r.NextDouble() * 0.1))
    // main body
    match r.Next(3) with
        | 0 -> let vol = Vector3(vol.X, vol.Y, vol.Z * 1.f)
               yield! Block(col, pos, vol * Vector3(1.0f, 0.7f, 0.7f)) |> crenelate
               yield! Block(col, pos + vol * Vector3(-1.f, 0.f, 0.f), vol * Vector3(0.5f, 1.f, 1.f)) |> crenelate
               yield! Block(col, pos + vol * Vector3(1.f, 0.f, 0.f), vol * Vector3(0.5f, 1.f, 1.f))    |> crenelate
               yield! placeTower r Color4.DarkOliveGreen 1 2 (pos + vol * Vector3.UnitY)
        | 1 -> yield! Block(col, pos, vol) |> stack (r.Next(1, 3)) 0.6f |> List.collect crenelate
               yield! Block(col, pos, vol * Vector3(2.5f, 0.6f, 0.5f)) |> crenelate
               yield! Block(col, pos, vol * Vector3(0.5f, 0.6f, 1.5f)) |> crenelate    
               yield! Block(col, pos - 2.f * vol * Vector3.UnitX, vol * Vector3(0.5f, 0.7f, 0.7f)) |> crenelate
               yield! Block(col, pos + 2.f * vol * Vector3.UnitX, vol * Vector3(0.5f, 0.7f, 0.7f)) |> crenelate
               let towerw = r.NextF(0.05f, 0.08f)
               for x in [1.f; -1.f] do
                 for z in [1.f; -1.f] do
                   yield! Block(col.Mult(0.5), pos + vol * Vector3(x, 0.f, z), (Vector3(towerw, vol.Y + 0.1f, towerw))) |> crenelate
        | _ -> yield! Block(col, pos, vol) |> stack (r.Next(1, 4)) 0.6f |> List.collect crenelate
               let towerw = r.NextF(0.05f, 0.18f)
               for x in [1.f; -1.f] do
                 for z in [1.f; -1.f] do
                   yield! Block(col.Mult(0.5), pos + vol * Vector3(x, 0.f, z), (Vector3(towerw, vol.Y + 0.1f, towerw))) |> crenelate                                
               yield! Block(col, pos, (Vector3(0.2f, vol.Y * 0.4f, vol.Z * 1.5f))) |> crenelate
]    

let placeMoat (r:Random) (points:Vector3 list) = [
    let noisy m f s = [for p:Vector3 in s -> p * (r.NextF(m, f))]
    let subdivsmooth s = Seq1.pairwisec s |> Seq.collect (fun (a:Vector3,b:Vector3) -> [(2.0f*a+b)*0.33f; (a + 2.0f*b) * 0.33f]) |> Seq.toList
    let points2 = points |> noisy 1.1f 1.3f |> subdivsmooth |> noisy 1.0f 1.1f |> subdivsmooth  |> subdivsmooth |> List.map ((+) (Vector3(0.f, -0.09f, 0.f)))
    let points3 = points |> noisy 1.4f 1.6f |> subdivsmooth |> noisy 1.f 1.3f |> subdivsmooth  |> subdivsmooth |> List.map ((+) (Vector3(0.f, -0.09f, 0.f)))
    yield Bridged(Color4.DarkGreen, points, points2)
    yield Bridged(Color4.DarkBlue, points2, points3)
]

let placeCourtyard (r:Random) (pos:Vector3) (dim:Vector3) = [
    let bb = BoundingBox(Vector3.Zero, dim)
    let block c x y i j h = Block(c, bb.Minimum + 0.5f * bb.Dimensions * Vector3(x + i, 0.f, y + j),
                                bb.Dimensions * 0.5f * (Vector3(i - x, h, j- y)))
    let b =
      [ // yield block Color4.Green 0.f 0.f 1.f 1.f 0.01f
        for i in 0..10 do
            let x = float32 (r.Next(10)) * 0.1f
            let y = float32 (r.Next(10)) * 0.1f
            yield block (Color4.Green.Mult(0.2)) x y (x + 0.1f) (y + 0.1f) 0.1f
            yield block (Color4.Green.Mult(0.25)) y x (x + 0.1f) (x + 0.1f) 0.1f            
      ]
    yield Offset(Matrix4.CreateTranslation pos,
                 placeTree r Vector3.Zero (Vector3(0.15f, 0.2f, 0.15f)) @
                 b @ List.map (scale3 -1.f 1.f 1.f) b
                   @ List.map (scale3 -1.f 1.f -1.f) b
                   @ List.map (scale3 1.f 1.f -1.f) b)
]

let castle seed = [
    let r = new Random(seed)
    [for i in 0..20 -> r.Next()] |> ignore
    let rdim () = r.NextV3(0.1f, 0.3f, 0.1f, 0.1f, 0.1f, 0.3f)
    let spacehash = new SpatialHash(0.03f * Vector3.One)
    let mbuildings n =
        let make i = BoundingBox.FromBox(r.NextV3() * xz, 0.5f * rdim())
        List.init n (make >> spacehash.PlaceNearest)
    let myard () = [BoundingBox.FromBox(Vector3.Zero, rdim() * 1.5f) |> spacehash.PlaceNearest]
    let courtyard = myard ()
    let buildings = mbuildings 5
    let mm = r.Next(1, 4)
    let keeps2 = List.init (r.Next(1, 4)) (fun i -> placeKeep r (r.NextV3() * xz))
    let keeps = keeps2 |> List.map (fun prims -> prims
                                                 |> Seq.collect (Primitive.toVertices)
                                                 |> Seq.map (fun vertex -> vertex.pos)
                                                 |> BoundingBox.FromPoints
                                                 |> spacehash.PlaceNearest)
    // let keeps = List.init mm (fun i -> BoundingBox.FromBox(r.NextV3() * xz, rdim() * 2.0f) |> spacehash.PlaceNearest)
    let courtyard = courtyard @ myard ()
    let courtyard = courtyard @ myard () @ myard()        
    let buildings = mbuildings (r.Next (15, 50))
    // Place external wall and towers on the convex hull of the internal structures
    let hull:Vector3 list =
        courtyard @ keeps @ buildings
        |> List.collect (fun bb -> [bb.Minimum;bb.Maximum;
                                    bb.Minimum+Vector3(bb.Dimensions.X, 0.f, 0.f);
                                    bb.Minimum+Vector3(0.f, 0.f, bb.Dimensions.Z)])
        |> ConvexHull.FromPoints
    let towers = [for p in hull -> Vector3(p.X * 1.1f, p.Y * 0.0f, p.Z * 1.1f)]
    yield! towers |> placeMoat r
    let style = r.Next(3)
    let capColor = Color4.Aquamarine.Mult(0.5).Add(Color4.Blue.Mult(r.NextDouble() * 0.9)).Add(Color4.Green.Mult(r.NextDouble() * 0.9))
    let capColor = capColor.Mult(0.7)
    yield! towers |> Seq.collect (fun x -> placeTower r capColor style (r.Next (1, 3)) x)
    yield! Seq.collect (placeWall r 0.45f 0.1f) (Seq1.pairwisec towers)
    yield! keeps |> Seq.collect (fun k -> placeKeep r k.BottomCenter) // (k.Dimensions * 0.5f))
    yield! courtyard |> Seq.collect (fun k -> placeCourtyard r k.BottomCenter (k.Dimensions * 0.5f))
    yield! buildings |> Seq.collect (fun k -> placeHut r k.BottomCenter (k.Dimensions * 0.5f))
    yield Block(Color4.DarkGreen.Mult(0.3), Vector3(0.0f, -0.11f, 0.0f), Vector3(10.f, 0.01f, 10.f))
]

[<EntryPoint>]
let main args =
    let r = new Random()
    castle 4 |> ProcCastle.Renderer._display
    0
