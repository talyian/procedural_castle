module ProcCastle.Trees
open ProcCastle.Base
open OpenTK
open OpenTK.Graphics

type Tree =
    | Leaf of Vector3
    | Branch of Vector3 * float32 * (float32 * Tree) list

let rec generate = function
    | 0 -> Leaf(Vector3.One)
    | n -> let pos = 0.6f * Vector3.Normalize (r.NextV3(-0.5f, 0.5f, 0.2f, 0.6f, -0.5f, 0.5f))
           Branch(pos, 0.03f * float32 n, [for i in 0..r.Next(2, 6) -> r.NextF(0.2f, 1.0f), generate (n-1)])

let ratio n = function | Leaf(n) -> 1.0f | _ -> n
let rec private _toPolygon (mat:Matrix4) = function
    | Leaf(dim) ->
        [ Offset(mat, [Block(Color4.DarkGreen, Vector3.Zero, Vector3(0.5f, 0.5f, 0.5f))
                       Block(Color4.DarkGreen.Mult(0.5), Vector3.One * 0.3f, Vector3(0.5f, 0.5f, 0.5f))])]
    | Branch(vec, width, children) ->
        let b = Vector3.Cross(Vector3.UnitY, vec)
        let rot = Matrix4.CreateFromAxisAngle(b, Vector3.CalculateAngle(Vector3.UnitY, vec))
        let c = if b.Length > 0.001f then rot * Matrix4.CreateScale (vec.Length) else Matrix4.Identity
        let cc = List.collect (fun (b, a) -> _toPolygon (Matrix4.CreateTranslation (4.0f * b * Vector3.UnitY)) a) children
        [ Offset(c * mat, Block(Color4.Bisque.Mult(0.2), Vector3.Zero,  Vector3(width, 4.0f, width)) :: cc)]
let toPolygon t = match _toPolygon Matrix4.Identity t with | [ Offset(c, f) ] -> f | n -> n

let create = generate >> toPolygon
