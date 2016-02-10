# A Procedural Castle in F# #

#### Introduction

![](castle.png)

My goals here were to implement, using F# and OpenTK, a procedural castle village generator.
The code should be as clean as you should expect from a month-long programming challenge
and should be simple, elegant, and powerful.

- F# - Ever wanted an indentation-based (like Python), object-oriented .NET (like C#) language with strong functional (like Haskell) roots?
- OpenTK - This is the go-to library for working with OpenGL on .NET. In addition to providing nearly 1-to-1 OpenGL bindings, it also provides convenience functions for creating a window and setting up the context, as well as Matrix and Vector classes.
- Blender - Even though I wanted to write a minimally useful in-engine renderer, there was going to be one feature (ambient occlusion and shadows) that I wasn't going to try to tackle, so I used Blender for a nicer render.

#### Example Generator: Trees

The following code shows the definition of a Tree, and the code used to generate a random tree, and convert it a series of polygons for rendering.

The body of the `generate` function is three lines: 1 to create a `Leaf` node, 1 to create a random point `pos`, and 1 and create a `Branch` ending on `pos`, recursively containing between 2 and 5 sub-branches.

The body of the `_toPolygon` function is only slightly more involved. This says tha teach `Leaf` node is drawn as two different dark-green `Block`s, offset by `0.3` along each axis.
This is just enough detail to appear slightly busy, presenting the illusion of leaves from far away.
The Branch part is slightly more involved. I have to calculate the rotation+scale matrix `c` based on the endpoint of the current branch, and recursively apply it to all child nodes.
After the recursive step, I have a list of child nodes, comprising of a combination of rectangular `Block` objects and `Offset` matrices representing a nested transformation.
Both `Block`s and `Offset`s are primitive types that my rendering function recognizes and can convert to a triangle array for rasterization.

    type Tree =
        | Leaf of Vector3
        | Branch of Vector3 * float32 * Tree list

    let rec generate = function
        | 0 -> Leaf(Vector3.One)
        | n -> let pos = 0.6f * Vector3.Normalize (r.NextV3(-0.5f, 0.5f, 0.2f, 0.6f, -0.5f, 0.5f))
               Branch(pos, 0.03f * float32 n, [for i in 0..r.Next(2, 6) -> generate (n-1)])

    let rec private _toPolygon (mat:Matrix4) = function
        | Leaf(dim) ->
            [ Offset(mat, [Block(Color4.DarkGreen, Vector3.Zero, Vector3(0.5f, 0.5f, 0.5f))
                           Block(Color4.DarkGreen.Mult(0.5), Vector3.One * 0.3f, Vector3(0.5f, 0.5f, 0.5f))])]
        | Branch(vec, width, children) ->
            let b = Vector3.Cross(Vector3.UnitY, vec)
            let rot = Matrix4.CreateFromAxisAngle(b, Vector3.CalculateAngle(Vector3.UnitY, vec))
            let c = if b.Length > 0.001f then rot * Matrix4.CreateScale (vec.Length) else Matrix4.Identity
            let cc = List.collect (_toPolygon (Matrix4.CreateTranslation Vector3.UnitY)) children
            [ Offset(c * mat, Block(Color4.Bisque.Mult(0.2), Vector3.Zero,  Vector3(width, 1.0f, width)) :: cc)]

    let toPolygon t = match _toPolygon Matrix4.Identity t with | [ Offset(c, f) ] -> f | n -> n

![](trees.png)
