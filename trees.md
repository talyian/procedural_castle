---
title: Introduction
---
# A Procedural Castle in F# #

### Introduction

![](http://i.imgur.com/A13MyOt.png)

My goals here were to implement, using F# and OpenTK, a procedural castle village generator.
The code should be as clean as you should expect from a month-long programming challenge
and should be simple, elegant, and powerful.

- F# - Ever wanted an indentation-based (like Python), object-oriented .NET (like C#) language with strong functional (like Haskell) roots?
- OpenTK - This is the go-to library for working with OpenGL on .NET. In addition to providing nearly 1-to-1 OpenGL bindings, it also provides convenience functions for creating a window and setting up the context, as well as Matrix and Vector classes.
- Blender - Even though I wanted to write a minimally useful in-engine renderer, there was going to be one feature (ambient occlusion and shadows) that I wasn't going to try to tackle, so I used Blender for a nicer render.

### Notes on the high-level design of this system

#### The Frontend

The "front end" of the generator are a set of functions that generate `Tree`s, `Tower`s, `Column`s, etc. As you will
see from the tree example below, the front end is concerned purely with construction rules. For example, the three
basic rules for producing a tower are:

* a tower is formed of between 3 and 6 stories
* a tower has a 'cap' structure on top
* a tower could have adjoining buttresses or a risen foundation layer

In code, this looks like:

    let rec create () =
        let stories = rand.Next(4, 8)
        let segments = generateSegments (Options.sides) stories
        let tower = Capped(segments, Options.topstyle ())
        let tower = match rand.Next(4) with
            | 0 -> Buttress(tower, [Capped(generateSegments 4 3, SpireStyle.ConeNoFlag)])
            | 1 -> Foundation(tower)
            | _ -> tower

#### The Backend

Ultimately all the high-level types like `Tower` get converted into a set of primitive types -- cylinders, blocks, vertex-lists,
etc, in the backend understands. This provides a uniform representation of objects. Utility classes such as bounding-boxes
and spatial hashes only ever need to deal with "boxes" and "lists of vertexes" instead of having separate logic
for each object they interact with.

##### Backend Logic Example: the Convex Hull

![](http://i.imgur.com/kXVftTZ.png)

One example of backend logic is calculating the convex hull of a given set of points. This
is useful because we first generate a bunch of structures, and need to figure out
where to build the walls so that they surround all the structures we've built. The algorithm implemented here
is the Graham scan, an `O(n log n)` complexity algorithm. To understand this function, it may be
helpful to translate it piece-by-piece into English.

English  | F#
--- | ---
Start with the leftmost point | `let firstpt = List.minBy (fun (v:Vector3) -> v.X) points`
To find the next point, take all the other points ... | `let nextpt pt = List.except [pt] points |>`
and find the one with the smallest counterclockwise angle | `List.reduce (fun p q -> if Matrix2(p.Xz-pt.Xz,q.Xz-pt.Xz).Determinant < 0.0f then q else p)`
Keep taking the next point until you find the first point again | `List.unfold (fun p -> let n = nextpt p in if n = firstpt then None else Some(p, n)) firstpt`

    module ConvexHull =
        let FromPoints points =
            let firstpt = List.minBy (fun (v:Vector3) -> v.X) points
            let nextpt pt =
                List.except [pt] points |> List.reduce (fun p q -> if Matrix2(p.Xz-pt.Xz,q.Xz-pt.Xz).Determinant < 0.0f then q else p)
            List.unfold (fun p -> let n = nextpt p in if n = firstpt then None else Some(p, n)) firstpt

#### Backend Logic Notes: The placement algorithm

I used a spatial hash to detect collisions when placing objects. When placing a new object, we
check for collisions, spiraling outwards from the initial point until we find a location. Not too much to say here,
other than if i had more time, I would have preferred using some kind of tree structure instead for more flexibility.
Since I wasn't comparing object-object collisions directly, I could only resolve collisions at the size of a spatial hash cell.
This led to a tradeoff between hash resolution (lower resolution means larger gaps between objects) and performance (each insertion
and collision check is O(V) where V is the volume of the inserted object)

#### Example Generator: Trees

A tree is one of the most common recursively generated objects out there. There is a fundamental fractal nature about trees that makes it very suitable for generation algorithms.

    type Tree =
        | Leaf of Vector3
        | Branch of Vector3 * float32 * Tree list

The body of the `generate` function is three lines: 1 to create a `Leaf` node, 1 to create a random point `pos`, and 1 and create a `Branch` ending on `pos`, recursively containing between 2 and 5 sub-branches.

    let rec generate = function
        | 0 -> Leaf(Vector3.One)
        | n -> let pos = 0.6f * Vector3.Normalize (r.NextV3(-0.5f, 0.5f, 0.2f, 0.6f, -0.5f, 0.5f))
               Branch(pos, 0.03f * float32 n, [for i in 0..r.Next(2, 6) -> generate (n-1)])

The body of the `_toPolygon` function is below. Leaves are converted to dark green blocks.
The Branch part is slightly more involved. We find the rotation+scale matrix `c` based
on the endpoint of the current branch, and recursively apply it to all child nodes.
After the recursive step, I have a list of child nodes, comprising of a combination of rectangular
`Block` objects and `Offset` matrices representing a nested transformation.
Both `Block`s and `Offset`s are primitive types that my rendering function recognizes and can convert to
a triangle array for rasterization.

    let rec private _toPolygon (mat:Matrix4) = function
        | Leaf(dim) -> [ Offset(mat, [Block(Color4.DarkGreen, Vector3.Zero, dim) ])]
        | Branch(vec, width, children) ->
            let b = Vector3.Cross(Vector3.UnitY, vec)
            let rot = Matrix4.CreateFromAxisAngle(b, Vector3.CalculateAngle(Vector3.UnitY, vec))
            let c = if b.Length > 0.001f then rot * Matrix4.CreateScale (vec.Length) else Matrix4.Identity
            let cc = List.collect (fun (b, a) -> _toPolygon (Matrix4.CreateTranslation (4.0f * b * Vector3.UnitY)) a) children
            [ Offset(c * mat, Block(Color4.Bisque.Mult(0.2), Vector3.Zero,  Vector3(width, 4.0f, width)) :: cc)]
    let toPolygon t = match _toPolygon Matrix4.Identity t with | [ Offset(c, f) ] -> f | n -> n

![](http://i.imgur.com/6HNMMqI.png)
![](http://i.imgur.com/hl5efc7.png)

### Finally: A note on (un)randomness

Procedural Generation is about controlled chaos. Instead of manually designing individual elements, we
have to specify a design space of what kinds of structure the program randomly generates. This theme
came up multiple times during the course of this project. A couple examples:

1) I put off implementing the spatial hash until halfway through the project. Randomly placed Points (https://lucyuki.files.wordpress.com/2015/03/screen-shot-2015-02-16-at-4-56-34-pm.png) aren't really interesting in the long run. All the interesting details about placement happen around questions like "what is the minimum gap between two adjacent objects?" "Do nearby objects influence each other's orientation?" "do objects of similar size cluster together?" In the end, I found a pretty dumb generation strategy (generate large flat areas first, then add a few small objects to break up the space, then add large castle structures, then fill in the gaps with a final set of smaller objects" that yielded the results I wanted -- busy, but not too crowded. Since each new object had to align with at least one previously place object, the smaller units tended to clump and arrange themselves slightly on a grid.

2) My initial stab at random towers was terrible -- when you choose a rule that says "pick 5 random tower levels and stack them on top of each other", you get what you'd expect -- a tower of barely related objects stacked on top of each other. In the final iteration, I redid the tower grammar to simply generate an N-sided cylinder and simply varied the style of the top and large-scale decorations. While this was more coherent, there should be even better rules available for generating structures closer to what you'd see in real life.

![](http://i.imgur.com/cwlIORm.png)

All in all, This was quite a fun project. Feel free to play with and extend the code - Even if you aren't familiar with F#, the overall structure of the code should make sense. If you're on OSX or Linux, the included Makefile should work assuming you have `mono` and `fsharp` installed. If you're on Windows, you're on your own, but try to open the included fsproj in Visual Studio.
