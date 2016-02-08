#I "packages/OpenTK/lib/NET40"
#r "OpenTK.dll"
#r "base.dll"

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL

open System
open ProceduralGen

let rand = new Random(0)
let island () = [
    // sea
    let swidth = 10
    let square = [
        Color4.Aqua, V3(swidth, 0, swidth)
        Color4.Aqua, V3(swidth, 0, -swidth)
        Color4.Aqua, V3(-swidth, 0, swidth)
        Color4.Aquamarine, V3(swidth, 0, -swidth)
        Color4.Aquamarine, V3(-swidth, 0, swidth)
        Color4.Aquamarine, V3(-swidth, 0, -swidth)
        ]
    yield Polygon(square)

    // island -- diamond square algo
    let h, w = 128, 128
    let data = Array2D.create w h 0.1f
    let diamond d =
        for x in 0..d..(w - d - 1) do
          for y in 0..d..(h - d - 1) do
            data.[x + d / 2, y + d / 2] <- (
                data.[x, y] +
                data.[x + d, y + d] +
                data.[x + d, y] +
                data.[x, y + d]) / 4.0f + rand.NextF() * (float32 d) / (float32 h) * 5.0f
        for x in 0..d..(w-d-1) do
          for y in 0..d..(h-d-1) do
            data.[x + d / 2, y] <- (
                data.[x, y]
                + data.[x + d/2, y + d/2]
                + data.[x + d, y]
                + (if y > 0 then data.[x+d/2,y-d/2] else 0.0f)
                ) / 4.0f
            data.[x, y + d/2] <- (
                data.[x, y]
                + data.[x + d / 2, y + d/2]
                + data.[x + d, y]
                + (if x > 0 then data.[x - d/2, y+d/2] else 0.0f)
                ) / 4.0f

    for d in (Array.unfold (function | 1 -> None | n -> Some(n, n / 2)) h) do diamond d

    for i in 0 .. Array2D.length1 data - 2 do
        for j in 0.. Array2D.length2 data - 2 do
            let x = i - Array2D.length1 data / 2
            let z = j - Array2D.length2 data / 2
            let sq = [
                Color4.Green, V3(x, data.[i, j], z)
                Color4.Green, V3(x+1, data.[i+1, j], z)
                Color4.Green, V3(x, data.[i, j+1], z+1)
                Color4.DarkGreen, V3(x+1, data.[i+1, j], z)
                Color4.DarkGreen, V3(x, data.[i, j+1], z+1)
                Color4.DarkGreen, V3(x+1, data.[i+1, j+1], z+1)
            ]
            yield Polygon(sq)
]

display (island())
