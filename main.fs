module ProcCastle.Main
open OpenTK

let preview f =
    [ for i in -2.f..2.f..2.f do
        for j in -2.f..2.f..2.f do
            let m = Matrix4.CreateTranslation(Vector3(i, 0.f, j)) * Matrix4.CreateScale 0.5f
            yield Base.Offset(m, (f()))] |> Renderer._display

[<EntryPoint>]
let main args =
    preview (fun () -> Trees.create 4); 0
    preview (fun () -> Towers.create 5); 0
