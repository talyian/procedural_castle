module ProcCastle.Main
open OpenTK

let preview f =
    [ for i in -2.f..2.f..2.f do
      for j in -2.f..2.f..2.f -> Matrix4.CreateTranslation(1.5f * Vector3(i, 0.f, j)) * Matrix4.CreateScale 0.5f ]
    |> List.mapi (fun i m -> Base.Offset(m, f i))
    |> Renderer._display

[<EntryPoint>]
let main args =
    for i in 3..20 do preview (fun x -> Towers.create i)
    0
    // preview (fun () -> Trees.create 5); 0
    // Renderer._display (Gen.castle 4); 0
