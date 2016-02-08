module ProcCastle.Renderer

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open ProcCastle.Base

let createVertexBuffer data =
    let buffer_id = GL.GenBuffer()
    let buffersize = nativeint (sizeof<Vertex> * Array.length data)
    GL.BindBuffer(BufferTarget.ArrayBuffer, buffer_id)
    GL.BufferData(BufferTarget.ArrayBuffer, buffersize, data, BufferUsageHint.StaticDraw)
    buffer_id

let createIndexBuffer indices =
    let buffer_id = GL.GenBuffer()
    let buffersize = nativeint (sizeof<int> * Array.length indices)
    GL.BindBuffer(BufferTarget.ElementArrayBuffer, buffer_id)
    GL.BufferData(BufferTarget.ElementArrayBuffer, buffersize, indices, BufferUsageHint.StaticDraw)
    buffer_id


type Vertex with
    static member BindPointers () =
        GL.EnableClientState(ArrayCap.VertexArray)
        GL.VertexPointer(3, VertexPointerType.Float, sizeof<Vertex>, 16)
        GL.EnableClientState(ArrayCap.NormalArray)
        GL.NormalPointer(NormalPointerType.Float, sizeof<Vertex>, 28)
        GL.EnableClientState(ArrayCap.ColorArray)
        GL.ColorPointer(4, ColorPointerType.Float, sizeof<Vertex>, 0)

let createVBO objects =
    let vertices = objects |> Array.ofList |> Array.collect Primitive.toVertices
    let vv = createVertexBuffer vertices
    let ii = createIndexBuffer [| 0 .. vertices.Length - 1 |]
    let center = vertices |> Seq.fold (fun c v -> (1.0f / float32 vertices.Length) * v.pos + c) Vector3.Zero
    vv, ii, vertices.Length, center

type Viewer (objects, w, h) =
    inherit OpenTK.GameWindow(w,h, new GraphicsMode(ColorFormat(32), 16, 0, 8), "C A S T L E",
                              GameWindowFlags.FixedWindow, DisplayDevice.Default,
                              2, 1, GraphicsContextFlags.Default)
    let vv, ii, vcount, center = createVBO objects
    let proj = Matrix4.CreatePerspectiveFieldOfView(0.5f, (float32 w / float32 h), 1.0f, 15.0f)
    let mv = Matrix4.LookAt(Vector3(0.f, 4.f, 8.5f), center, Vector3.UnitY)
    // let proj = Matrix4.CreateOrthographic(10.0f, 10.0f * (float32 h / float32 w), 0.1f, 10.0f)
    // let mv = Matrix4.LookAt(Vector3(0.f, 6.f, 0.5f), center, Vector3.UnitY - Vector3.UnitZ)
    member val GameTime = 0.0 with get, set
    member val Rendered = false
    member this.ScreenShot() =
        let bmp = new System.Drawing.Bitmap(this.ClientSize.Width, this.ClientSize.Height)
        let data = bmp.LockBits(this.ClientRectangle, System.Drawing.Imaging.ImageLockMode.WriteOnly, System.Drawing.Imaging.PixelFormat.Format24bppRgb)
        GL.ReadPixels(0, 0, this.ClientSize.Width, this.ClientSize.Height, PixelFormat.Bgr, PixelType.UnsignedByte, data.Scan0)
        bmp.UnlockBits(data)
        bmp.RotateFlip(System.Drawing.RotateFlipType.RotateNoneFlipY); bmp
    override this.OnKeyPress e = this.Close()
    override this.OnUpdateFrame e = this.GameTime <- this.GameTime + e.Time
    override this.OnLoad e =
        GL.Enable(EnableCap.DepthTest)
        GL.DepthFunc(DepthFunction.Lequal)
        GL.Enable(EnableCap.ColorMaterial);
        GL.ColorMaterial(MaterialFace.Front, ColorMaterialParameter.Diffuse);
        GL.Enable(EnableCap.Lighting)
        GL.Enable(EnableCap.Light0)
        GL.Light(LightName.Light0, LightParameter.Position, Vector4(200.0f, 100.0f, 100.0f, 0.0f))
        GL.Light(LightName.Light0, LightParameter.Diffuse, Vector4.One * 2.0f)
        GL.Light(LightName.Light0, LightParameter.ConstantAttenuation, 0.3f)
    override this.OnRenderFrame e =
        GL.MatrixMode(MatrixMode.Projection)
        GL.LoadMatrix(ref proj)
        GL.MatrixMode(MatrixMode.Modelview)
        GL.LoadMatrix(ref mv)
        GL.ClearColor(Color4.CornflowerBlue)
        GL.Clear(ClearBufferMask.DepthBufferBit ||| ClearBufferMask.ColorBufferBit)

        GL.BindBuffer(BufferTarget.ArrayBuffer, vv)
        GL.BindBuffer(BufferTarget.ElementArrayBuffer, ii)
        Vertex.BindPointers()
        GL.DrawElements(BeginMode.Triangles, vcount, DrawElementsType.UnsignedInt, 0);
        GL.DisableClientState(ArrayCap.ColorArray)
        GL.Color4(Color4.Black)
        GL.DrawElements(BeginMode.Lines, vcount, DrawElementsType.UnsignedInt, 0);
        this.SwapBuffers()
        if not (this.Rendered) then (this.ScreenShot()).Save(this.Title + ".png")

let _display (o:Primitives list) = (new Viewer(o, 500, 500)).Run()
let _tempdisplay o t =
    let v = new Viewer(o, 500, 500)
    v.UpdateFrame.Add (fun e -> if v.GameTime > t then v.Close())
    v.Run()
