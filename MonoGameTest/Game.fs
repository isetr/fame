module Game

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Animation

type Game1 () as x =
    inherit Game()

    do x.Content.RootDirectory <- "Content"
    do x.Window.Position <- Point(60, 60)

    let graphics = new GraphicsDeviceManager(x)

    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    let mutable WorldObjects =
        lazy 
        (
            [
                ( Still <| x.Content.Load<Texture2D> "player"
                , WorldActor.ActorType.Player (WorldActor.PlayerState.Nothing)
                , Vector2(10.f,10.f)
                , Vector2(64.f, 64.f)
                , false )
                ( FullImage ( x.Content.Load<Texture2D>("enemy")
                            , { FrameCount = 2; FrameSize = Vector2(128.f, 128.f); TimePerFrame = 1000 })
                , WorldActor.ActorType.Wall
                , Vector2(256.f, 10.f)
                , Vector2(64.f, 64.f)
                , true )
            ]
            |> List.map (fun (tex, at, pos, size, is) ->
                let animation =
                    tex |> Animation.Create
                WorldActor.Create x.Content (animation |> Animation.Lens (fun data -> {data with Size = Vector2(64.f,64.f)}) |> Some) at pos size is
            )
        )

    let DrawActor (spriteBatch: SpriteBatch) (actor: WorldActor.WorldActor) =
        match actor.Animation with
        | Some anim -> Animation.Draw spriteBatch anim actor.Position
        | None -> ()

    override x.Initialize () =
        spriteBatch <- new SpriteBatch(x.GraphicsDevice)
        base.Initialize()
        ()

    override x.LoadContent () =
        WorldObjects.Force () |> ignore
        ()

    override x.Update gameTime =
        let current = WorldObjects.Value
        WorldObjects <-
            lazy 
            (
                current
                |> List.map (InputHandler.HandleInput (Keyboard.GetState()))
                |> List.map (Physics.AddFriction)
                |> Physics.HandleCollisions
                |> List.map (Physics.ResolveVelocities)
                |> List.map (WorldActor.Update gameTime)
            )
        WorldObjects.Force () |> ignore
        ()

    override x.Draw gameTime =
        x.GraphicsDevice.Clear <| Color.CornflowerBlue
        spriteBatch.Begin ()
        WorldObjects.Value
        |> List.iter (DrawActor spriteBatch)
        spriteBatch.End ()
        ()
