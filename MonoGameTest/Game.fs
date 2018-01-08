module Game

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Animation
open WorldActor

type GameState =
    {
        Actors: WorldActor list
        Playing: bool
        WonAnimation: Animation
    }

type Game1 () as x =
    inherit Game()

    do x.Content.RootDirectory <- "Content"
    do x.Window.Position <- Point(60, 60)

    let graphics = new GraphicsDeviceManager(x)

    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    let mutable WorldObjects =
        lazy (
            {
                Actors =
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
                Playing = true
                WonAnimation =
                    x.Content.Load<Texture2D> "AkiCalm"
                    |> Still
                    |> Animation.Create
                    |> Animation.Lens (fun data -> {data with Size = Vector2(64.f,64.f)})
            }
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
        let current = WorldObjects.Value.Actors
        let playing = WorldObjects.Value.Playing
        let newPlaying =
            if playing then
                let player =
                    current |> List.find (fun actor ->
                        match actor.ActorType with
                        | Player _ -> true
                        | _ -> false
                    )
                let enemy =
                    current
                    |> List.find (fun actor ->
                        match actor.ActorType with
                        | Player _ -> false
                        | _ -> true
                    )
                MathHelper.Distance(enemy.Position.X + enemy.Size.X, player.Position.X) > 0.15f ||
                MathHelper.Distance(enemy.Position.Y, player.Position.Y) > 10.f
            else
                false
        let value = WorldObjects.Value
        WorldObjects <-
            lazy 
            ({ value with
                Actors =
                    current
                    |> List.map (InputHandler.HandleInput (Keyboard.GetState()))
                    |> List.map (Physics.AddFriction)
                    |> Physics.HandleCollisions
                    |> List.map (Physics.ResolveVelocities)
                    |> List.map (WorldActor.Update gameTime)
                    |> fun x ->
                        if playing && not newPlaying then
                            x |> List.map (fun actor ->
                                match actor.ActorType with
                                | Player _ -> actor
                                | _ ->
                                    { actor with
                                        Animation =
                                            actor.Animation
                                            |> Option.map (fun anim ->
                                                if anim.CurrentFrame = 0 then
                                                    value.WonAnimation
                                                else
                                                    Animation.Lens (fun data ->
                                                        { data with TimePerFrame = System.Int32.MaxValue }
                                                    ) anim
                                            )
                                    }
                            )
                        else x
                Playing = newPlaying
            })
        WorldObjects.Force () |> ignore
        ()

    override x.Draw gameTime =
        x.GraphicsDevice.Clear <| Color.CornflowerBlue
        spriteBatch.Begin ()
        WorldObjects.Value
        |> fun gameState -> gameState.Actors |> List.iter (DrawActor spriteBatch)
        spriteBatch.End ()
        ()
