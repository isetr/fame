module BallGame

    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Graphics
    open Microsoft.Xna.Framework.Input

    type Game2 () as x =
        inherit Game()

        do x.Content.RootDirectory <- "Content"
        do x.Window.Position <- Point(60, 60)

        let graphics = new GraphicsDeviceManager(x)
        do graphics.PreferredBackBufferHeight <- 900
        do graphics.PreferredBackBufferWidth <- 1600

        let rng = new System.Random()


        let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>
        let mutable spriteFont = Unchecked.defaultof<SpriteFont>

        let mutable WorldObjects =
            lazy 
            (
                ("enemy", WorldActor.ActorType.Player (WorldActor.PlayerState.Nothing), Vector2(0.f, 0.f), Vector2(128.f, 128.f), false)
                |> List.replicate 200
                |> List.map (fun (tex, at, _, size, is) -> (tex, at, Vector2(float32 <| rng.Next(1592),float32 <| rng.Next(892)), size, is))
                |> List.map (fun (tex, at, pos, size, is) -> WorldActor.Create x.Content tex at pos size is)
            )

        let DrawActor (spriteBatch: SpriteBatch) (actor: WorldActor.WorldActor) =
            match actor.Animation with
            | Some anim -> Animation.Draw spriteBatch anim actor.Position
            | None -> ()

        override x.Initialize () =
            spriteBatch <- new SpriteBatch(x.GraphicsDevice)
            spriteFont <- x.Content.Load<SpriteFont> "Font"
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
            let fps = 
                match gameTime.ElapsedGameTime.Milliseconds with
                | 0 -> string 0
                | s -> sprintf "FPS: %A" ((1000.f / (float32 s)))
            spriteBatch.DrawString(spriteFont, fps, new Vector2(1.f, 1.f), Color.Black);
            WorldObjects.Value
            |> List.iter (DrawActor spriteBatch)
            spriteBatch.End ()
            ()
