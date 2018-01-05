module Game

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

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
                ("player", WorldActor.ActorType.Player (WorldActor.PlayerState.Nothing), Vector2(10.f,10.f), Vector2(32.f, 32.f), false)
                ("player", WorldActor.ActorType.Wall, Vector2(50.f, 10.f), Vector2(32.f, 32.f), true)
            ]
            |> List.map (fun (tex, at, pos, size, is) -> WorldActor.Create x.Content tex at pos size is)
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
