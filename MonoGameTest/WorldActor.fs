module WorldActor

    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Graphics
    open Microsoft.Xna.Framework.Content

    type BodyType =
        | Static
        | Dynamic of Vector2

    type PlayerState =
        | Nothing

    type ActorType =
        | Player of PlayerState
        | Wall

    type WorldActor =
        {
            Position    : Vector2
            Size        : Vector2
            BodyType    : BodyType
            ActorType   : ActorType
            Animation   : Animation.Animation option
        }
        
        member this.CurrentBounds = Rectangle((int this.Position.X),(int this.Position.Y),(int this.Size.X),(int this.Size.Y))

        member this.DesiredBounds = 
            let desiredPos =
                match this.BodyType with
                | Dynamic v -> this.Position + v
                | Static    -> this.Position
            Rectangle((int desiredPos.X),(int desiredPos.Y),(int this.Size.X),(int this.Size.Y))

    let Create (content: ContentManager) textureName actorType position size isStatic =
        let tex =
            if not <| System.String.IsNullOrEmpty textureName then
                let tex = content.Load<Texture2D> textureName
                let anim = Animation.Create tex 100
                Some anim
            else
                None
        let bt =
            if isStatic then
                Static
            else
                Dynamic <| Vector2 (0.f, 0.f)
        {
            Position = position
            Size = size
            BodyType = bt
            ActorType = actorType
            Animation = tex
        }

    let Update (gameTime: GameTime) (actor: WorldActor) =
        let animation = 
            match actor.Animation with
            | Some anim -> Some <| Animation.Update gameTime anim
            | None -> None
        {
            actor with
                Animation = animation
        }
