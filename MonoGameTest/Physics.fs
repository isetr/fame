module Physics

    open Microsoft.Xna.Framework
    open WorldActor

    let IsStaticActor (actor: WorldActor) =
        match actor.BodyType with
        | Static -> true
        | _ -> false

    let PartitionWorldObjcets (worldObjects: WorldActor list) =
        worldObjects
        |> List.partition IsStaticActor

    let HandleCollisions (worldObjects: WorldActor list) =
        let stc, dyn = PartitionWorldObjcets worldObjects

        let FindNewVelocity rect1 rect2 (velocity: Vector2) =
            let inter = Rectangle.Intersect(rect1, rect2)
            if inter.Height > inter.Width then
                Vector2(0.f, velocity.Y)
            elif inter.Width > inter.Height then
                Vector2(velocity.X, 0.f)
            else
                velocity

        let FindOptimumCollision (a: WorldActor) (b: WorldActor) =
            match a.ActorType, b.ActorType with
            | Player _, Wall ->
                match a.BodyType, b.BodyType with
                | Dynamic s, Static ->
                    {
                        a with
                            BodyType = Dynamic (FindNewVelocity a.DesiredBounds b.CurrentBounds s)
                    }
                | _ -> a
            | _ -> a

        let rec FigureCollisions (actor: WorldActor) (sortedActors: WorldActor list) =
            match sortedActors with
            | [] -> actor
            | x :: xs ->
                let a = 
                    if actor.DesiredBounds.Intersects x.DesiredBounds then
                        FindOptimumCollision actor x
                    else
                        actor
                FigureCollisions a xs

        let rec FixCollisions (toFix: WorldActor list) (alreadyFixed: WorldActor list) =
            match toFix with
            | [] -> alreadyFixed
            | x :: xs ->
                let a = FigureCollisions x alreadyFixed
                FixCollisions xs (a::alreadyFixed)

        FixCollisions dyn stc

    let AddGravity (gameTime: GameTime) (actor: WorldActor) =
        let ms = gameTime.ElapsedGameTime.TotalMilliseconds
        let g = ms * 0.01
        match actor.BodyType with
        | Dynamic s -> { actor with BodyType = Dynamic <| Vector2 (s.X, s.Y + (float32 g)) }
        | _ -> actor

    let AddFriction (actor: WorldActor) =
        match actor.BodyType with
        | Dynamic v -> { actor with BodyType = Dynamic <| Vector2 (v.X * 0.95f, v.Y) }
        | _ -> actor

    let ResolveVelocities (actor: WorldActor) =
        match actor.BodyType with
        | Dynamic v -> { actor with Position = actor.Position + v }
        | _ -> actor
