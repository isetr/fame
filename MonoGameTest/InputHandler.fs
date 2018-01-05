module InputHandler

    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Input

    let HandleInput (kbState: KeyboardState) (actor: WorldActor.WorldActor) =
        let rec HandleKeys keys (currentVelocity: Vector2) =
            match keys with
            | [] -> currentVelocity
            | x :: xs ->
                match x with
                | Keys.Left ->
                    let newSpeed =
                        if (currentVelocity.X - 0.1f) < -1.f then
                            -1.f
                        else
                            currentVelocity.X - 0.1f
                    let newV = Vector2 (newSpeed, currentVelocity.Y)
                    HandleKeys xs newV
                | Keys.Right ->
                    let newSpeed =
                        if (currentVelocity.X + 0.1f) > 1.f then
                            1.f
                        else
                            currentVelocity.X + 0.1f
                    let newV = Vector2 (newSpeed, currentVelocity.Y)
                    HandleKeys xs newV
                | _ -> HandleKeys xs currentVelocity
        match actor.ActorType with
        | WorldActor.ActorType.Player _ ->
            let initVelocity =
                match actor.BodyType with
                | WorldActor.BodyType.Dynamic v -> v
                | _ -> Vector2 ()
            let velocity = HandleKeys (kbState.GetPressedKeys() |> Array.toList) initVelocity
            {
                actor with
                    BodyType = WorldActor.BodyType.Dynamic velocity
            }
        | _ -> actor


