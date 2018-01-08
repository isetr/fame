module InputHandler

    open System
    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Input

    let HandleInput (kbState: KeyboardState) (actor: WorldActor.WorldActor) =
        let keyForces = function
            | Keys.Left  -> - Vector2.UnitX
            | Keys.Right ->   Vector2.UnitX
            | Keys.Up    -> - Vector2.UnitY
            | Keys.Down  ->   Vector2.UnitY
            | _          ->   Vector2.Zero
        match actor.ActorType with
        | WorldActor.ActorType.Player _ ->
            let initVelocity =
                match actor.BodyType with
                | WorldActor.BodyType.Dynamic v -> v
                | _ -> Vector2 ()
            let keyForce =
                kbState.GetPressedKeys()
                |> Array.fold (fun x y -> x + keyForces y) Vector2.Zero
            let velocity =
                initVelocity + (keyForce * 0.1f)
                |> (fun v ->
                    let trunc (x:float32) =
                        if Math.Abs x > 1.0f then
                            1.0f * (float32 <| Math.Sign x)
                        else x
                    Vector2(trunc v.X, trunc v.Y)
                )
            { actor with
                BodyType = WorldActor.BodyType.Dynamic velocity
            }
        | _ -> actor


