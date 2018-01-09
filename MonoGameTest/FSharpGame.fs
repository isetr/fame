module FSharpGame

open Microsoft.Xna.Framework

type FSharpGameBehaviour<'InitState,'GameState> =
    {
        Init          : Game -> unit
        Initialize    : Game * GraphicsDeviceManager -> 'InitState
        LoadContent   : Game * 'InitState -> 'GameState
        Update        : Game * GameTime * 'GameState -> 'GameState
        Draw          : Game * GameTime * 'GameState -> unit
        UnloadContent : Game * 'GameState -> unit
    }

type FSharpGame<'InitState,'GameState>(behaviour: FSharpGameBehaviour<'InitState,'GameState>) as this =
    inherit Game()

    do behaviour.Init (this :> Game)

    let graphics = new GraphicsDeviceManager(this)

    let mutable initState = Unchecked.defaultof<'InitState>

    let mutable gameState = lazy Unchecked.defaultof<'GameState>

    override this.Initialize () =
        initState <- behaviour.Initialize (this :> Game, graphics)
        base.Initialize ()

    override this.LoadContent () =
        gameState <- lazy (behaviour.LoadContent (this :> Game, initState))
        gameState.Force() |> ignore
        ()

    override this.Update gameTime =
        let value = gameState.Value
        gameState <- lazy (behaviour.Update (this :> Game, gameTime, value))
        gameState.Force() |> ignore
        ()

    override this.Draw gameTime =
        behaviour.Draw(this :> Game, gameTime, gameState.Value)

    override this.UnloadContent () =
        behaviour.UnloadContent (this :> Game, gameState.Value)
        base.UnloadContent ()


