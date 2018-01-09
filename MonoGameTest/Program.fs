open Game

[<EntryPoint>]
let main argv = 
    use g = new FSharpGame.FSharpGame<_,_>(game1Behaviour)
    g.Run()
    0
