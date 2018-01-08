module Animation

    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Graphics

    let FrameWidth = 128
    let FrameHeight = 128

    type Animation =
        {
            TextureStrip    : Texture2D
            FrameCount      : int
            CurrentFrame    : int
            CurrentTime     : int
            TimePerFrame    : int
        }

    let Create (texture: Texture2D) (frameLength: int) =
        let frameCount = texture.Width / FrameWidth
        {
            TextureStrip = texture
            FrameCount = frameCount
            CurrentFrame = 0
            CurrentTime = 0
            TimePerFrame = frameLength
        }

    let Update (gameTime: GameTime) (animation: Animation) =
        let time = animation.CurrentTime + (int gameTime.ElapsedGameTime.Milliseconds)
        let newFrame =
            if time > animation.TimePerFrame then
                (animation.CurrentFrame + 1) % animation.FrameCount
            else
                animation.CurrentFrame
        let counter = 
            if time >animation.TimePerFrame then
                0
            else
                time
        {
            animation with
                CurrentFrame = newFrame
                CurrentTime = counter
        }

    let Draw (spriteBatch: SpriteBatch) (animation: Animation) (position: Vector2) =
        let rect = System.Nullable(Rectangle(animation.CurrentFrame * FrameWidth, 0, FrameWidth, FrameHeight))
        spriteBatch.Draw(animation.TextureStrip, position, rect, Color.White)
