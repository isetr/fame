module Animation

    open Microsoft.Xna.Framework
    open Microsoft.Xna.Framework.Graphics

    let FrameWidth = 128
    let FrameHeight = 128

    type AnimationInitKind =
        | Still of Texture2D
        | FullImage of Texture2D * FullImageData
        | ImagePart of Texture2D * ImagePartData
    and FullImageData =
        {
            FrameSize: Vector2
            FrameCount: int
            TimePerFrame: int
        }
    and ImagePartData =
        {
            StartFrame: int
            FrameSize: Vector2
            FrameCount: int
            TimePerFrame: int
        }
    type AnimationData =
        {
            Size: Vector2
            Color: Color
            TimePerFrame: int
        }
    type Animation =
        private {
            TextureStrip      : Texture2D
            FrameCount        : int
            CurrentFrameIndex : int
            Elapsed           : int
            StartFrame        : int
            FrameSize         : Vector2
            Data              : AnimationData
        }

    let rec Create = function
        | Still texture ->
            Create <| FullImage (
                texture,
                {
                    FrameSize = Vector2(float32 texture.Bounds.Width, float32 texture.Bounds.Height)
                    FrameCount = 1
                    TimePerFrame = 0
                }
            )
        | FullImage (texture,fullData) ->
            Create <| ImagePart (
                texture,
                {
                    FrameSize = fullData.FrameSize
                    FrameCount = fullData.FrameCount
                    TimePerFrame = fullData.TimePerFrame
                    StartFrame = 0
                }
            )
        | ImagePart (texture,data) ->
            {
                TextureStrip = texture
                FrameCount = data.FrameCount
                CurrentFrameIndex = 0
                Elapsed = 0
                StartFrame = data.StartFrame
                FrameSize = data.FrameSize
                Data =
                    {
                        Color = Color.White
                        Size = data.FrameSize
                        TimePerFrame = data.TimePerFrame
                    }
            }

    let Lens (lens : AnimationData -> AnimationData) (animation : Animation) =
        { animation with
            Data = animation.Data |> lens
        }

    let Update (gameTime: GameTime) (animation: Animation) =
        let elapsed = animation.Elapsed + int gameTime.ElapsedGameTime.Milliseconds
        let (elapsed,currentIndex) =
            if animation.Data.TimePerFrame > 0 && elapsed >= animation.Data.TimePerFrame then
                let frameJump = elapsed / animation.Elapsed
                let newFrameIndex = (animation.CurrentFrameIndex + frameJump) % animation.FrameCount
                elapsed - frameJump * animation.Data.TimePerFrame, newFrameIndex
            else
                elapsed, animation.CurrentFrameIndex
        { animation with
            CurrentFrameIndex = currentIndex
            Elapsed = elapsed
        }

    let Draw (spriteBatch: SpriteBatch) (animation: Animation) (position: Vector2) =
        let sourceX = animation.CurrentFrameIndex * int animation.FrameSize.X % animation.TextureStrip.Width
        let sourceY = animation.CurrentFrameIndex * int animation.FrameSize.X / animation.TextureStrip.Width
        let sourceRect = Rectangle(sourceX, sourceY, int animation.FrameSize.X, int animation.FrameSize.Y)
        let destRect = Rectangle(position.ToPoint(), animation.Data.Size.ToPoint())
        spriteBatch.Draw(
            animation.TextureStrip,
            destRect,
            sourceRect |> System.Nullable,
            animation.Data.Color
        )
