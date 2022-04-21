module App.Game

open App.Model
open Browser.Types
open Fable.Core.JsInterop
open App.Render

// we want a 10x18 hole but we oversize it so we can have the left, right and bottom for the pit walls
let private pitWidth = 12 // left and right are for walls
let private pitHeight = 19 // bottom row is for wall
let private pitColor = { Fill = 0x1F2937 ; Top = 0x4B5563 ; Left = 0x4B5563 ;Right = 0x111827 ; Bottom = 0x111827 }

let private isInCollision blockArray x y (pit:Cell[][]) =
  blockArray
  |> Array.mapi(fun rowIndex row ->
    let pitRow = if (rowIndex+y) >= pit.Length || (rowIndex+y) < 0 then [||] else pit.[rowIndex+y]
    row
    |> Array.mapi(fun colIndex cell ->
      // if either cell is empty then their is no collision
      if (colIndex+x) >= pitRow.Length || (colIndex+x) < 0 then true else pitRow.[colIndex+x] = Cell.Empty || cell = 0 
    )
    |> Array.skipWhile id
  )
  |> Array.concat
  |> Array.tryHead
  |> Option.isSome

let private processTurn frameTime game =
  let (|NoTimeRemaining|TimeRemaining|) timeRemaining = if timeRemaining <= 0.<ms> then NoTimeRemaining else TimeRemaining
  let createNewBlockInPlay () =
    let block = Blocks.getRandomBlock ()
    let constraints = Blocks.getConstraints block
    { Block = block ; X = 5 ; Y = -constraints.MinY }
  let putNextBlockInPlay () =
    let constraints = Blocks.getConstraints game.NextBlock
    { Block = game.NextBlock ; X = 5 ;  Y = -constraints.MinY }
  
  let newTimeRemaining = game.TimeUntilDrop - frameTime
  let newGameState =
    match newTimeRemaining,game.BlockInPlay with
    | TimeRemaining,_ -> game
    | NoTimeRemaining,Some blockInPlay ->
      let wouldCollide = isInCollision blockInPlay.Block.Current blockInPlay.X (blockInPlay.Y+1) game.Cells
      if wouldCollide then
        { game with
            BlockInPlay = Some (putNextBlockInPlay ())
            NextBlock = Blocks.getRandomBlock ()
        }
      else
        { game with BlockInPlay = Some { blockInPlay with Y = blockInPlay.Y + 1 } }
    | NoTimeRemaining,None ->
      { game with BlockInPlay = createNewBlockInPlay () |> Some ; NextBlock = Blocks.getRandomBlock () }

  { newGameState with
      IsInCollision =
        newGameState.BlockInPlay
        |> Option.map (fun blockInPlay -> isInCollision blockInPlay.Block.Current blockInPlay.X blockInPlay.Y game.Cells)
        |> Option.defaultValue false
      TimeUntilDrop = if newTimeRemaining < 0.<ms> then game.Speed else newTimeRemaining
  }

let init (canvas:HTMLCanvasElement) =
  let context = canvas.getContext_2d()
  context?imageSmoothingEnabled <- false
  context.lineCap <- "square"
  context.translate(0.5,0.5)
  
  let initialGameState =
    { Cells =
        {0..(pitHeight-1)}
        |> Seq.map(fun row ->
          {0..(pitWidth-1)}
          |> Seq.map(fun col ->
            if col = 0 || col = pitWidth-1 || row = pitHeight-1 then Cell.Block pitColor else Cell.Empty
          )
          |> Seq.toArray
        )
        |> Seq.toArray
      NextBlock = Blocks.getRandomBlock ()
      BlockInPlay = None
      Score = 0
      Speed = 1000.<ms>
      TimeUntilDrop = 1000.<ms>
      IsInCollision = false
    }
  
  let gameLoop game (timestamp:float<ms>) =
    // doing the sizing in the game loop allows us to respond to canvas resizes
    let width = canvas.width
    let height = canvas.height
    let blockSize = (height - (pitHeight |> float)) / (pitHeight |> float) |> floor
    let spacingSize = blockSize + 1.
    let pitCanvasLeft = width/2. - (blockSize*(pitWidth |> float)/2.)
    let pitCanvasTop = 0.
    
    // game rendering
    let drawSquare (color:BlockColor) (column:int) (row:int) =
      let x = pitCanvasLeft + (spacingSize * (column |> float))
      let y = pitCanvasTop + (spacingSize * (row |> float))
      drawSquareAtCanvasCoords context color x y blockSize
    let drawBlock (blockInPlay:BlockInPlay) =
      blockInPlay.Block.Current
      |> Array.iteri(fun rowIndex row ->
        row
        |> Array.iteri(fun colIndex cell ->
          if cell > 0 then drawSquare blockInPlay.Block.Color (blockInPlay.X+colIndex) (blockInPlay.Y+rowIndex)
        )
      )
    let drawPit cells blockInPlay =
      cells
      |> Array.iteri(fun row rowContent ->
        rowContent
        |> Array.iteri(fun col cell ->
          match cell with
          | Cell.Empty -> ()
          | Cell.Block blockColor -> drawSquare blockColor col row
        )  
      )
      match blockInPlay with | Some blockInPlay -> drawBlock blockInPlay | None -> ()
      
    let newGameState = processTurn timestamp game
    
    clearCanvas context  
    drawPit newGameState.Cells newGameState.BlockInPlay
    
    newGameState
    
  let controlStateHandler game controlState =
    let rotateLeft block =
      { block with
          CurrentRotation =
            if block.CurrentRotation - 1 < 0 then block.Rotations.Length-1 else block.CurrentRotation-1
      }
    let rotateRight block =
      { block with
          CurrentRotation =
            if block.CurrentRotation + 1 >= block.Rotations.Length then 0 else block.CurrentRotation+1
      }
    let playerMovedBlockOption =
      game.BlockInPlay
      |> Option.map (fun blockInPlay ->
        let candidateBlockInPlay  =
          match controlState with
          | ControlState.MoveLeft -> { blockInPlay with X = blockInPlay.X - 1 }
          | ControlState.MoveRight -> { blockInPlay with X = blockInPlay.X + 1 }
          | ControlState.MoveDown -> { blockInPlay with Y = blockInPlay.Y + 1 }
          | ControlState.RotateLeft -> { blockInPlay with Block = blockInPlay.Block |> rotateLeft }
          | ControlState.RotateRight -> { blockInPlay with Block = blockInPlay.Block |> rotateRight }
          | _ -> blockInPlay
        if isInCollision candidateBlockInPlay.Block.Current candidateBlockInPlay.X candidateBlockInPlay.Y game.Cells then
          blockInPlay
        else
          candidateBlockInPlay
      )
    { game with BlockInPlay = playerMovedBlockOption }
    
  gameLoop,controlStateHandler,initialGameState
  