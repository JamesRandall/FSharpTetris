module App.Model

[<Measure>] type ms

type BlockColor =
  { Fill: int
    Left: int
    Top: int
    Bottom: int
    Right: int
  }
  
type Constraints =
  { MinX: int
    MaxX: int
    MinY: int
    MaxY: int
  }

type Block =
  { Rotations: int[][][]
    CurrentRotation: int
    Color: BlockColor
  }
  member x.Current = x.Rotations.[x.CurrentRotation]
  
type BlockInPlay =
  { Block: Block
    X: int
    Y: int
  }
  member x.Current = x.Block.Current
  
[<RequireQualifiedAccess>]
type Cell =
  | Block of BlockColor
  | Wall
  | Empty
  
[<RequireQualifiedAccess>]
type ControlState =
  | MoveLeft
  | MoveRight
  | MoveDown
  | RotateLeft
  | RotateRight
  | None
  
[<RequireQualifiedAccess>]
type ErasingRowMode =
  | BlankingRows of rowIndexes:int list 
  | FallingRows of rowIndexes:int list
  
[<RequireQualifiedAccess>]
type GameMode =
  | Normal
  | ErasingRows of ErasingRowMode
  | GameOver
  
type Game =
  { Cells: Cell[][] // Array2D not supported in Fable
    NextBlock: Block
    BlockInPlay: BlockInPlay option
    Score: int
    Speed: float<ms>
    TimeUntilDrop: float<ms> // time until the game forces the block to drop a row
    IsInCollision: bool
    GameMode: GameMode
  }