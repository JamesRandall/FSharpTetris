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
  
[<RequireQualifiedAccess>]
type Cell =
  | Block of BlockColor
  | Empty
  
[<RequireQualifiedAccess>]
type ControlState =
  | MoveLeft
  | MoveRight
  | MoveDown
  | RotateLeft
  | RotateRight
  | None
  
type Game =
  { Cells: Cell[][] // Array2D not supported in Fable
    NextBlock: Block
    BlockInPlay: BlockInPlay option
    Score: int
    Speed: float<ms>
    TimeUntilDrop: float<ms> // time until the game forces the block to drop a row
    IsInCollision: bool
  }