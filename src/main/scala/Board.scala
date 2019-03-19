import Player.Player
import util.Properties

//noinspection RangeToIndices
// Defines the board class and its constructor.
class Board(val rows: Int, val columns: Int) {

  // Initializes a two dimensional array that represents the board.
  // Each position either contains the checker of Player1, Player2 or nothing (NoPlayer).
  var board = Array.fill[Player](rows,columns) {Player.NoPlayer}

  // TODO: Returns the owner of the checker at position (row, column).
  // Tip: In the game rows and columns start at 1, but arrays start at 0.
  def getCheckerAtPosition(row: Int, column: Int): Player = {
    board(row - 1)(column - 1)
  }

  // TODO: Returns true of the row and column actually exist on the board.
  def isPositionInBoard(row: Int, column: Int): Boolean = {
    row <= board.length && column <= board(row - 1).length
  }

  // TODO: The player has placed a checker in a column. It has to sink until it reaches the bottom or another checker
  def sinkChecker(column: Int, player: Player): Unit = {
    val transposedBoard = board.transpose
    for (rowIndex <- 0 until transposedBoard.length) {
      if (transposedBoard(rowIndex)(column - 1) != Player.NoPlayer) {
        board(rowIndex - 1)(column - 1) = player
      } else if (rowIndex == transposedBoard.length - 1) {
        board(rowIndex)(column - 1) = player
      }
    }
  }

  // TODO: The toString() method returns a string that visually represents the board.
  // An example representation for a 4x4 board where the player1 has sinked a checker in column 2.
  //
  // 0000
  // 0000
  // 0000
  // 0100
  //
  // Feel free to choose a different visual representation. As long as its easy to understand.
  override def toString(): String = {
    var repres = ""

    for (rowIndex <- 0 until board.length) {
      for (columnIndex <- 0 until board(rowIndex).length) {
        if (board(rowIndex)(columnIndex) == Player.Player1) {
          repres += "X "
        } else if (board(rowIndex)(columnIndex) == Player.Player2) {
          repres += "O "
        } else if (board(rowIndex)(columnIndex) == Player.NoPlayer) {
          repres += "_ "
        } else {
          throw new IllegalArgumentException
        }
      }
      repres += Properties.lineSeparator
    }
    repres
  }
}
