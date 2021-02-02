/**
 * This class models a chess board.
 */

package game

class Board {
    
    /**
     * An internal array for holding the chess pieces.
     * Initially filled with Nones
     */
    val board = Array.fill[Option[Piece]](8, 8)(None)


    /**
     * Places a new piece in the given location.
     * 
     * @param piece the piece being placed.
     * @param column the column in which to place the piece
     * @param row the row in which to place the piece
     * @throws IllegalArgumentException
     */
    
    def setPiece(piece: Piece, column: Int, row: Int) = {
        if (isFree(column, row)) {
            board(column)(row) = Some(piece)
        } else {            
            throw new IllegalArgumentException(
                    "Tried to place a piece in an occupied square.")
        }
    }

    /**
     * Returns the piece located in the given coordinates.
     * 
     * @param column the column of interest
     * @param row the row of interest
     * @return the piece located in the coordinates wrapped in an Option or None if the location was empty
     */    
    def getPiece(column: Int, row: Int) = board(column)(row)
    

    /**
     * Tests if the given location was free.
     * 
     * @param column the column of interest
     * @param row the row of interest
     * @return true if and only if the location was empty
     */
    def isFree(column: Int, row: Int) = board(column)(row).isEmpty

}

object Board {
        /**
     * Converts the letters a,b,c,d,e,f,g,h to positions 0-7.
     * @param row the character representation of a column.
     * @return the integer representation.
     */
    
    def columnCharToInteger(column: Char) = column - 'a'

    /**
     * Converts the characters '1','2','3'..... to positions 0-7.
     * @param row the character representation of a row.
     * @return the integer representation.
     */

    def rowCharToInteger(row: Char) = row - '1'
    
}
