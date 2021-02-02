

// These are objects you can use to refer to different chess piece types

package game

class PieceType(val letter: String)
case object King    extends PieceType("K")
case object Queen   extends PieceType("Q")
case object Rook    extends PieceType("R")
case object Bishop  extends PieceType("B")
case object Knight  extends PieceType("N")
case object Pawn    extends PieceType("")


/**
 * This class models one single chess piece.
 */
sealed abstract class Piece(owner: Player, ptype: PieceType) {
  def getOwner = owner
  def getType  = ptype
}

    /**
     * All possible chess piece types are case classes derived from Piece.
     * Making the class sealed makes sure that no new piece types can be introduced in other files.
     * 
     * The letters corresponding to the pieces are :
     * 
     * King     : K
     * Queen    : Q
     * Rook     : R
     * Bishop   : B
     * Knight   : N
     * Pawn     : (nothing)
     * 
     * object created with case classes can be compared using the equals operator and they can also
     * be effectively used in match.
     * 
     * Note that the letter in Piece is a val defined in parameters, it can be easily accessed.
     */

//These case classes refer to different chess pieces (with the types defined before)

case class King(owner: Player)   extends Piece(owner, King)
case class Queen(owner: Player)  extends Piece(owner, Queen)
case class Rook(owner: Player)   extends Piece(owner, Rook)
case class Bishop(owner: Player) extends Piece(owner, Bishop)
case class Knight(owner: Player) extends Piece(owner, Knight)
case class Pawn(owner: Player)   extends Piece(owner, Pawn)
