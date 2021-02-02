/**
 * This class serves as a facade for the chess game.
 *
 * @param board The chess board used in this game
 */

package game

class Game(val board: Board) { // Notice that board is defined as a val (you can access it)

    /**
     * The black player.
     */
    private var black: Option[Player] = None

    def getBlack = black
    
    /**
     * The white player.
     */
    private var white: Option[Player] = None

    def getWhite = white


    /**
     * Adds a player to a game.
     * @param player the player to be added
     */
    def addPlayer(player: Player):Unit = {
        if (player.color == Black) {
            black = Some(player)
        } else {
            white = Some(player)
        }
    }
}
