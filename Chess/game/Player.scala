/**
 * A list of chess colors available. 
 */

package game

sealed trait Color
case object White extends Color
case object Black extends Color


/**
 * This class models a chess player.
 *
 * A new player with the given name and color can be easily created:
 * 
 * val player = new Player("Matti", Black);
 */

case class Player(name: String, color: Color) 