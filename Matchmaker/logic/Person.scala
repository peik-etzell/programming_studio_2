package logic

/**
 * A class representing a Person used in the Matcmaker.
 * Super class for the different types of Person.
 */
abstract class Person(val name: String, val favoriteColor: Color, val hobby: String, val occupation: String) {
  /**
   * Determines the match level for this person and the other.
   */
  def calculateMatch(other: Person): Int

  /**
   * Method that calculates a match score for two people. The result should be
   * the average of matching each person with the other both ways.
   */
  def bothMatch(other: Person): Int = (this.calculateMatch(other) + other.calculateMatch(this)) / 2

  override def toString = s"${this.name}: (${this.favoriteColor}, ${this.hobby}, ${this.occupation  })"
}

/**
 * Perectionist person.
 * The perfectionist scores their matches in the following manner:
 *
 * 		- 100 p - other is also a Perfectionist and all the interests are the same (except name)
 *
 * 		- 90 p - other person has all the same interests
 *
 * 		- 0 p - At least one interest is different.
 */
case class Perfectionist(n: String, fColor: Color, hobb: String, occ: String) extends Person (n, fColor, hobb, occ) {

  def calculateMatch(other: Person) = {
    other match {
      case Perfectionist(_, this.fColor, this.hobb, this.occ) => 100
      case Hobbyist(_, this.favoriteColor, this.hobby, this.occupation) => 90
      case _ => 0
    }
  }

}

/**
 * Person who likes hobbies.
 * The hobbyist scores their matches in the following manner:
 *
 * 		- 100 p - other person is also a Hobbyist and has the same hobby
 *
 * 		- 90 p - 	other person is not a hobbyist, but has the same hobby
 * 							 and at least one more mutual interest (favorite color or occupation)
 *
 * 		- 80 p -	other person is not a hobbyist, but has the same hobby,
 * 							but no other matching interests
 *
 * 		- 60 p - 	other person is also a Hobyist, but has a different hobby
 *
 * 		- 0 p - 	None of the above
 */
case class Hobbyist(n: String, fColor: Color, hobb: String, occ: String) extends Person (n, fColor, hobb, occ) {

  def calculateMatch(other: Person) = {
    other match {
      case Hobbyist(_, _, this.hobby, _) => 100
      case person: Hobbyist => 60
      case Perfectionist(_, color, this.hobby, occupation) => if (color == this.fColor || occupation == this.occupation) 90 else 80
      case ColorNeutral(_, this.hobby, occupation) => if (this.occupation == occupation) 90 else 80
      case _ => 0
    }

  }
}


/**
 * Person who does not care about colors.
 * The ColorNeutral person scores their matches in the following manner:
 *
 * 		- 100 p - other person is also ColorNeutral
 *
 * 		- 90 p - other person has the same hobby or occupation
 *
 * 		- 0 p - None of the above
 *
 */
case class ColorNeutral(n: String, hobb: String, occ: String) extends Person (n, NoColor, hobb, occ) {

  def calculateMatch(other: Person) = {
    other match {
      case person: ColorNeutral => 100
      case _ => if (other.occupation == this.occupation || other.hobby == this.hobby) 90 else 0
    }
  }
}


/*
 * A trait that defines different colors a Person might like.
 */
trait Color

case object Red extends Color
case object Green extends Color
case object Blue extends Color
case object Purple extends Color
case object Yellow extends Color
case object Orange extends Color
case object Brown extends Color
case object Gray extends Color
case object Black extends Color
case object White extends Color
case object NoColor extends Color
