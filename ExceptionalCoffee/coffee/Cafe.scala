package coffee

import scala.util.{Try, Success, Failure}
import scala.collection.mutable.Buffer

class Cafe (val coffeemaker: Coffeemaker) {

  /*
   * Implement methods makeOrderTryCatch and makeOrderTry.
   *
   * How methods should work:
   * Try to make asked amount of coffee using coffeemaker. Coffeemaker can throw exceptions if it runs out
   * coffee beans or becomes dirty. If making coffee throws NoBeansException or MachineDirtyException, fix problem using methods in Cafe class.
   * You don't need to handle other exceptions.
   * If an exception is thrown, method returns one less coffee than asked amount, so you don't have to make
   * a new coffee if making coffee fails.
   *
   * Implement makeOrderTryCatch using try-catch structure and makeOrderTry using Try, Success and Failure classes.
   * Both methods should work similarly.
   *
   * Don't change order of the methods. Write all code inside methods.
   */

  /**
   * Implement method using try-catch structure to catch exceptions
   */
  def makeOrderTryCatch(amount: Int): Buffer[Coffee] = {
    //add your code here
    val result = Buffer[Coffee]()

    while (result.size < amount) {
      try {
        result += coffeemaker.makeCoffee()
      } catch {
        case NoBeansException(text) => this.addBeans()
        case MachineDirtyException(text) => this.cleanMachine()
      }
    }
    result
  }


  /**
   * Implement method using Try, Success and Failure classes to handle exceptions
   */
  def makeOrderTry(amount: Int): Buffer[Coffee] = {
    //add your code here
    val result = Buffer[Coffee]()

    while (result.size < amount) {
      var coffee = Try {
        this.coffeemaker.makeCoffee()
      }
      coffee match {
        case Success(coffee) => result += coffee
        case Failure(NoBeansException) => addBeans()
        case Failure(e: MachineDirtyException) => cleanMachine()
        case Failure(e) => throw e
      }
    }
    result
  }

  //example method, modify later
  def addMilk(coffees: Buffer[Coffee]) = {
    val result = Buffer[Coffee]()
    for {
      coffee <- coffees
      coffeeWithMilk <- Try(coffeemaker.addMilk(coffee))
    } {
      result += coffeeWithMilk
    }
    result
  }

  /**
   * If coffeemaker runs out of coffee beans, add them using this method
   */
  def addBeans(): Unit = {
    coffeemaker.coffeeBeanAmount = coffeemaker.maxBeanAmount
  }

  /**
   * Clean machine when it becomes dirty.
   */
  def cleanMachine(): Unit = {
    coffeemaker.cleanliness = coffeemaker.maxCleanliness
  }
}
