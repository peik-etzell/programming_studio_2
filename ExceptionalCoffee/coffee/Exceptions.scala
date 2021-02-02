package coffee

class CoffeemakerExceptions(text: String) extends Exception(text)

/**
 * Thrown if a coffeemaker runs out of beans.
 */
case class NoBeansException(text: String) extends CoffeemakerExceptions(text)

/**
 * Thrown if machine becomes dirty.
 */
case class MachineDirtyException(text: String) extends CoffeemakerExceptions(text)

/**
 * Thrown if the coffee is spilled after adding the milk.
 * You don't need to catch this exception.
 */
case class MilkSpilledException(text: String) extends CoffeemakerExceptions(text)

/**
 * Exception for testing purposes only.
 * You don't need to catch this exception.
 */
case class MachineBrokenException(text: String) extends CoffeemakerExceptions(text)
