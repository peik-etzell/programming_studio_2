package genericInterval
import scala.math._

/*
 * The goal of this exercise is to practice generalization of an existing class.
 * This is a relatively normal refactoring procedure, when it is noticed that old code  
 * can be used in a wider context than was initially planned.
 * 
 * In the original exercise on round 3, the goal was to implement the class Interval which handled
 * a time interval between  two Moment objects. The exercise involved implementations for a number of methods including
 * union, intersection etc. which are common for intervals of any type.
 * 
 * In this exercise, you will pull your old Interval class from naphtalene and create a
 * parameterized version GenericInterval[T] of it. The greatest part of the functionality offered by the Moment class
 * was related to comparing Moment objects, which was crucial for the proper functionality of the Interval class.
 * We will generalize this, too, by offering an Ordering[T] object with the Intervall class can perform exactly the same comparisons.
 *
 * One feature that Ordering[T] cannot offer, and which the original Interval class used,
 * is the distance between two "moments". This is conciously omitted in the GenericInterval[T] class.
 * The length was used only in the toString method anyway so does not affect any other methods.
 */

/**
 * Each instance of the class `GenericInterval` represents an interval -- an inclusive range --
 * on a scale. An interval has a "start" and an "end", represented
 * as objects of type T. An interval always contains at least a single item.
 *
 * An interval object may be used represent a range of just about anything that has an Ordering.
 *
 * An `Interval` object is immutable after it has been created. That is, its state
 * can not be changed in any way.
 *
 * @param start the start of the interval (that is, the first item included in the interval)
 * @param end the end of the interval (that is, the last item included in the interval); equal to or higher than `start`
 */
class GenericInterval[T](val start: T, val end: T)(implicit order: Ordering[T]) {

	/**
	 * Returns a textual description of the interval.
	 * For instance, the interval from 1900 to 2013 is represented by the string `"1900...2013"`.
	 */

	override def toString = this.start + "..." + this.end

	/**
	 * Determines whether this interval is after the given item.
	 * This is only deemed to be the case if the entire interval comes after the
	 * given item.
	 *
	 * @param item the item being compared
	 * @return `true` if this entire interval is after the item, `false` in all other cases
	 */
	def isLaterThan(item: T) = {
		order.gt(this.start, item) && order.gt(this.end, item)
	}

	/**
	 * Determines whether this interval is after than the given interval.
	 * This is only deemed to be the case if this entire interval comes after the
	 * given interval. That is, no overlap is allowed.
	 *
	 * @param another an interval
	 * @return `true` if this entire interval is after than the other, `false` in all other cases
	 */
	def isLaterThan(another: GenericInterval[T]): Boolean = {
		this.isLaterThan(another.start) && this.isLaterThan(another.end)
	}

	/**
	 * Determines whether this interval contains the given item.
	 * (An interval also includes its start and end items.)
	 *
	 * @param item a single item
	 * @return a boolean value indicating if the item is inside this interval
	 */
	def contains(item: T): Boolean = {
		order.lteq(start, item) && order.gteq(end, item)
	}

	/**
	 * Determines whether this interval contains the given interval.
	 * This is the case if and only if all items within the other interval are
	 * contained within this interval.
	 *
	 * @param another an interval
	 * @return `true` if this interval contains the other, `false` otherwise
	 */
	def contains(another: GenericInterval[T]): Boolean = {
		this.contains(another.start) && this.contains(another.end)
	}

	/**
	 * Determines whether this interval overlaps (intersects) the given interval.
	 * This is the case if (and only if) one or more of the items within the other
	 * interval are contained within this interval. (Note: this also includes cases
	 * in which this interval is entirely contained within the other interval.)
	 *
	 * @param another an interval
	 * @return `true` if the intervals overlap, `false` otherwise
	 */
	def overlaps(another: GenericInterval[T]): Boolean = {
		!this.isLaterThan(another) && !another.isLaterThan(this)
	}

	/**
	 * Creates, and returns a reference to, a new `GenericInterval` object that represents the union
	 * of this interval with the given interval. That is, the starting item of the new interval
	 * is the starting item of one of the two original intervals, whichever is smaller.
	 * Similarly, the end of the new interval is the greater of the two original ends.
	 *
	 * The two original intervals may overlap, but are not required to do so.
	 *
	 * Examples: The union of the interval from 1995 to 2003 with the interval from 2000 to 2013 is
	 * a new interval from 1995 to 2013. The union of the interval from 2000 to 2001 with the interval
	 * from 1995 to 1997 is a new interval from 1995 to 2001.
	 *
	 * @param another an interval
	 * @return the union of the two intervals
	 */
	def union(another: GenericInterval[T]): GenericInterval[T] = {
		new GenericInterval[T](order.min(this.start, another.start), order.max(this.end, another.end))
	}

	/**
	 * Creates, and returns a reference to, a new `GenericInterval` object that represents the intersection
	 * of this interval with the given interval. That is, the start of the new interval
	 * is the start of one of the two original intervals, whichever is greater.
	 * Similarly, the end of the new interval is the smaller of the two original ends.
	 *
	 * However, this method only produces a new interval in case the two original intervals overlap.
	 *
	 * Examples: The intersection of the interval from 1995 to 2003 with the interval from 2000 to 2013 is
	 * a new interval from 2000 to 2003. The intersection of the interval from 2000 to 2001 with the interval
	 * from 1995 to 1997 does not exist.
	 *
	 * @param another an interval
	 * @return the intersection of the two intervals, if one exists, in which case it is wrapped inside
	 *         a `Some` object. If no intersection exists, returns `None` instead.
	 * @see union
	 */
	def intersection(another: GenericInterval[T]): Option[GenericInterval[T]] = {
		if (this.overlaps(another)) {
			Some(new GenericInterval[T](order.max(this.start, another.start), order.min(this.end, another.end)))
		} else None
	}
}






