package eu.gruchala.other

trait ListElem {

    // returns value of ListElem, if it is non empty
    // otherwise throws UnsupportedOperationException
    def value: Int

    // returns next ListElem connected with current one
    // if current is Empty then it should throw UnsupportedOperationException
    def next: ListElem

    // checks if ListElem is empty
    def isEmpty: Boolean

    // create new ListElem with current one as tail, same as :: in standard Scala List API
    // :: called on Empty should create one element list
    def ::(e: Int): ListElem

    // returns list length
    def length: Int

    // returns maximum list element
    // called on empty list should throw UnsupportedOperationException
    def max: Int

    // creates new list by applying function f to each list element
    // map on empty list should always return empty list
    def map(f: Int => Int): ListElem

    // creates new list only with elements satisfying given predicate f
    // filter on empty list should always return empty list
    def filter(f: Int => Boolean): ListElem
}

object Empty extends ListElem {

    // returns value of ListElem, if it is non empty
    def value: Int = throw new UnsupportedOperationException

    // returns next ListElem connected with current one
    def next: ListElem = throw new UnsupportedOperationException

    // checks if ListElem is empty
    def isEmpty: Boolean = true

    // create new ListElem with current one as tail, same as :: in standard Scala List API
    def ::(e: Int): ListElem = new NonEmpty(e, this)

    // returns list length
    def length: Int = 0

    // returns maximum list element
    def max: Int = throw new UnsupportedOperationException

    // creates new list by applying function f to each list element
    def map(f: (Int) => Int): ListElem = this

    // creates new list only with elements satisfying given predicate f
    def filter(f: (Int) => Boolean): ListElem = this
}

class NonEmpty(num: Int, nextElem: ListElem) extends ListElem {

    // returns value of ListElem, if it is non empty
    def value: Int = num

    // returns next ListElem connected with current one
    def next: ListElem = nextElem

    // checks if ListElem is empty
    def isEmpty: Boolean = false

    // create new ListElem with current one as tail, same as :: in standard Scala List API
    def ::(e: Int): ListElem = new NonEmpty(e, this)

    // returns list length
    def length: Int = 1 + next.length

    // returns maximum list element
    def max: Int = {
        def iter(curMax: Int, elem: ListElem) : Int =
            if (elem.isEmpty) curMax
            else iter(if (curMax < elem.value) elem.value else curMax, elem.next)
        iter(value, nextElem)
    }

    // creates new list by applying function f to each list element
    def map(f: (Int) => Int): ListElem = new NonEmpty(f(value), next.map(f))

    // creates new list only with elements satisfying given predicate f
    def filter(f: (Int) => Boolean): ListElem = if (f(value)) new NonEmpty(value, next.filter(f)) else next.filter(f)
}

object FunList {

    def apply(ints: Int*) : ListElem =
        if (ints.isEmpty) Empty else new NonEmpty(ints.head, apply(ints.tail: _*))
}
