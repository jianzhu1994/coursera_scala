package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
  * This class is a test suite for the methods in object FunSets. To run
  * the test suite, you can either:
  *  - run the "test" command in the SBT console
  *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
  */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
    * Link to the scaladoc - very clear and detailed tutorial of FunSuite
    *
    * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
    *
    * Operators
    *  - test
    *  - ignore
    *  - pending
    */

  /**
    * Tests are written using the "test" operator and the "assert" method.
    */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
    * For ScalaTest tests, there exists a special equality operator "===" that
    * can be used inside "assert". If the assertion fails, the two values will
    * be printed in the error message. Otherwise, when using "==", the test
    * error message will only say "assertion failed", without showing the values.
    *
    * Try it out! Change the values so that the assertion fails, and look at the
    * error message.
    */
  test("adding ints") {
    assert(1 + 2 === 3)
  }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
    * When writing tests, one would often like to re-use certain values for multiple
    * tests. For instance, we would like to create an Int-set and have multiple test
    * about it.
    *
    * Instead of copy-pasting the code for creating the set into every test, we can
    * store it in the test class using a val:
    *
    *   val s1 = singletonSet(1)
    *
    * However, what happens if the method "singletonSet" has a bug and crashes? Then
    * the test methods are not even executed, because creating an instance of the
    * test class fails!
    *
    * Therefore, we put the shared values into a separate trait (traits are like
    * abstract classes), and create an instance inside each test method.
    *
    */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val positiveInts = (x: Int) => x > 0
    val negativeInts = (x: Int) => x < 0
    val zeroToTen = (x: Int) => x > 0 && x < 10

    val empty = (x: Int) => false
  }

  /**
    * This test is currently disabled (by using "ignore") because the method
    * "singletonSet" is not yet implemented and the test would fail.
    *
    * Once you finish your implementation of "singletonSet", exchange the
    * function "ignore" by "test".
    */
  test("singletonSet contains its element") {

    /**
      * We create a new instance of the "TestSets" trait, this gives us access
      * to the values "s1" to "s3".
      */
    new TestSets {
      /**
        * The string argument of "assert" is a message that is printed in case
        * the test fails. This helps identifying which assertion failed.
        */
      assert(contains(s1, 1), "Singleton contains 1")
      assert(contains(s2, 2), "Singleton contains 2")
      assert(contains(s3, 3), "Singleton contains 3")
    }
  }

  test("singletonSet doesn't contain other things") {

    new TestSets {
      assert(!contains(s1, -1), "Singleton doesn't contain -1")
      assert(!contains(s1, 2), "Singleton doesn't contain -1")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "1 in {1,2}")
      assert(contains(s, 2), "2 in {1,2}")
      assert(!contains(s, 3), "3 not {1,2}")

      val t = union(s, s3)
      assert(contains(t, 3), "3 in {1, 2, 3")

      val u = union(s1, s)
      assert(contains(s, 1), "1 in {1, 1, 2}")

      val all = union(positiveInts, negativeInts)
      assert(contains(all, 23502), "all in set of all")
      assert(contains(all, -19241), "all in set of all")

      val none = union(empty, empty)
      assert(!contains(none, 1), "empty has nothing")

      val justOne = union(empty, s1)
      assert(contains(justOne, 1), "empty and something makes something")
    }
  }

  test("intersect contains some elements") {

    new TestSets {
      val s = union(s2, s3)
      val t = union(s1, s2)
      val u = intersect(s, t)

      assert(contains(u, 2), "2 in {2,3} U {1,2}")
      assert(!contains(u, 1), "1 not in {2,3} U {1,2}")
      assert(!contains(u, 1), "3 not in {2,3} U {1,2}")

      val none = intersect(empty, positiveInts)
      assert(!contains(none, 0), "nothing is in the intersection of nothing and anything")

      val noneAgain = intersect(negativeInts, positiveInts)
      assert(!contains(noneAgain, 0), "nothing is in the intersection of negs and pos")

      assert(contains(intersect(positiveInts, zeroToTen), 5), "something is in x > 0 and x < 10")
    }
  }

  test("set difference tests") {

    new TestSets {
      val s = union(s1, s2)
      val t = union(s2, s3)
      val u = diff(s,t)

      assert(!contains(u, 2), "2 not in {1, 2} - {2, 3}")
      assert(contains(u, 1), "1 in {1, 2} - {2, 3}")

      val greaterThanTen = diff(positiveInts, zeroToTen)
      assert(!contains(greaterThanTen, 9), "set diff with positive ints")
      assert(contains(greaterThanTen, 11), "set difference with positive ints")
    }
  }

  test("filter things out of a set") {
    new TestSets {
      val even = (x: Int) => x % 2 == 0
      val odds = (x: Int) => x % 2 == 1

      val evenPositiveInts = filter(positiveInts, even)
      assert(contains(evenPositiveInts, 2), "2 is even")
      assert(!contains(evenPositiveInts, 3), "3 is not even")
      assert(contains(evenPositiveInts, 239422), "239422 is even")

      val oddPositiveInts = filter(positiveInts, odds)
      assert(!contains(oddPositiveInts, 2), "2 is not odd")
      assert(contains(oddPositiveInts, 3), "3 is odd")
    }

  }

  test("forall ") {

    new TestSets {
      assert(forall(positiveInts, (x: Int) => x > 0), "positive ints are positive")
      assert(!forall(negativeInts, (x: Int) => x > 0), "negative ints aren't positive")
      assert(forall(s2, (x: Int) => x % 2 == 0), "2 is 0 mod 2 ")

      assert(!forall(positiveInts, (x: Int) => x.abs != 1000), "check the boundary!")

      assert(forall((x: Int) => false, (x: Int) => true), "empty sets are true")
      assert(forall(empty, empty), "empty sets are true")

      assert(forall((x: Int) => true, (x: Int) => true), "forall true, if true is true")
      assert(!forall((x: Int) => true, empty), "forall true, if false is false")
    }

  }

  test("exists!") {
    new TestSets {
      assert(exists(zeroToTen, (x: Int) => x == 5), "5 is in 0 to 10.")
      assert(!exists(zeroToTen, (x: Int) => x == -5), "-5 is not in 0 to 10.")

      assert(!exists(empty, (x: Int) => true), "empty sets are always false")
      assert(!exists(empty, (x: Int) => false), "empty sets are always false")

      assert(exists((x:Int) => true, (x: Int) => true), "universal sets are always false")
      assert(!exists((x:Int) => true, (x: Int) => false), "false condition is always false")
    }
  }

  test("map!") {
    new TestSets {
      val identitySingleton = map(s1, (x: Int) => x)
      printSet(identitySingleton)
      assert(exists(identitySingleton, (x:Int) => x == 1), "identity mapping")
      assert(forall(identitySingleton, (x: Int) => x == 1), "identity mapping")

      val doubler = map(s1, (x: Int) => x * 2)
      assert(contains(doubler, 2), "x = x * 2")

      val tripler = map(positiveInts, (x:Int) => x * 3)
      assert(forall(tripler, (x:Int) => x % 3 == 0), "x*3 is 0 mod 3 for all x")

      val cuber = map(zeroToTen, (x: Int) => x * x * x)
      assert(contains(cuber, 729), "9 cubed is 729")
    }

  }
}