

object generators {

  val integers = new Generator[Int] {
    def generate = scala.util.Random.nextInt()
 }

  val booleans = integers map (_ >= 0)
  
  def pairs[T, S](t: Generator[T], u: Generator[S]) = {
    t flatMap(x => u.map(y => (x,y)))
  }

  def single[T](x: T): Generator[T] = new Generator[T] {
    override def generate: T = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] =
    for(x <-integers) yield lo + x %(hi - lo)

  def oneOf[T](xs: T*): Generator[T] =
    for(idx <- choose(0, xs.length)) yield xs(idx)

  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if(isEmpty) emptyLists else nonEmptyLists
  } yield list

  def emptyLists = single(Nil)

  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail

/*
 * Random Testing : how to apply Random generators in Unit Tests
 */

  def test[T](g: Generator[T], numTimes: Int = 100)
    (test: T => Boolean): Unit = {
      for(i <- 0 until numTimes) {
        val value = g.generate
        assert(test(value), "test failed for " + value)
      }
      println("passed " + numTimes + "tests ")
  }

}
