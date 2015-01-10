
/*
 * Tree Generators allows us to create generators for different types
 * either integers, booleans even random lists or trees.
 * As an application we mention the usage of random generators in Tests.
 */

trait Generator[+T] {
  self =>             // self is an alias for 'this'
  def generate: T

  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate)
  }

  def flatMap[S](f: T => Generator[S]):Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }
}
