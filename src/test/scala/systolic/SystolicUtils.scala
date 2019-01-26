// See README.md for license details.
package systolic

object SystolicUtils {
  type Matrix[T] = Seq[Seq[T]]

  def rows[T](m: Matrix[T]) = m.length
  def cols[T](m: Matrix[T]) = m.head.length

  def mult[A](a: Seq[Seq[A]], b: Seq[Seq[A]])(implicit n: Numeric[A]) = {
    import n._
    for (row <- a)
      yield for(col <- b.transpose)
        yield row zip col map Function.tupled(_*_) reduceLeft (_+_)
  }

  def identity(dim: Int): Matrix[Int] = {
    for (i <- 0 until dim)
      yield Seq.fill(i)(0) ++ Seq(1) ++ Seq.fill(dim-i-1)(0)
  }

  def zero(dim: Int): Matrix[Int] = Seq.fill(dim, dim)(0)

  def rand(r: Int, c: Int, max: Int = 8): Matrix[Int] = Seq.fill(r, c)(scala.util.Random.nextInt(max))

  def print2DArray[A](a: Matrix[A]): Unit = {
    a.foreach {
      line => println(line.map(_.toString).reduce(_ + "\t" + _))
    }
  }
}
