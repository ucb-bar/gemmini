// See README.md for license details.
package gemmini

object TestUtils {
  type Matrix[T] = Seq[Seq[T]]

  def rows[T](m: Matrix[T]) = m.length
  def cols[T](m: Matrix[T]) = m.head.length
  def dims[T](m: Matrix[T]) = (rows(m), cols(m))

  def mult[A](a: Matrix[A], b: Matrix[A])(implicit n: Numeric[A]): Matrix[A] = {
    import n._
    for (row <- a)
      yield for(col <- b.transpose)
        yield row zip col map Function.tupled(_*_) reduceLeft (_+_)
  }

  def add[A](a: Matrix[A], b: Matrix[A])(implicit n: Numeric[A]): Matrix[A] = {
    import n._
    for ((ra, rb) <- a zip b)
      yield for ((elema, elemb) <- ra zip rb)
        yield elema + elemb
  }

  def identity(dim: Int): Matrix[Int] = {
    for (i <- 0 until dim)
      yield Seq.fill(i)(0) ++ Seq(1) ++ Seq.fill(dim-i-1)(0)
  }

  def consecutive(dim: Int): Matrix[Int] = {
    (1 to dim*dim).grouped(dim).toSeq
  }

  def zero(dim: Int): Matrix[Int] = Seq.fill(dim, dim)(0)

  def rand(dim: Int, max: Int = 32): Matrix[Int] = Seq.fill(dim, dim)(scala.util.Random.nextInt(max))

  def print2DArray[A](a: Matrix[A]): Unit = {
    a.foreach {
      line => println(line.map(_.toString).reduce(_ + "\t" + _))
    }
  }
}
