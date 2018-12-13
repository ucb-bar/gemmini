// See README.md for license details.
package systolic

object SystolicUtils {
  def mult[A](a: Seq[Seq[A]], b: Seq[Seq[A]])(implicit n: Numeric[A]) = {
    import n._
    for (row <- a)
      yield for(col <- b.transpose)
        yield row zip col map Function.tupled(_*_) reduceLeft (_+_)
  }

  def print2DArray[A](a: Seq[Seq[A]]): Unit = {
    a.foreach {
      line => println(line.map(_.toString).reduce(_ + "\t" + _))
    }
  }
}
