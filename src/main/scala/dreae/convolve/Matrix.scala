package dreae.convolve

import dreae.convolve.Operator.OpAdd

class Matrix[T](val matr: Seq[Seq[T]]) {
  val rows = matr.size
  val cols = matr.head.size

  def this(array: List[T], cols: Int) = this{
    val rows = {
      for(i <- 0 until (array.size / cols)) yield {
        array.slice(i * cols, (i * cols) + cols)
      }
    }.toList
    rows
  }

  def apply(col: Int)(row: Int) = matr(col)(row)

  def +[B](other: Matrix[B])(implicit add: Operator[T,B,OpAdd,_]) = {
    if(other.rows != rows || other.cols != cols) {
      throw new Exception()
    }
    val newMatr = for(i <- 0 until matr.size) yield {
      for(j <- 0 until matr(i).size) yield add(this(i)(j), other(i)(j))
    }
    new Matrix(newMatr)
  }
}
