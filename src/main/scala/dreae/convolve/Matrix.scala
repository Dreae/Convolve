package dreae.convolve

import Operator._

class Matrix[T](val matr: Seq[Seq[T]]) {
  val rows = matr.size
  val cols = matr.head.size

  def apply(row: Int)(col: Int) = matr(row)(col)

  def +[B,R](other: Matrix[B])(implicit add: Operator[T,B,OpAdd,R]) = {
    if(other.rows != rows || other.cols != cols) throw new Exception()
    val newMatr = for(i <- 0 until matr.size) yield {
      for(j <- 0 until matr(i).size) yield add(this(i)(j), other(i)(j))
    }
    new Matrix(newMatr)
  }

  def -[B,R](other: Matrix[B])(implicit sub: Operator[T,B,OpSub,R]) = {
    if(other.rows != rows || other.cols != cols) throw new Exception()
    val newMatr = for(i <- 0 until matr.size) yield {
      for(j <- 0 until matr(i).size) yield sub(this(i)(j), other(i)(j))
    }
    new Matrix(newMatr)
  }

  def *[B,R,F](other: Matrix[B])(implicit mul: Operator[T,B,OpMul,R], add: Operator[R,R,OpAdd,R]) = {
    if(cols != other.rows) throw new Exception()
    val newMatr = for(i <- 0 until rows) yield {
      for(p <- 0 until other.cols) yield {
        val newCol = for(m <- 0 until cols) yield {
          mul(this(i)(m), other(m)(p))
        }
        newCol.tail.foldLeft(newCol.head)(add(_,_))
      }
    }
    new Matrix(newMatr)
  }

  def *[B,R](c: B)(implicit mul: Operator[T,B,OpMul,R]) = {
    val newMatr = for(row <- matr) yield {
      for(e <- row) yield mul(e, c)
    }
    new Matrix(newMatr)
  }

  lazy val T = {
    val newMatr = for(i <- 0 until cols) yield {
      for(j <- 0 until rows) yield this(j)(i)
    }
    new Matrix(newMatr)
  }

  override lazy val toString = {
    val str = new StringBuilder()
    for (i <- 0 until rows) {
      for(j <- 0 until cols) str.append(this(i)(j) + " ")
      str.append('\n')
    }
    str.mkString
  }
}
