package dreae.convolve

import dreae.convolve.Operator.OpType

trait Operator[A, B, O <: OpType, R] extends ((A, B) => R)

object Operator {
  trait OpType
  trait OpAdd extends OpType
  trait OpSub extends OpType

  implicit object OpAddII extends Operator[Int, Int, OpAdd, Int] {
    override def apply(a: Int, b: Int) = a + b
  }
  implicit object OpAddIL extends Operator[Int, Long, OpAdd, Long] {
    override def apply(a: Int, b: Long) = a + b
  }
}
