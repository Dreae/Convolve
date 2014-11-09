package dreae.convolve

import dreae.convolve.Operator.OpType

trait Operator[A, B, O <: OpType, R] extends ((A, B) => R)

object Operator {
  trait OpType
  trait OpAdd extends OpType
  trait OpSub extends OpType
  trait OpMul extends OpType

  implicit object OpAddII extends Operator[Int, Int, OpAdd, Int] {
    override def apply(a: Int, b: Int) = a + b
  }
  implicit object OpSubII extends Operator[Int, Int, OpSub, Int] {
    override def apply(a: Int, b: Int) = a - b
  }
  implicit object OpMulII extends Operator[Int, Int, OpMul, Int] {
    override def apply(a: Int, b: Int) = a * b
  }

  implicit object OpAddLL extends Operator[Long, Long, OpAdd, Long] {
    override def apply(a: Long, b: Long) = a + b
  }
  implicit object OpSubLL extends Operator[Long, Long, OpSub, Long] {
    override def apply(a: Long, b: Long) = a - b
  }
  implicit object OpMulLL extends Operator[Long, Long, OpMul, Long] {
    override def apply(a: Long, b: Long) = a * b
  }

  implicit object OpAddFF extends Operator[Float, Float, OpAdd, Float] {
    override def apply(a: Float, b: Float) = a + b
  }
  implicit object OpSubFF extends Operator[Float, Float, OpSub, Float] {
    override def apply(a: Float, b: Float) = a - b
  }
  implicit object OpMulFF extends Operator[Float, Float, OpMul, Float] {
    override def apply(a: Float, b: Float) = a * b
  }

  implicit object OpAddDD extends Operator[Double, Double, OpAdd, Double] {
    override def apply(a: Double, b: Double) = a + b
  }
  implicit object OpSubDD extends Operator[Double, Double, OpSub, Double] {
    override def apply(a: Double, b: Double) = a - b
  }
  implicit object OpMulDD extends Operator[Double, Double, OpMul, Double] {
    override def apply(a: Double, b: Double) = a * b
  }

  implicit object OpAddIL extends Operator[Int, Long, OpAdd, Long] {
    override def apply(a: Int, b: Long) = a + b
  }
  implicit object OpSubIL extends Operator[Int, Long, OpSub, Long] {
    override def apply(a: Int, b: Long) = a - b
  }
  implicit object OpMulIL extends Operator[Int, Long, OpMul, Long] {
    override def apply(a: Int, b: Long) = a * b
  }

  implicit object OpAddLI extends Operator[Long, Int, OpMul, Long] {
    override def apply(a: Long, b: Int) = a + b
  }
  implicit object OpSubLI extends Operator[Long, Int, OpMul, Long] {
    override def apply(a: Long, b: Int) = a - b
  }
  implicit object OpMulLI extends Operator[Long, Int, OpMul, Long] {
    override def apply(a: Long, b: Int) = a * b
  }
}
