package microbenchmarks

import streams._
import lms._

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.Fork
import org.openjdk.jmh.annotations.OutputTimeUnit
import java.util.concurrent.TimeUnit

import scala.lms.common._
import scala.lms.internal._
import scala.language.reflectiveCalls
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter

/*
 Naming conventions:
   - StagedStream the datatype
   - StagedStreamBenchmarks the name of the benchmark suite
   - StagedStreamBenchmarksS the staged benchmarks
   - StagedStreamBenchmarksT the type of the benchmarks after compilation
 */

trait StagedStreamBenchmarksS extends StagedStream {
  def sum (xs : Rep[Array[Int]]) : Rep[Int] =
    Stream[Int](xs)
      .fold(unit(0), ((a : Rep[Int]) => (b : Rep[Int]) => a + b))
  def sumOfSquares (xs : Rep[Array[Int]]) : Rep[Int]  =
    Stream[Int](xs)
      .map(x => x*x)
      .fold(unit(0), ((a : Rep[Int]) => (b : Rep[Int]) => a + b))
  def sumOfSquaresEven (xs : Rep[Array[Int]]) : Rep[Int]  =
    Stream[Int](xs)
      .filter(y => y % unit(2) == unit(0))
      .map(x => x*x)
      .fold(unit(0), ((a : Rep[Int]) => (b : Rep[Int]) => a + b))
  def cart (vHi : Rep[Array[Int]], vLo : Rep[Array[Int]]) : Rep[Int] =
    Stream[Int](vHi)
      .flatmap(d => Stream[Int](vLo).map (dp => dp * d))
      .fold(unit(0), ((a : Rep[Int]) => (b : Rep[Int]) => a + b))
  def maps (xs : Rep[Array[Int]]) : Rep[Int]  =
    Stream[Int](xs)
      .map(x => x * 1)
      .map(x => x * 2)
      .map(x => x * 3)
      .map(x => x * 4)
      .map(x => x * 5)
      .map(x => x * 6)
      .map(x => x * 7)
      .fold(unit(0), ((a : Rep[Int]) => (b : Rep[Int]) => a + b))
   def filters (xs : Rep[Array[Int]]) : Rep[Int]  =
     Stream[Int](xs)
       .filter(x => x > 1)
       .filter(x => x > 2)
       .filter(x => x > 3)
       .filter(x => x > 4)
       .filter(x => x > 5)
       .filter(x => x > 6)
       .filter(x => x > 7)
       .fold(unit(0), ((a : Rep[Int]) => (b : Rep[Int]) => a + b))
  def dotProduct (a : Rep[Array[Int]], b : Rep[Array[Int]]) : Rep[Int] =
     Stream[Int](a)
      .zip(((a : Rep[Int])=> (b : Rep[Int]) => a * b), Stream[Int](b))
      .fold(unit(0), ((a : Rep[Int])=> (b : Rep[Int]) => a + b))
  def flatMap_after_zipWith(xs : Rep[Array[Int]], ys: Rep[Array[Int]]) : Rep[Int] =
     Stream[Int](xs)
      .zip(((a : Rep[Int]) => (b : Rep[Int]) => a + b), Stream[Int](xs))
      .flatmap(x => Stream[Int](ys).map(y => (x + y)))
      .fold(unit(0), ((a : Rep[Int])=> (b : Rep[Int]) => a + b))
  def zipWith_after_flatMap(xs : Rep[Array[Int]], ys: Rep[Array[Int]]) : Rep[Int] =
     Stream[Int](xs)
      .flatmap(x => Stream[Int](ys).map(y => (x + y)))
      .zip(((a : Rep[Int]) => (b : Rep[Int]) => a + b), Stream[Int](xs))
      .fold(unit(0), ((a : Rep[Int])=> (b : Rep[Int]) => a + b))
  def flatMap_take (vHi : Rep[Array[Int]], vLo : Rep[Array[Int]]) : Rep[Int] =
     Stream[Int](vHi)
      .flatmap(x => Stream[Int](vLo).map(y => (x * y)))
      .take(20000000)
      .fold(unit(0), ((a : Rep[Int])=> (b : Rep[Int]) => a + b))
  def zip_flat_flat (vHi : Rep[Array[Int]], vLo : Rep[Array[Int]]) : Rep[Int] =
     Stream[Int](vHi)
      .flatmap(x => Stream[Int](vLo).map(y => (x * y)))
      .zip(x => (y:Rep[Int]) => x + y,
        Stream[Int](vLo)
          .flatmap(x => Stream[Int](vHi).map(y => (x * y))))
      .take(20000000)
      .fold(unit(0), ((a : Rep[Int])=> (b : Rep[Int]) => a + b))
  def zip_filter_filter (xs : Rep[Array[Int]], ys: Rep[Array[Int]]) : Rep[Int] =
     Stream[Int](xs)
      .filter(_ > 5)
      .zip(((a : Rep[Int]) => (b : Rep[Int]) => a + b), 
        Stream[Int](ys).filter(_ > 5))
      .fold(unit(0), ((a : Rep[Int])=> (b : Rep[Int]) => a + b))
}

trait StagedStreamBenchmarksT extends StagedStreamBenchmarksS
    with PrimitiveOpsExp
    with ArrayOpsExp
    with IfThenElseExpOpt
    with BooleanOpsExp
    with OrderingOpsExp
    with NumericOpsExp
    with WhileExp
    with VariablesExpOpt
    with EqualExpOpt
    with StringOpsExp
    with TupledFunctionsExp
    with TupleOpsExp
    with StructExp
    with ScalaCompileMultiParams {

  val sum : (Array[Int] => Int)
  val sumOfSquares : (Array[Int] => Int)
  val sumOfSquaresEven : (Array[Int] => Int)
  val cart : ((Array[Int], Array[Int]) => Int)
  val maps : (Array[Int] => Int)
  val filters : (Array[Int] => Int)
  val dotProduct : ((Array[Int], Array[Int]) => Int)
  val flatMap_after_zipWith : ((Array[Int], Array[Int]) => Int)
  val zipWith_after_flatMap : ((Array[Int], Array[Int]) => Int)
  val flatMap_take : ((Array[Int], Array[Int]) => Int)
  val zip_flat_flat : ((Array[Int], Array[Int]) => Int)
  val zip_filter_filter : ((Array[Int], Array[Int]) => Int)
}

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Thread)
@Fork(1)
class S {
  var N : Int = 100000000
  // var Limit : Int = (Int)(N*0.2)
  var v : Array[Int] = _
  var vHi : Array[Int] = _
  var vLo : Array[Int] = _
  var vFaZ : Array[Int] = _
  var vZaF : Array[Int] = _
  var staged : StagedStreamBenchmarksT = _

  @Setup
  def prepare() : Unit = {
    v          = Array.tabulate(N)(i => i.toInt % 10)
    vHi        = Array.tabulate(10000000)(i => i.toInt % 10)
    vLo        = Array.tabulate(10)(i => i.toInt % 10)
    vFaZ       = Array.tabulate(10000)(_.toInt)
    vZaF       = Array.tabulate(10000000)(_.toInt)

    staged = new StagedStreamBenchmarksT { self =>

      val codegen = new ScalaGenEffect
        with ScalaGenPrimitiveOps
        with ScalaGenArrayOps
        with ScalaGenTupledFunctions
        with ScalaGenIfThenElse
        with ScalaGenBooleanOps
        with ScalaGenOrderingOps
        with ScalaGenWhile
        with ScalaGenNumericOps
        with ScalaGenStruct
        with ScalaGenTupleOps
        with ScalaGenEqual
        with ScalaGenStringOps
        with ScalaGenVariables { val IR: self.type = self }

      dumpGeneratedCode = false
      
      override val sum : (Array[Int] => Int) = compile(self.sum)
      override val sumOfSquares : (Array[Int] => Int) = compile(self.sumOfSquares)
      override val sumOfSquaresEven : (Array[Int] => Int) = compile(self.sumOfSquaresEven)
      override val cart : ((Array[Int], Array[Int]) => Int) = compile2(self.cart)
      override val maps : (Array[Int] => Int) = compile(self.maps)
      override val filters : (Array[Int] => Int) = compile(self.filters)
      override val dotProduct : ((Array[Int], Array[Int]) => Int) = compile2(self.dotProduct)
      override val flatMap_after_zipWith : ((Array[Int], Array[Int]) => Int) = compile2(self.flatMap_after_zipWith)
      override val zipWith_after_flatMap : ((Array[Int], Array[Int]) => Int) = compile2(self.zipWith_after_flatMap)
      override val flatMap_take : ((Array[Int], Array[Int]) => Int) = compile2(self.flatMap_take)
      //override lazy val zip_flat_flat : ((Array[Int], Array[Int]) => Int) = compile2(self.zip_flat_flat) // FIXME crashes
      override val zip_flat_flat : ((Array[Int], Array[Int]) => Int) = (new staged$0).apply _
      //override lazy val zip_filter_filter : ((Array[Int], Array[Int]) => Int) = compile2(self.zip_filter_filter) // FIXME crashes
      override val zip_filter_filter : ((Array[Int], Array[Int]) => Int) = (new staged$1).apply _
    }
  }

  @Benchmark
  def sum_staged () : Int = {
    val sum : Int = staged.sum(v)
    sum
  }

  @Benchmark
  def sumOfSquares_staged () : Int = {
    val sum : Int = staged.sumOfSquares(v)
    sum
  }

  @Benchmark
  def sumOfSquaresEven_staged () : Int = {
    val sum : Int = staged.sumOfSquaresEven(v)
    sum
  }

  @Benchmark
  def cart_staged () : Int = {
    val sum : Int = staged.cart(vHi, vLo)
    sum
  }

  @Benchmark
  def maps_staged () : Int = {
    val sum : Int = staged.maps(v)
    sum
  }

  @Benchmark
  def filters_staged () : Int = {
    val sum : Int = staged.filters(v)
    sum
  }

  @Benchmark
  def dotProduct_staged () : Int = {
    val sum : Int = staged.dotProduct(vHi, vHi)
    sum
  }

  @Benchmark
  def flatMap_after_zipWith_staged () : Int = {
    val sum : Int = staged.flatMap_after_zipWith(vFaZ, vFaZ)
    sum
  }

  @Benchmark
  def zipWith_after_flatMap_staged () : Int = {
    val sum : Int = staged.zipWith_after_flatMap(vZaF, vZaF)
    sum
  }

  @Benchmark
  def flatMap_take_staged () : Int = {
    val sum : Int = staged.flatMap_take(v, vLo)
    sum
  }
  
  @Benchmark
  def zip_flat_flat_staged () : Int = {
    val sum : Int = staged.zip_flat_flat(v, vLo)
    sum
  }
  
  @Benchmark
  def zip_filter_filter_staged () : Int = {
    val sum : Int = staged.zip_filter_filter(vHi, vHi)
    sum
  }
  
}

/*****************************************
  Emitting Generated Code
*******************************************/
class staged$0 extends ((Array[Int], Array[Int])=>(Int)) {
def apply(x0:Array[Int], x1:Array[Int]): Int = {
var x2: Int = 0
val x3 = x0.length
var x4: Int = x3
var x5: Int = 0
val x6 = {x7: (Unit) =>
(): Unit
}
var x8: scala.Function1[Unit, Unit] = x6
var x9: Boolean = true
var x10: Int = 0
val x18 = x1.length
val x11 = {x12: (Unit) =>
val x13 = x5
val x14 = x4
val x15 = x13 < x14
x9 = x15
val x43 = if (x15) {
var x19: Int = x18
var x20: Int = 0
val x21 = x8
val x17 = x0(x13)
val x22 = {x23: (Unit) =>
val x24 = x20
val x25 = x19
val x26 = x24 < x25
val x36 = if (x26) {
val x27 = x1(x24)
val x28 = x17 * x27
x10 = x28
val x30 = x24 + 1
x20 = x30
()
} else {
x8 = x21
val x34 = x21(())
x34
}
x36: Unit
}
x8 = x22
val x39 = x22(())
val x40 = x13 + 1
x5 = x40
()
} else {
()
}
x43: Unit
}
x8 = x11
val x46 = x11(())
var x47: Int = x18
var x48: Int = 0
val x49 = x9
var x50: Boolean = x49
var x51: Int = 20000000
val x97 = while ({val x52 = x51
val x54 = x50
val x55 = x48
val x56 = x47
val x53 = x52 > 0
val x57 = x55 < x56
val x58 = x54 && x57
val x59 = x53 && x58
x59}) {
val x61 = x48
var x63: Int = x3
var x64: Int = 0
val x62 = x1(x61)
val x93 = while ({val x65 = x64
val x66 = x63
val x68 = x50
val x70 = x51
val x67 = x65 < x66
val x69 = x68 && x67
val x71 = x70 > 0
val x72 = x71 && x69
x72}) {
val x74 = x64
val x77 = x10
var x78: Int = x77
val x79 = x8
val x80 = x79(())
val x82 = x51
val x83 = x82 - 1
x51 = x83
val x85 = x2
val x75 = x0(x74)
val x76 = x62 * x75
val x81 = x77 + x76
val x86 = x85 + x81
x2 = x86
val x88 = x9
x50 = x88
val x90 = x74 + 1
x64 = x90
()
}
val x94 = x61 + 1
x48 = x94
()
}
val x98 = x2
x98
}
}
/*****************************************
  End of Generated Code
*******************************************/

/*****************************************
  Emitting Generated Code
*******************************************/
class staged$1 extends ((Array[Int], Array[Int])=>(Int)) {
def apply(x100:Array[Int], x101:Array[Int]): Int = {
var x102: Int = 0
val x103 = x100.length
var x104: Int = x103
var x105: Int = 0
val x106 = {x107: (Unit) =>
(): Unit
}
var x108: scala.Function1[Unit, Unit] = x106
var x109: Boolean = true
var x110: Int = 0
val x111 = {x112: (Unit) =>
val x113 = x105
val x114 = x104
val x115 = x113 < x114
x109 = x115
val x134 = if (x115) {
val x118 = x108
val x117 = x100(x113)
val x121 = x117 > 5
val x119 = {x120: (Unit) =>
val x127 = if (x121) {
x110 = x117
()
} else {
x108 = x118
val x125 = x118(())
x125
}
x127: Unit
}
x108 = x119
val x130 = x119(())
val x131 = x113 + 1
x105 = x131
()
} else {
()
}
x134: Unit
}
x108 = x111
val x137 = x111(())
val x138 = x101.length
var x139: Int = x138
var x140: Int = 0
val x141 = x109
var x142: Boolean = x141
val x167 = while ({val x143 = x142
val x144 = x140
val x145 = x139
val x146 = x144 < x145
val x147 = x143 && x146
x147}) {
val x149 = x140
val x150 = x101(x149)
val x151 = x150 > 5
val x163 = if (x151) {
val x152 = x110
var x153: Int = x152
val x154 = x108
val x155 = x154(())
val x157 = x102
val x156 = x152 + x150
val x158 = x157 + x156
x102 = x158
val x160 = x109
x142 = x160
()
} else {
()
}
val x164 = x149 + 1
x140 = x164
()
}
val x168 = x102
x168
}
}
/*****************************************
  End of Generated Code
*******************************************/
