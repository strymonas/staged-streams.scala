import streams._
import lms._
import org.scalacheck._

import scala.lms.common._
import scala.lms.internal._
import scala.language.reflectiveCalls
import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter

trait StagedStreamTests extends StagedStream {
  def sizeTest (xs : Rep[Array[Int]]) : Rep[Int] =
    Stream[Int](xs)
      .fold(0, ((a : Rep[Int])=> (z : Rep[Int]) => a + 1))
  def sumTest (xs : Rep[Array[Int]]) : Rep[Int] =
    Stream[Int](xs)
      .fold(0, ((a : Rep[Int])=> (b : Rep[Int]) => a + b))
  def mapFoldTest (xs : Rep[Array[Int]]) : Rep[Int] =
    Stream[Int](xs)
      .map(_*2)
      .fold(0, ((a : Rep[Int])=> (b : Rep[Int]) => a + b))
  def flatmapTest (xs : Rep[Array[Int]]) : Rep[Int] =
    Stream[Int](xs)
      .flatmap(d => Stream[Int](xs).map (dp => dp * d))
      .fold(unit(0), ((a : Rep[Int]) => (b : Rep[Int]) => a + b))
  def filterTest (xs : Rep[Array[Int]]) : Rep[Int] =
    Stream[Int](xs)
      .filter(d => d % 2 == 0)
      .fold(unit(0), ((a : Rep[Int]) => (b : Rep[Int]) => a + b))
  def filterFilterTest (xs : Rep[Array[Int]]) : Rep[Int] =
    Stream[Int](xs)
      .filter(d => d % 2 == 0)
      .filter(d => d % 3 == 0)
      .fold(unit(0), ((a : Rep[Int]) => (b : Rep[Int]) => a + b))
  def takeSizeTest (xs : Rep[Array[Int]], n : Rep[Int]) : Rep[Int] =
    Stream[Int](xs)
      .take(n)
      .fold(0, ((a : Rep[Int])=> (z : Rep[Int]) => a + 1))
  def flatMapTakeSizeTest (xs : Rep[Array[Int]], n : Rep[Int], m : Rep[Int]) : Rep[Int] =
    Stream[Int](xs)
      .flatmap(x => Stream[Int](xs).take(n))
      .take(m)
      .fold(0, ((a : Rep[Int])=> (z : Rep[Int]) => a + 1))
  def zip_with_simpleTest(xs : Rep[Array[Int]], ys: Rep[Array[Int]]) : Rep[Int] =
    Stream[Int](xs)
      .zip(((a : Rep[Int])=> (b : Rep[Int]) => a + b), Stream[Int](ys))
      .fold(0, ((a : Rep[Int])=> (b : Rep[Int]) => a + b))
  def zip_with_genTest(xs : Rep[Array[Int]], ys: Rep[Array[Int]]) : Rep[Int] =
    Stream[Int](xs)
      .flatmap(x => Stream[Int](ys).map(y => (x + y)))
      .zip(((a : Rep[Int]) => (b : Rep[Int]) => a + b), Stream[Int](xs))
      .fold(0, ((a : Rep[Int])=> (b : Rep[Int]) => a + b))
  def makeLinearTest(xs : Rep[Array[Int]]) : Rep[Int] = {
      val Stream(stream) = Stream[Int](xs).flatmap(x => Stream[Int](xs).map(y => (x + y)))
      Stream(Linear(Stream[Int](xs).makeLinear(stream)))
      .fold(0, ((a : Rep[Int])=> (b : Rep[Int]) => a + b))
  }
  def zip_flat_flat_take (vHi : Rep[Array[Int]], vLo : Rep[Array[Int]], num: Rep[Int]) : Rep[Int] =
    Stream[Int](vHi)
      .flatmap(x => Stream[Int](vLo).map(y => (x * y)))
      .zip(x => (y:Rep[Int]) => x + y,
        Stream[Int](vLo)
          .flatmap(x => Stream[Int](vHi).map(y => (x * y))))
      .take(num)
      .fold(unit(0), ((a : Rep[Int])=> (b : Rep[Int]) => a + b))
  def zip_filter_filter (xs : Rep[Array[Int]], ys: Rep[Array[Int]], num: Rep[Int]) : Rep[Int] =
    Stream[Int](xs)
      .filter(_ < num)
      .zip(((a : Rep[Int]) => (b : Rep[Int]) => a + b),
        Stream[Int](ys).filter(_ > num))
      .fold(unit(0), ((a : Rep[Int])=> (b : Rep[Int]) => a + b))
}

object StagedStreamSpec extends Properties("Staged Stream") {

  import Prop.forAll
  import Prop.BooleanOperators

  val staged = new StagedStreamTests
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
      with FunctionsRecursiveExp
      with TupleOpsExp
      with StructExp
      with ScalaCompileMultiParams { self =>

    val codegen = new ScalaGenEffect
        with ScalaGenPrimitiveOps
        with ScalaGenArrayOps
        with ScalaGenFunctions
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

    val sizeTest : (Array[Int] => Int) = compile(self.sizeTest)
    val sumTest : (Array[Int] => Int) = compile(self.sumTest)
    val mapFoldTest : (Array[Int] => Int) = compile(self.mapFoldTest)
    val flatmapTest : (Array[Int] => Int) = compile(self.flatmapTest)
    val filterTest : (Array[Int] => Int) = compile(self.filterTest)
    val filterFilterTest : (Array[Int] => Int) = compile(self.filterFilterTest)
    val takeSizeTest : ((Array[Int], Int) => Int) = compile2(self.takeSizeTest)
    val flatMapTakeSizeTest : ((Array[Int], Int, Int) => Int) = compile3(self.flatMapTakeSizeTest)
    val zip_with_simpleTest : ((Array[Int], Array[Int]) => Int) = compile2(self.zip_with_simpleTest)
    val zip_with_genTest : ((Array[Int], Array[Int]) => Int) = compile2(self.zip_with_genTest)
    val makeLinearTest : (Array[Int] => Int) = compile(self.makeLinearTest)
    val zip_flat_flat_take : ((Array[Int], Array[Int], Int) => Int) = compile3(self.zip_flat_flat_take)
    val zip_filter_filter : ((Array[Int], Array[Int], Int) => Int) = compile3(self.zip_filter_filter)
  }

  property("size") = forAll { (xs: Array[Int]) =>
    var x = xs.length
    var y = staged.sizeTest(xs)
    x == y
  }

  property("sum") = forAll { (xs: Array[Int]) =>
    var x = xs.sum
    var y = staged.sumTest(xs)
    x == y
  }

  property("map/fold") = forAll { (xs: Array[Int]) =>
    var x = xs.map(_*2).fold(0)(_+_)
    var y = staged.mapFoldTest(xs)
    x == y
  }

  property("flatmap") = forAll { (xs: Array[Int]) =>
    var x = xs.flatMap(d => xs.map (dp => dp * d)).fold(0)(_+_)
    var y = staged.flatmapTest(xs)
    x == y
  }

  property("filter") = forAll { (xs: Array[Int]) =>
    var x = xs.filter(d => d % 2 == 0).fold(0)(_+_)
    var y = staged.filterTest(xs)
    x == y
  }

  property("filter/filter") = forAll { (xs: Array[Int]) =>
    var x = xs.filter(d => d % 2 == 0).filter(d => d % 3 == 0).fold(0)(_+_)
    var y = staged.filterFilterTest(xs)
    x == y
  }

  property("take/size") = forAll { (xs: Array[Int], n: Int) =>
    var x = xs.take(n).size
    var y = staged.takeSizeTest(xs, n)
    x == y
  }

  property("flatmap/take/size") = forAll { (xs: Array[Int], n: Int, m : Int) =>
    var x = xs.flatMap(x => xs.take(n)).take(m).size
    var y = staged.flatMapTakeSizeTest(xs, n, m)
    x == y
  }

  property("zip simple/sum") = forAll { (xs: Array[Int], ys: Array[Int]) =>
    var x = (xs, ys).zipped.map(_ + _).sum
    var y = staged.zip_with_simpleTest(xs, ys)
    x == y
  }

  property("zip gen/sum") = forAll { (xs: Array[Int], ys: Array[Int]) =>
    var x = (xs.flatMap((x : Int) => ys.map(y => x + y)), xs).zipped.map(_ + _).sum
    val y = staged.zip_with_genTest(xs, ys)
    x == y
  }

  property("makeLinearTest") = forAll { (xs: Array[Int]) =>
    var x = xs.flatMap(x => xs.map(y => (x + y))).sum
    val y = staged.makeLinearTest(xs)
    x == y
  }

  property("zip/flat/flat") = forAll { (xs: Array[Int], ys: Array[Int], num: Int) =>
    var x = (xs.flatMap((x : Int) => ys.map(y => x * y)), ys.flatMap((x : Int) => xs.map(y => x * y))).zipped.map(_ + _).take(num).sum
    val y = staged.zip_flat_flat_take(xs, ys, num)
    x == y
  }

  property("zip/filter/filter") = forAll { (xs: Array[Int], ys: Array[Int], num: Int) =>
    var x = (xs.filter(_ < num), ys.filter(_ > num)).zipped.map(_ + _).sum
    val y = staged.zip_filter_filter(xs, ys, num)
    x == y
  }
}
