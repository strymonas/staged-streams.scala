package microbenchmarks

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.Fork
import org.openjdk.jmh.annotations.OutputTimeUnit
import java.util.concurrent.TimeUnit

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@State(Scope.Thread)
@Fork(1)
class B {

   var N : Int = _
   var v : Array[Int] = _
   var vHi : Array[Int] = _
   var vLo : Array[Int] = _
   var vFaZ : Array[Int] = _
   var vZaF : Array[Int] = _

   @Setup
   def prepare() : Unit = {
      N = 100000000
      v          = Array.tabulate(N)(i => i.toInt % 10)
      vHi        = Array.tabulate(10000000)(i => i.toInt % 10)
      vLo        = Array.tabulate(10)(i => i.toInt % 10)
      vFaZ       = Array.tabulate(10000)(_.toInt)
      vZaF       = Array.tabulate(10000000)(_.toInt)
   }

   @Benchmark
   def sum_baseline () : Int = {
      var i=0
      var sum=0
      while (i < v.length) {
         sum += v(i)
         i += 1
      }
      sum
   }

   @Benchmark
   def sumOfSquarses_baseline () : Int = {
      var i=0
      var sum=0
      while (i < v.length) {
         sum += v(i) * v(i)
         i += 1
      }
      sum
   }

   @Benchmark
   def sumOfSquaresEven_baseline () : Int = {
      var i=0
      var sum=0
      while (i < v.length) {
         if (v(i) % 2 == 0)
         sum += v(i) * v(i)
         i += 1
      }
      sum
   }

   @Benchmark
   def cart_baseline () : Int = {
      var d, dp=0
      var sum=0
      while (d < vHi.length) {
         dp = 0
         while (dp < vLo.length) {
            sum += vHi(d) * vLo(dp)
            dp +=1
         }
         d += 1
      }
      sum
   }

   @Benchmark
   def filters_baseline () : Int = {
      var i=0
      var sum=0
      while (i < v.length) {
         if (v(i) > 1 && v(i) > 2 && v(i) > 3 && v(i) > 4 && v(i) > 5 && v(i) > 6 && v(i) > 7)
         sum += v(i)
         i += 1
      }
      sum
   }

   @Benchmark
   def maps_baseline () : Int = {
      var i=0
      var sum=0
      while (i < v.length) {
         sum += v(i) * 1*2*3*4*5*6*7
         i += 1
      }
      sum
   }

   @Benchmark
   def dotProduct_baseline () : Int = {
      var counter = 0
      var sum = 0
      while (counter < vHi.length) {
         sum += vHi(counter) * vHi(counter)
         counter += 1
      }
      sum
   }

   @Benchmark
   def flatMap_after_zipWith_baseline () : Int = {
      var counter1 = 0
      var sum = 0
      while (counter1 < vFaZ.length) {
         val item1 = vFaZ(counter1) + vFaZ(counter1)
         var counter2 = 0
         while (counter2 < vFaZ.length) {
            val item2 = vFaZ(counter2)
            sum +=  item2 + item1
            counter2 += 1
         }
         counter1 += 1
      }
      sum
   }

   @Benchmark
   def zipWith_after_flatMap_baseline () : Int = {
      var sum = 0
      var index1 =  0
      var index2 =  0
      var flag1 = (index1 <= vZaF.length - 1)
      while (flag1 && (index2 <= vZaF.length - 1)) {
         var el2 = vZaF(index2)
         index2 += 1
         var index_zip = 0
         while (flag1 && (index_zip <= vZaF.length - 1)) {
            var el1 = vZaF(index_zip)
            index_zip += 1
            var elz =  vZaF(index1)
            index1 += 1
            flag1 = (index1 <= vZaF.length - 1);
            sum = sum + elz + el1 + el2
         }
      }
      sum
   }

   @Benchmark
   def flatMap_take_baseline () : Int = {
      var counter1 = 0
      var counter2 = 0
      var sum = 0
      var n = 0
      var flag = true
      val size1 = v.length
      val size2 = vLo.length
      while (counter1 < size1 && flag) {
         val item1 = v(counter1)
         while (counter2 < size2 && flag) {
           val item2 = vLo(counter2)
           sum = sum + item1 * item2
           counter2 += 1
           n += 1
           if (n == 20000000)
             flag = false
         }
         counter2 = 0
         counter1 += 1
      }
      sum
   }
}
