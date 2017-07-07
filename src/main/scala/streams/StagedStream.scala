package streams

import scala.reflect.Manifest
import scala.Array
import scala.collection.mutable.ArrayBuffer

import scala.lms.common._

trait StagedStream extends BooleanOps
with OrderingOps with PrimitiveOps with ArrayOps with StringOps with NumericOps with TupleOps with StructOps
with Equal with Variables with While with IfThenElse with Functions
with LiftVariables with LiftBoolean with LiftPrimitives with LiftString {

   trait Cardinality
   case object AtMost1 extends Cardinality
   case object Many extends Cardinality

   trait Producer[A] extends Serializable { self =>
     type St

     val card: Cardinality
     def init(k: St => Rep[Unit]): Rep[Unit]
     def step(st: St, k: (A => Rep[Unit])): Rep[Unit]
     def hasNext(st: St): Rep[Boolean]

   }
   type Id[A] = A
   trait StagedStream[A]
   case class Linear[A](producer: Producer[A]) extends StagedStream[A]
   case class Nested[A, B](producer: Producer[A], nestedf: A => StagedStream[B]) extends StagedStream[B]

   case class Stream[A : Manifest](stream: StagedStream[Rep[A]]) {

      def fold[W : Manifest](z: Rep[W], f: (Rep[W] => Rep[A] => Rep[W])): Rep[W] = {
         var acc: Var[W] = z
         fold_raw(acc, f)
         acc
      }

      def fold_raw[W : Manifest](z: Var[W], f: (Rep[W] => Rep[A] => Rep[W])): Rep[Unit] = {

         def consume[A](consumer: A => Rep[Unit], stream: StagedStream[A]): Rep[Unit] = {
            stream match {
              case Linear(producer) => {
                  producer.card match {
                    case Many =>
                      producer.init(sp => {
                         while(producer.hasNext(sp)) {
                            producer.step(sp, consumer)
                         }
                      })
                    case AtMost1 =>
                      producer.init(sp => {
                        if (producer.hasNext(sp)) { producer.step(sp, consumer) }
                      })
                  }
               }
               case Nested(producer, nestedf) => {
                 consume(((a: Id[_]) => consume[A](consumer, nestedf(a))), Linear(producer))
               }
            }
         }

         consume((a: Rep[A]) => { z = f (z)(a) }, stream)
      }

      def mapRaw[A, B](f: (A => (B => Rep[Unit]) => Rep[Unit]), stream: StagedStream[A]): StagedStream[B] = {
        stream match {
            case Linear(producer) => {
              val prod = new Producer[B] {

                type St = producer.St

                val card = producer.card
                def init(k: St => Rep[Unit]): Rep[Unit] = {
                  producer.init(k)
                }
                def step(st: St, k: (B => Rep[Unit])): Rep[Unit] = {
                  producer.step(st, el => f(el)(k))
                }
                def hasNext(st: St): Rep[Boolean] = {
                  producer.hasNext(st)
                }
              }
              Linear(prod)
            }
          case Nested(producer, nestedf) => {
            Nested(producer, (a: Id[_]) => mapRaw(f, nestedf(a)))
          }
        }
      }


      def map[B : Manifest](f: (Rep[A] => Rep[B])): Stream[B] =
        Stream(mapRaw[Rep[A], Rep[B]]((a => k => k(f(a))), stream))

      def flatmapRaw[A, B](f: (A => StagedStream[B]), stream: StagedStream[A]): StagedStream[B] = {
        stream match {
            case Linear(producer) => Nested(producer, f)
            case Nested(producer, nestedf) =>
              Nested(producer, (a: Id[_]) => flatmapRaw(f, nestedf(a)))
        }
      }

      def flatmap[B : Manifest](f: (Rep[A] => Stream[B])): Stream[B] =
        Stream(flatmapRaw[Rep[A], Rep[B]]((a => { val Stream (nested) = f(a); nested }), stream))

      def filter(f: (Rep[A] => Rep[Boolean])): Stream[A] = {

        val filterStream = (a: Rep[A]) =>
          new Producer[Rep[A]] {
            type St = Rep[A]

            val card = AtMost1
            def init(k: St => Rep[Unit]): Rep[Unit] =
              k(a)
            def step(st: St, k: (Rep[A] => Rep[Unit])): Rep[Unit] =
              k(st)
            def hasNext(st: St): Rep[Boolean] =
              f(st)
          }
        Stream(stream).flatmap(x => Stream(Linear(filterStream(x))))
      }


      def moreTermination[A](f: Rep[Boolean] => Rep[Boolean], stream: StagedStream[A]): StagedStream[A] = {
        def addToProducer[A](f: Rep[Boolean] => Rep[Boolean], producer: Producer[A]): Producer[A] = {
          producer.card match {
              case Many =>
                new Producer[A] {
                  type St = producer.St

                  val card = producer.card
                  def init(k: St => Rep[Unit]): Rep[Unit] =
                    producer.init(k)
                  def step(st: St, k: (A => Rep[Unit])): Rep[Unit] =
                    producer.step(st, el => k(el))
                  def hasNext(st: St): Rep[Boolean] =
                    f(producer.hasNext(st))
                }
              case AtMost1 => producer
          }
        }
        stream match {
          case Linear(producer) => Linear(addToProducer(f, producer))
          case Nested(producer, nestedf) =>
            Nested(addToProducer(f, producer), (a: Id[_]) => moreTermination(f, nestedf(a)))
        }
      }

      def addCounter[A](n: Rep[Int], producer: Producer[A]): Producer[(Var[Int], A)] =
        new Producer[(Var[Int], A)] {
          type St = (Var[Int], producer.St)

          val card = producer.card
          def init(k: St => Rep[Unit]): Rep[Unit] = {
            producer.init(st => {
              var counter: Var[Int] = n
              k(counter, st)
            })
          }
          def step(st: St, k: (((Var[Int], A)) => Rep[Unit])): Rep[Unit] = {
              val (counter, nst) = st
              producer.step(nst, el => {
                k((counter, el))
              })
          }
          def hasNext(st: St): Rep[Boolean] = {
              val (counter, nst) = st
              producer.card match {
                case Many => counter > 0 && producer.hasNext(nst)
                case AtMost1 => producer.hasNext(nst)
              }
          }
        }

      def takeRaw[A](n: Rep[Int], stream: StagedStream[A]): StagedStream[A] = {
        stream match {
          case Linear(producer) => {
            mapRaw[(Var[Int], A), A]((t => k => {
              t._1 = t._1 - 1
              k(t._2)
            }), Linear(addCounter(n, producer)))
          }
          case Nested(producer, nestedf) => {
            Nested(addCounter(n, producer), (t: (Var[Int], Id[_])) => {
              mapRaw[A, A]((el => k => {
                t._1 = t._1 - 1
                k(el)
              }), moreTermination(b => t._1 > 0 && b, nestedf(t._2)))
            })
          }
        }
      }

      def take(n: Rep[Int]): Stream[A] = Stream(takeRaw(n, stream))

      def zipProducer[A, B](producer1: Producer[A], producer2: Producer[B]): Producer[(A, B)] = {
        new Producer[(A, B)] {
            type St = (producer1.St, producer2.St)

            val card = Many
            def init(k: St => Rep[Unit]): Rep[Unit] = {
              producer1.init(s1 => producer2.init(s2 => k((s1, s2))))
            }
            def step(st: St, k: (((A, B)) => Rep[Unit])): Rep[Unit] = {
              val (s1, s2) = st
              producer1.step(s1, el1 => producer2.step(s2, el2 => k((el1, el2))))
            }
            def hasNext(st: St): Rep[Boolean] = {
              val (s1, s2) = st
              producer1.hasNext(s1) && producer2.hasNext(s2)
            }
        }
      }

      def pushLinear[A, B, C](producer: Producer[A], nestedProducer: Producer[B], nestedf: (B => StagedStream[C])): StagedStream[(A, C)] = {
        val newProducer = new Producer[(Var[Boolean], producer.St, B)] {
            type St = (Var[Boolean], producer.St, nestedProducer.St)

            val card = Many
            def init(k: St => Rep[Unit]): Rep[Unit] = {
              producer.init(s1 => nestedProducer.init(s2 => {
                var flag: Var[Boolean] = producer.hasNext(s1)
                k((flag, s1, s2))
              }))
            }
            def step(st: St, k: (((Var[Boolean], producer.St, B)) => Rep[Unit])): Rep[Unit] = {
              val (flag, s1, s2) = st
              nestedProducer.step(s2, b => k((flag, s1, b)))
            }
            def hasNext(st: St): Rep[Boolean] = {
              val (flag, s1, s2) = st
              flag && nestedProducer.hasNext(s2)
            }
        }
        Nested(newProducer, (t: (Var[Boolean], producer.St, B)) => {
          val (flag, s1, b) = t
          mapRaw[C, (A, C)]((c => k => {
            producer.step(s1, a => k((a, c)))
            flag = producer.hasNext(s1)
          }), moreTermination((b => flag && b), nestedf(b)))
        })
      }

      def makeLinear[A : Manifest](stream: StagedStream[Rep[A]]): Producer[Rep[A]] = {
        stream match {
          case Linear(producer) => producer
          case Nested(producer, nestedf) => {
            def makeAdvanced[A](advf: Var[Unit => Unit], k: (A => Rep[Unit]), stream: StagedStream[A]): Rep[Unit] = {
              stream match {
                case Linear(producer) => {
                  producer.init(st => {
                    val oldAdvf: Rep[Unit => Unit] = advf
                    def f: Rep[Unit => Unit] = fun { (_ : Rep[Unit]) =>
                      if(producer.hasNext(st))
                        producer.step(st, k)
                      else {
                        advf = oldAdvf
                        oldAdvf(())
                      }
                    }
                    advf = f
                    f(())
                  })
                }
                case Nested(producer, nestedf) =>
                  makeAdvanced(advf, ((el: Id[_]) => makeAdvanced(advf, k, nestedf(el))), Linear(producer))
              }
            }
            new Producer[Rep[A]] {
              type St = (Var[Boolean], Var[A], Var[Unit => Unit])

              val card = Many
              def init(k: St => Rep[Unit]): Rep[Unit] = {
                producer.init(st => {
                  var advf: Var[Unit => Unit] = fun { (_ : Rep[Unit]) => () }
                  var flag: Var[Boolean] = unit(true)
                  var current: Var[A] = unit(new Array[A](1)(0)) // default[A] hack  
                  def f: Rep[Unit => Unit] = fun { (_ : Rep[Unit]) =>
                    flag = producer.hasNext(st)
                    if(flag) {
                      producer.step(st, el => {
                        makeAdvanced(advf, ((a: Rep[A]) => current = a), nestedf(el))
                      })
                    }
                  }
                  advf = f
                  f(())
                  k((flag, current, advf))
                })
              }
              def step(st: St, k: (Rep[A] => Rep[Unit])): Rep[Unit] = {
                val (flag, current, advf) = st
                var el: Var[A] = current
                val f: Rep[Unit => Unit] = advf
                f(())
                k(el)
              }
              def hasNext(st: St): Rep[Boolean] = {
                val (flag, _, _) = st
                flag
              }

            }
          }
        }
      }

      def zipRaw[A : Manifest, B : Manifest](stream1: StagedStream[Rep[A]], stream2: StagedStream[Rep[B]]): StagedStream[(Rep[A], Rep[B])] = {
        (stream1, stream2) match {
          case (Linear(producer1), Linear(producer2)) =>
            Linear(zipProducer(producer1, producer2))
          case (Linear(producer1), Nested(producer2, nestf2)) =>
            pushLinear(producer1, producer2, nestf2)
          case (Nested(producer1, nestf1), Linear(producer2)) =>
            mapRaw[(Rep[B], Rep[A]), (Rep[A], Rep[B])]((t => k => k((t._2, t._1))), pushLinear(producer2, producer1, nestf1))
          case (stream1, stream2) =>
            zipRaw(Linear(makeLinear(stream1)), stream2)
        }
      }

      def zip[B : Manifest, C : Manifest](f: (Rep[A] => Rep[B] => Rep[C]), stream2: Stream[B]): Stream[C] = {
        val Stream(stream2Raw) = stream2
        Stream(mapRaw[(Rep[A], Rep[B]), Rep[C]]((t => k => k(f(t._1)(t._2))), zipRaw(stream, stream2Raw)))
      }

   }

   object Stream {

      def apply[A : Manifest](arr: Rep[Array[A]]): Stream[A] = {
        val prod = new Producer[Rep[A]] {
            type St = (Var[Int], Var[Int], Rep[Array[A]])

            val card = Many
            def init(k: St => Rep[Unit]): Rep[Unit] = {
              var n = arr.length
              var i = unit(0)
              k((i, n, arr))
            }
            def step(st: St, k: (Rep[A] => Rep[Unit])): Rep[Unit] = {
              val (i, _, arr) = st
                val el = arr(i)
                k(el)
                i = i + 1
            }
            def hasNext(st: St): Rep[Boolean] = {
              val (i, n, _) = st
              i < n
            }
        }
        Stream(Linear(prod))
      }
   }
}
