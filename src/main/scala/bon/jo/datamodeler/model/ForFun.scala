package bon.jo.datamodeler.model

import bon.jo.datamodeler.model.ForFun.Fact

trait ForFun[C[_]] {

  def apply[B]() : C[B]
  def apply[A](a : A) : C[A]
  def combine[A](a : C[A],b : C[A]) : C[A]
  def map[A,B](f : A => B ,o : C[A]) : C[B]
  def flatten[A,B](o : C[A])(using asIte : A => C[B]): C[B]
  def flatMap[A, B](f : A => C[B],o : C[A]) : C[B] =
    flatten(map[A,C[B]](f,o))
}

object ForFun:
  inline def forfun[C[_]](using ForFun[C]) = summon
  extension[A,C[_]](ca : C[A]) (using ForFun[C])
    def combine(b : C[A]) : C[A] = forfun.combine(ca,b)
    def map[B](f : A => B ) : C[B] = forfun.map(f,ca)
    def flatMap[B](f : A => C[B]): C[B] = forfun.flatMap(f,ca)
    def flatten[B](using asIte : A => C[B]): C[B] = forfun.flatten(ca)



  trait Fact[C[_]]:
    def apply[A](a : C[A] ) : Iterable[A]
    def apply[A](a : Iterable[A]) : C[A]

  given[C[_]](using Fact[C] ) : ForFun[C] =
  new ForFun[C]:
    def toIterable[A](c : C[A]): Iterable[A] = summon[Fact[C]](c)
    def fromIterable[A](c : Iterable[A]): C[A] =  summon[Fact[C]](c)
    def apply[B]() : C[B] = fromIterable(List())
    def apply[B](a : B): C[B] =  fromIterable(List(a))
    def combine[A](a : C[A],b : C[A]) : C[A] = fromIterable(toIterable(a) ++ toIterable(b))
    def map[A,B](f : A => B ,o : C[A]): C[B] = fromIterable(toIterable(o).map(f))
    def flatten[A,B](o : C[A])(using asIte : A => C[B]): C[B] = fromIterable(toIterable(o)
      .flatMap{
      e =>
        toIterable(asIte(e))
    })







