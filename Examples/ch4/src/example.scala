object ch4 extends App {
  import shapeless.Generic

  def getRepr[A](value: A)(implicit gen: Generic[A]) =
    gen.to(value)

  case class Vec(x: Int, y: Int)
  case class Rect(origin: Vec, size: Vec)

  getRepr(Vec(1, 2))
  // res1: Int :: Int :: shapeless.HNil = 1 :: 2 :: HNil

  getRepr(Rect(Vec(0, 0), Vec(5, 5)))
  // res2: Vec :: Vec :: shapeless.HNil = Vec(0,0) :: Vec(5,5) :: HNil

  import shapeless.{HList, ::, HNil}

  import shapeless.ops.hlist.Last

  val last1 = Last[String :: Int :: HNil]
  // last1: shapeless.ops.hlist.Last[String :: Int :: shapeless.HNil]{type Out = Int} = shapeless.ops.hlist$Last$$anon$34@12389dd9

  val last2 = Last[Int :: String :: HNil]
  // last2: shapeless.ops.hlist.Last[Int :: String :: shapeless.HNil]{type Out = String} = shapeless.ops.hlist$Last$$anon$34@6cb2b0cb

  last1("foo" :: 123 :: HNil)
  // res1: last1.Out = 123

  last2(321 :: "bar" :: HNil)
  // res2: last2.Out = bar

  //last1(321 :: "bar" :: HNil)
  // <console>:16: error: type mismatch;
  //  found   : Int :: String :: shapeless.HNil
  //  required: String :: Int :: shapeless.HNil
  //        last1(321 :: "bar" :: HNil)
  //

  trait Second[L <: HList] {
    type Out
    def apply(value: L): Out
  }

  object Second {
    type Aux[L <: HList, O] = Second[L] { type Out = O }

    def apply[L <: HList](implicit inst: Second[L]): Aux[L, inst.Out] =
      inst
  }

  implicit def hlistSecond[A, B, Rest <: HList]: Second.Aux[A :: B :: Rest, B] =
    new Second[A :: B :: Rest] {
      type Out = B
      def apply(value: A :: B :: Rest): B =
        value.tail.head
    }

  val second1 = Second[String :: Boolean :: Int :: HNil]
  // second1: Second[String :: Boolean :: Int :: shapeless.HNil]{type Out = Boolean} = $anon$1@668168cd

  val second2 = Second[String :: Int :: Boolean :: HNil]
  // second2: Second[String :: Int :: Boolean :: shapeless.HNil]{type Out = Int} = $anon$1@2ddf467d

  second1("foo" :: true :: 123 :: HNil)
  // res11: second1.Out = true

  second2("bar" :: 321 :: false :: HNil)
  // res12: second2.Out = 321

  def lastField[A, Repr <: HList](input: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    last: Last[Repr]
  ): last.Out = last.apply(gen.to(input))

  lastField(Rect(Vec(1, 2), Vec(3, 4)))
  // res14: Vec = Vec(3,4)

  import shapeless.ops.hlist.IsHCons

  def getWrappedValue[A, Repr <: HList, Head](in: A)(
    implicit
    gen: Generic.Aux[A, Repr],
    isHCons: IsHCons.Aux[Repr, Head, HNil]
  ): Head = gen.to(in).head
  // This fixes the bug. Both the method definition and the call site now compile as expected:

  case class Wrapper(value: Int)
  getWrappedValue(Wrapper(42))
  // res17: Int = 42
}