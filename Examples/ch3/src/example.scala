object ch3 extends App {
  ////// Turn a value of type A into a row of cells in a CSV file:
  trait CsvEncoder[A] {
    def encode(value: A): List[String]
  }
  ////
  ////// Custom data type:
  //case class Employee(name: String, number: Int, manager: Boolean)
  //
  ////// CsvEncoder instance for the custom data type:
  ////implicit val employeeEncoder: CsvEncoder[Employee] =
  ////  new CsvEncoder[Employee] {
  ////    def encode(e: Employee): List[String] =
  ////      List(
  ////        e.name,
  ////        e.number.toString,
  ////        if(e.manager) "yes" else "no"
  ////      )
  ////  }
  ////
  //def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
  //  values.map(value => enc.encode(value).mkString(",")).mkString("\n")
  //
  //val employees: List[Employee] = List(
  //  Employee("Bill", 1, true),
  //  Employee("Peter", 2, false),
  //  Employee("Milton", 3, false)
  //)
  ////
  ////writeCsv(employees)
  ////
  //case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  //
  //implicit val iceCreamEncoder: CsvEncoder[IceCream] =
  //  new CsvEncoder[IceCream] {
  //    def encode(i: IceCream): List[String] =
  //      List(
  //        i.name,
  //        i.numCherries.toString,
  //        if(i.inCone) "yes" else "no"
  //      )
  //  }
  //
  //val iceCreams: List[IceCream] = List(
  //  IceCream("Sundae", 1, false),
  //  IceCream("Cornetto", 0, true),
  //  IceCream("Banana Split", 0, false)
  //)
  //
  //writeCsv(iceCreams)
  //
  //implicit def pairEncoder[A, B](
  //                                implicit
  //                                aEncoder: CsvEncoder[A],
  //                                bEncoder: CsvEncoder[B]
  //                              ): CsvEncoder[(A, B)] =
  //  new CsvEncoder[(A, B)] {
  //    def encode(pair: (A, B)): List[String] = {
  //      val (a, b) = pair
  //      aEncoder.encode(a) ++ bEncoder.encode(b)
  //    }
  //  }
  //
  //writeCsv(employees zip iceCreams)
  //
  object CsvEncoder {
    // "Summoner" method
    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] =
      enc

    // "Constructor" method
    def instance[A](func: A => List[String]): CsvEncoder[A] =
      new CsvEncoder[A] {
        def encode(value: A): List[String] =
          func(value)
      }

    // Globally visible type class instances
  }

  ////CsvEncoder[IceCream]
  //
  //import shapeless._
  //
  ////the[CsvEncoder[IceCream]]
  //// res11: CsvEncoder[IceCream] = $anon$1@5940ac6a
  //
  //implicit val booleanEncoder2: CsvEncoder[Boolean] =
  //  CsvEncoder.instance(b => if(b) List("yes") else List("no"))
  //
  import shapeless._

  def createEncoder[A](func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      def encode(value: A): List[String] = func(value)
    }

  implicit val stringEncoder: CsvEncoder[String] =
    createEncoder(str => List(str))

  implicit val intEncoder: CsvEncoder[Int] =
    createEncoder(num => List(num.toString))

  implicit val booleanEncoder: CsvEncoder[Boolean] =
    createEncoder(bool => List(if(bool) "yes" else "no"))

  implicit val hnilEncoder: CsvEncoder[HNil] =
    createEncoder(hnil => Nil)

  implicit def hlistEncoder[H, T <: HList](
                                            implicit
                                            hEncoder: CsvEncoder[H],
                                            tEncoder: CsvEncoder[T]
                                          ): CsvEncoder[H :: T] =
    createEncoder {
      case h :: t =>
        hEncoder.encode(h) ++ tEncoder.encode(t)
    }
  //
  //val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly
  //
  //reprEncoder.encode("abc" :: 123 :: true :: HNil)
  //// res9: List[String] = List(abc, 123, yes)
  //
  //import shapeless.Generic
  //
  //implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
  //  val gen = Generic[IceCream]
  //  val enc = CsvEncoder[gen.Repr]
  //  createEncoder(iceCream => enc.encode(gen.to(iceCream)))
  //}
  //
  //implicit def genericEncoder[A, R](
  //                                   implicit
  //                                   gen: Generic.Aux[A, R],
  //                                   enc: CsvEncoder[R]
  //                                 ): CsvEncoder[A] =
  //  createEncoder(a => enc.encode(gen.to(a)))
  //
  //sealed trait Shape
  //final case class Rectangle(width: Double, height: Double) extends Shape
  //final case class Circle(radius: Double) extends Shape
  //
  //import shapeless.{Coproduct, :+:, CNil, Inl, Inr}
  //
  //implicit val cnilEncoder: CsvEncoder[CNil] =
  //  createEncoder(cnil => throw new Exception("Inconceivable!"))
  //
  //implicit def coproductEncoder[H, T <: Coproduct](
  //                                                  implicit
  //                                                  hEncoder: CsvEncoder[H],
  //                                                  tEncoder: CsvEncoder[T]
  //                                                ): CsvEncoder[H :+: T] = createEncoder {
  //  case Inl(h) => hEncoder.encode(h)
  //  case Inr(t) => tEncoder.encode(t)
  //}
  //
  //implicit val doubleEncoder: CsvEncoder[Double] =
  //  createEncoder(d => List(d.toString))
  //
  //writeCsv(shapes)
  //// res7: String =
  //// 3.0,4.0
  //// 1.0

  sealed trait Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A) extends Tree[A]

  implicit def hlistEncoder[H, T <: HList](
                                            implicit
                                            hEncoder: Lazy[CsvEncoder[H]], // wrap in Lazy
                                            tEncoder: CsvEncoder[T]
                                          ): CsvEncoder[H :: T] = createEncoder {
    case h :: t =>
      hEncoder.value.encode(h) ++ tEncoder.encode(t)
  }

  implicit def coproductEncoder[H, T <: Coproduct](
                                                    implicit
                                                    hEncoder: Lazy[CsvEncoder[H]], // wrap in Lazy
                                                    tEncoder: CsvEncoder[T]
                                                  ): CsvEncoder[H :+: T] = createEncoder {
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

  implicit def genericEncoder[A, R](
                                     implicit
                                     gen: Generic.Aux[A, R],
                                     rEncoder: Lazy[CsvEncoder[R]] // wrap in Lazy
                                   ): CsvEncoder[A] = createEncoder { value =>
    rEncoder.value.encode(gen.to(value))
  }

  //CsvEncoder[Tree[Int]]
  //CsvEncoder[Float]

  import scala.reflect.runtime.universe._

  println(reify(CsvEncoder[Int]))
  val five = reify{ 5 }
  println(five)
  println(reify{ 5.toString })
  println(reify{ five.splice.toString })
}