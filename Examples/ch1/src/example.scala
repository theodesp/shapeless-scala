object ch1 extends App {
  import shapeless.{HList, ::, HNil}

  val product: String :: Int :: Boolean :: HNil =
    "Sunday" :: 1 :: false :: HNil

  val repr = "Hello" :: 123 :: true :: HNil

  HList(5, 4, 3)

  sealed trait Shape
  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  val rect: Shape = Rectangle(3.0, 4.0)
  val circ: Shape = Circle(1.0)

  def area(shape: Shape): Double =
    shape match {
      case Rectangle(w, h) => w * h
      case Circle(r)       => math.Pi * r * r
    }

  area(rect)
  // res1: Double = 12.0

  area(circ)
  // res2: Double = 3.141592653589793

  type Rectangle2 = (Double, Double)
  type Circle2 = Double
  type Shape2 = Either[Rectangle2, Circle2]

  val rect2: Shape2 = Left((3.0, 4.0))
  val circ2: Shape2 = Right(1.0)

  def area2(shape: Shape2): Double =
    shape match {
      case Left((w, h)) => w * h
      case Right(r)     => math.Pi * r * r
    }

  area2(rect2)
  // res4: Double = 12.0

  area2(circ2)
  // res5: Double = 3.141592653589793

  val first = product.head
  // first: String = Sunday

  val second = product.tail.head
  // second: Int = 1

  val rest = product.tail.tail
  // rest: Boolean :: shapeless.HNil = false :: HNil

  import shapeless.Generic

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)

  val iceCreamGen = Generic[IceCream]
  // iceCreamGen: shapeless.Generic[IceCream]{type Repr = String :: Int :: Boolean :: shapeless.HNil} = anon$macro$4$1@6b9323fe

  val iceCream = IceCream("Sundae", 1, false)
  // iceCream: IceCream = IceCream(Sundae,1,false)

  val repr2 = iceCreamGen.to(iceCream)
  // repr: iceCreamGen.Repr = Sundae :: 1 :: false :: HNil

  val iceCream2 = iceCreamGen.from(repr2)
  // iceCream2: IceCream = IceCream(Sundae,1,false)

  val tupleGen = Generic[(String, Int, Boolean)]

  tupleGen.to(("Hello", 123, true))
  // res4: tupleGen.Repr = Hello :: 123 :: true :: HNil

  tupleGen.from("Hello" :: 123 :: true :: HNil)
  // res5: (String, Int, Boolean) = (Hello,123,true)

  import shapeless.{Coproduct, :+:, CNil, Inl, Inr}

  case class Red()
  case class Amber()
  case class Green()

  type Light = Red :+: Amber :+: Green :+: CNil

  val red: Light = Inl(Red())
  // red: Light = Inl(Red())

  val green: Light = Inr(Inr(Inl(Green())))
  // green: Light = Inr(Inr(Inl(Green())))

  val gen = Generic[Shape]

  gen.to(Rectangle(3.0, 4.0))
  // res3: gen.Repr = Inl(Rectangle(3.0,4.0))

  gen.to(Circle(1.0))
  // res4: gen.Repr = Inr(Inl(Circle(1.0)))
}
