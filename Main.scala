package io.chronon.sandbox

import scalaz.{ ~>, -\/, \/-, :<:, Coproduct, Free }
import scalaz.effect.{ IO, SafeApp }
import scala.language.{ higherKinds, implicitConversions, postfixOps }

object Main extends SafeApp {

  /* Injectable lets us lift an F[A] into a Coproduct[F, G, A] when there exists
   *  an injection relationship between F and G, F :<: G.
   */
  trait Injectable[Self[_], A] { this: Self[A] => 
    def inject[F[_]] ( implicit I: Self :<: F ): Free[F, A] =
      Free liftF ( I inj ( this ) )
  }

  /* Our first algebra is Foo. Requiring `action` to be a member of Foo
   * simplifies writing natural transformations later on. This can be a side-
   * effecting operation, but then something like doobie's Capture typeclass
   * should be considered for safety.
   */
  sealed trait Foo[A] extends Injectable[Foo, A] { def action: A }

  case class ReadFoo ( key: String ) extends Foo[String] {
    def action = {
      println ( s"[ EFFECT ] Reading from Foo at key: $key" )
      "foo"
    }
  }

  case class WriteFoo ( key: String, value: String ) extends Foo[Unit] {
    def action = println(s"[ EFFECT ] Writing value: $value to Foo at key: $key")
  }

  /* Our second algebra, Bar, is just a copy of Foo. */
  sealed trait Bar[A] extends Injectable[Bar, A] { def action: A }

  case class ReadBar ( key: String ) extends Bar[String] {
    def action = {
      println ( s"[ EFFECT ] Reading from Bar at key: $key" )
      "bar"
    }
  }

  case class WriteBar ( key: String, value: String ) extends Bar[Unit] {
    def action = println(s"[ EFFECT ] Writing value: $value to Bar at key: $key")
  }

  /* To run/interpret the Foo and Bar algebras, we need natural transformations
   * into IO.
   */
  object FooIO extends ( Foo ~> IO ) {
    def apply[A] ( fa: Foo[A] ) = IO ( fa action )
  }

  object BarIO extends ( Bar ~> IO ) {
    def apply[A] ( fa: Bar[A] ) = IO ( fa action )
  }

  /* We define the type FooBar so that we have something to inject into. Note
   * that FooBar is not a functor, as neither Foo nor Bar are functors. That
   * will be taken care of by Coyoneda inside Free.
   */
  type FooBar[A] = Coproduct[Foo, Bar, A]

  /* For any two natural transformations with the same codomain, we can define
   * another natural transformation from the coproduct of their domains to their
   * shared codomain.
   */
  implicit def naturalTransProduct[F[_], G[_], H[_]] ( fg: ( F ~> H, G ~> H ) ) =
    new ( Coproduct[F, G, ?] ~> H ) {
      def apply[A] ( ca: Coproduct[F, G, A] ) = ca fold ( fg._1, fg._2 )
    }

  /* At last we can construct our program over the coproduct of Foo and Bar! */
  val program = for {
    fromFoo <- ReadFoo ( "fooKey" ).inject[FooBar]
    _       <- WriteFoo ( "fooKey", "newFoo" ).inject[FooBar]
    fromBar <- ReadBar ( "barKey" ).inject[FooBar]
    _       <- WriteBar ("barKey", "newBar" ).inject[FooBar]
  } yield ()

  /* Finally we transform it into the IO monad using our product of natural
   * transformations, and set `runc` to this value to be executed at the end
   * of the universe.
   */
  override val runc = program foldMap ( ( FooIO, BarIO ) )
}
