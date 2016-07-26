package io.chronon.sandbox

import scalaz.{ ~>, -\/, \/-, :<:, Coproduct, Free }
import scalaz.effect.{ IO, SafeApp }
import scala.language.{ higherKinds, implicitConversions, postfixOps }

object Main extends SafeApp {

  trait Injectable[Self[_], A] { this: Self[A] => 
    def inject[F[_]] ( implicit I: Self :<: F ): Free[F, A] =
      Free liftF ( I inj ( this ) )
  }

  sealed trait Foo[A] extends Injectable[Foo, A] { def action: A }

  case class ReadFoo ( key: String ) extends Foo[String] {
    def action = { println ( s"[ EFFECT ] Reading from Foo at key: $key" ); "Foo" }
  }

  case class WriteFoo ( key: String, value: String ) extends Foo[Unit] {
    def action = println(s"[ EFFECT ] Writing value: $value to Foo at key: $key")
  }

  sealed trait Bar[A] extends Injectable[Bar, A] { def action: A }

  case class ReadBar ( key: String ) extends Bar[String] {
    def action = { println ( s"[ EFFECT ] Reading from Bar at key: $key" ); "Bar" }
  }

  case class WriteBar ( key: String, value: String ) extends Bar[Unit] {
    def action = println(s"[ EFFECT ] Writing value: $value to Bar at key: $key")
  }

  object FooIO extends ( Foo ~> IO ) { def apply[A] ( fa: Foo[A] ) = IO ( fa action ) }
  object BarIO extends ( Bar ~> IO ) { def apply[A] ( fa: Bar[A] ) = IO ( fa action ) }

  type FooBar[A] = Coproduct[Foo, Bar, A]

  implicit def naturalTransProduct[F[_], G[_], H[_]] ( fg: ( F ~> H, G ~> H ) ) =
    new ( Coproduct[F, G, ?] ~> H ) {
      def apply[A] ( ca: Coproduct[F, G, A] ) = ca fold ( fg._1, fg._2 )
    }

  val program = for {
    fromFoo <- ReadFoo ( "fooKey" ).inject[FooBar]
    _       <- WriteFoo ( "fooKey", "newFoo" ).inject[FooBar]
    fromBar <- ReadBar ( "barKey" ).inject[FooBar]
    _       <- WriteBar ("barKey", "newBar" ).inject[FooBar]
  } yield ()

  override val runc = program foldMap ( ( FooIO, BarIO ) )
}
