package fpspeedrun
import simulacrum.typeclass
import syntax.semigroup._

@typeclass
trait Monoid[A] extends Semigroup[A] with Default[A]{
  def empty: A
  override def default: A = empty
}

object Monoid extends StdMonoidInstances[Monoid] {
  implicit def optionMonoid[T: Semigroup]: Monoid[Option[T]] = new Monoid[Option[T]] {
    override def empty: Option[T] = None
    override def combine(xo: Option[T], yo: Option[T]): Option[T] =
      for (x <- xo; y <- yo) yield x |+| y
  }
  object Laws {
    def associativity[T](x: T, y: T, z: T)(implicit m: Monoid[T]): Boolean = {
      (x |+| (y |+| z)) == ((x |+| y) |+| z)
    }

    def leftIdentity[T](x: T)(implicit m: Monoid[T]): Boolean = {
      import m._
      (empty |+| x) == x
    }

    def rightIdentity[T](x: T)(implicit m: Monoid[T]): Boolean = {
      import m._
      (x |+| empty) == x
    }
  }
}

trait StdMonoidInstances[TC[x] >: Monoid[x]] {
  final implicit val stringMonoid: TC[String] = new Monoid[String] {
    override def empty: String = ""
    override def combine(x: String, y: String): String = x + y
  }

  final implicit def listMonoid[A]: TC[List[A]] = new Monoid[List[A]] {
    override def empty: List[A] = List.empty
    override def combine(x: List[A], y: List[A]): List[A] = x ::: y
  }
}
