package fpspeedrun

sealed trait Opt[A] {
  def getOrElse(default: () => A): A
}

case class None[A]() extends Opt[A] {
  override def getOrElse(default: () => A): A = default()
}

case class Some[A](a: A) extends Opt[A] {
  override def getOrElse(default: () => A): A = a
}

object Opt {
  implicit def OptDefault[T]: Default[Opt[T]] = {
    new Default[Opt[T]] {
      override def default: Opt[T] = None()
    }
  }
}