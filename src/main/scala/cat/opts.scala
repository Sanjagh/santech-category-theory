package cat

object opts {

  implicit class FunctorOpts[T, C[T]](c: C[T]) {

    def map[U](f: T => U)(implicit functor: Functor[C]): C[U] = {
      functor.map(c)(f)
    }
  }

}
