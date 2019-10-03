package chapter7

import java.time.{Duration, Instant}
import java.util.concurrent._

import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)


  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // I'm unsure if this works as intended, or how to test it
  private class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {

    override def cancel(evenIfRunning: Boolean): Boolean =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    override def isCancelled: Boolean = a.isCancelled || b.isCancelled
    override def isDone: Boolean = a.isDone && b.isDone
    override def get(): C = f(a.get(), b.get())
    override def get(timeout: Long, units: TimeUnit): C = {
      val start = Instant.now()
      val va = a.get(timeout, units)
      val remaining = Duration.between(start, Instant.now())
      val vb = b.get(remaining.toNanos, TimeUnit.NANOSECONDS)
      f(va, vb)
    }
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => new Map2Future[A, B, C](a(es), b(es), f)

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))((pa, pas) => map2(pa, pas)(_ :: _))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
    fork(sequence(ps.map(asyncF(f)))) // the top fork ensures that traversing the list does not block

  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] =
    fork(sequence(ps.map(asyncF(a => Some(a).filter(f))))).map(_.flatten)

  implicit class ParOps[A](p: Par[A]) {
    def map[B](f: A => B): Par[B] = Par.map(p)(f)
  }

}
