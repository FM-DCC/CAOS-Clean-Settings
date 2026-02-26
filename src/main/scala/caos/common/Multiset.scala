package caos.common

import scala.annotation.targetName

case class Multiset[A](data: Map[A, Int] = Map.empty):
  override def toString: String =
    this.data.flatMap{ case (element, count) =>
      List.fill(count)(element.toString)
    }.mkString(", ")
  end toString

  def isEmpty: Boolean =
    this.data.isEmpty
  end isEmpty

  def contains(element: A): Boolean =
    this.data.contains(element)
  end contains

  @targetName("add")
  def +(act:A): Multiset[A] =
    Multiset(data + (act -> (data.getOrElse(act,0)+1)))

  @targetName("union")
  def ++(other: Multiset[A]): Multiset[A] =
    Multiset(
      data.filter( (pair:(A,Int)) => !other.contains(pair._1) )
        ++
        (for (a,nr)<-other.data yield data.get(a) match {
    case Some(nr2) => a -> (nr+nr2)
    case None => a->nr
  }))

  @targetName("exclude")
  def --(other: Multiset[A]): Multiset[A] =
    Multiset((for at <- data if !other.data.contains(at._1)
      yield at) ++ // all t1 that is not in t2
      (for at <- data if other.data.contains(at._1) && other.data(at._1)<at._2
        yield at._1->(at._2-other.data(at._1)))) // all `this` that is partially dropped by `other`

  @targetName("delete")
  def -(act:A): Multiset[A] =
    data.get(act) match
      case Some(v) if v>1 =>
        Multiset(data + (act -> (v-1)))
      case _ =>
        Multiset(data - act)

  def included(other: Multiset[A]): Boolean =
    data.forall(a1 => other.data.get(a1._1).exists(_>=a1._2))


object Multiset:
  //    def apply[A](m:Map[A,Int]) = new Multiset[A]:
  //      data = m
  def apply[A]() = new Multiset[A](Map())
