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
  def +(element: A): Multiset[A] =
    val updatedCount = this.data.getOrElse(element, 0) + 1
    Multiset(this.data + (element -> updatedCount))
  end +

  @targetName("concat")
  def ++(multisetB: Multiset[A]): Multiset[A] =
    Multiset(
      (this.data.keySet ++ multisetB.data.keySet).map( element =>
        val countA = this.data.getOrElse(element, 0)
        val countB = multisetB.data.getOrElse(element, 0)
        element -> (countA + countB)
      ).toMap
    )
  end ++

  @targetName("sub")
  def -(element: A): Multiset[A] =
    this.data.get(element) match
      case Some(count) if count > 1 =>
        val updatedCount = count - 1
        Multiset(this.data + (element -> updatedCount))
      case _ =>
        Multiset(this.data - element)
  end -

  @targetName("exclude")
  def --(multisetB: Multiset[A]): Multiset[A] =
    val updatedDataA = this.data.map{ case (elementA, countA) =>
      val updatedCountA = countA - multisetB.data.getOrElse(elementA, 0)
      (elementA, updatedCountA)
    }.filter{ case (_, updatedCountA) =>
      updatedCountA > 0
    }
    Multiset(updatedDataA)
  end --

  def included(multisetB: Multiset[A]): Boolean =
    this.data.forall{ case (elementA, countA) =>
      multisetB.data.getOrElse(elementA, 0) >= countA
    }
  end included
end Multiset

object Multiset:
  def apply[A](): Multiset[A] =
    new Multiset[A](Map.empty)
  end apply
end Multiset