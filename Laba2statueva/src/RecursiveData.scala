

sealed trait List[A]
case class Cons[A](head: A, tail: List[A]) extends List[A]
case class Nil[A]() extends List[A]

/** Напишите свои решения в виде функций. */
object RecursiveData {

  // a) Реализуйте функцию, определяющую является ли пустым `List[Int]`.
def ListIntEmpty(list: List[Int]): Boolean = list == Nil()


  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testListIntEmpty(list: List[Int]): Boolean = ListIntEmpty(list)

  // b) Реализуйте функцию, которая получает head `List[Int]`или возвращает -1 в случае если он пустой.
def listIntHead(list: List[Int]): Int = list match {
  case list: Cons[Int] => list.head
  case _ => -1
}


  // используйте функцию из пункта (a) здесь, не изменяйте сигнатуру
  def testListIntHead(list: List[Int]): Int = listIntHead(list)

  // c) Можно ли изменить `List[A]` так чтобы гарантировать что он не является пустым?
  sealed trait ListNotEmpty[A]
  case class ConsNotEmpty[A](head: A, tail: ListNotEmpty[A]) extends ListNotEmpty[A]
  case class End[A](head: A) extends ListNotEmpty[A]

  /* d) Реализуйте универсальное дерево (Tree) которое хранит значения в виде листьев и состоит из:
   *      node - левое и правое дерево (Tree)
   *      leaf - переменная типа A
   */
  sealed trait Tree[A]
  case class Node[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](head: A) extends Tree[A]

  def main(args: Array[String]): Unit = {
    var list1: List[Int] = Cons(2, Cons(8, Nil()))
    var list2: List[Int] = Cons(6, Cons(2, Nil()))

    println(testListIntEmpty(list1))
    println(testListIntHead(list2))

  }
}
