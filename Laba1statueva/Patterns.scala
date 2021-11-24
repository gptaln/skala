package exercise1

/** Напишите решение в виде функции. 
  * 
  * Синтаксис:
  *   val a: Int = ???
  * 
  *   a match {
  *     case 0 => true
  *     case _ => false
  *   }
  */
object PatternMatching {

  sealed trait Hand
  case object Rock    extends Hand
  case object Paper   extends Hand
  case object Scissor extends Hand

  sealed trait Result
  case object Win  extends Result
  case object Lose extends Result
  case object Draw extends Result

  sealed trait Food
  case object Meat       extends Food
  case object Vegetables extends Food
  case object Plants     extends Food

  sealed trait Animal {

    val name: String
    val food: Food
  }
  case class Mammal(name: String, food: Food, weight: Int) extends Animal
  case class Fish(name: String, food: Food)                extends Animal
  case class Bird(name: String, food: Food)                extends Animal



  /* a) Напишите функцию, которая ставит в соответствие числу строку слудеющим образом:
   * Если:
   *     1 => "it is one"
   *     2 => "it is two"
   *     3 => "it is three"
   *     иначе => "what's that"
   */
   def a(a: Int): String = a match {
     case 1 => "it is one"
     case 2 => "it is two"
     case 3 => "it is three"
     case _ => "what's that"
   }

  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testIntToString(value: Int): String = a(value)

  /* b) Напишите функцию которая возвращает true если переменная `value` принимает значение:
   *     "max" или "Max
   *     "moritz" или "Moritz"
   */
  def b(b: String): Boolean = b match {
    case "max" => true
    case "Max" => true
    case "moritz" => true
    case "Moritz" => true
    case _ => false
  }



  // примените функции из пункта (b) здесь, не изменяя сигнатуру
  def testIsMaxAndMoritz(value: String): Boolean = b(value)

  // c) Напишите функцию проверки является ли `value` четным
  def c(c: Int): Boolean = c match {
    case c : Int if c % 2 == 0 => true
    case c : Int if c % 2 != 0 => false
  }



  // примените функции из пункта (c) здесь, не изменяя сигнатуру
  def testIsEven(value: Int): Boolean = c(value)


  
  /* d) Напишите функцию, моделирующую игру в Камень ножницы бумага 
   *     1. камень побеждает ножницы
   *     2. ножницы побеждают бумагу
   *     3. бумага побеждает камень
   *    Выиграет ли игрок `a`?
   */
  def d(a: Hand, b: Hand): Result = (a,b) match {
    case (Paper,Rock) | (Rock,Scissor)|(Scissor,Paper) => Win
    case (Rock,Paper) | (Scissor, Rock)|(Paper, Scissor) => Lose
    case (Rock,Rock) | (Scissor,Scissor)|(Paper,Paper) => Draw
  }


  // примените вашу функцию из пункта (d) здесь, не изменяя сигнатуру
  def testWinsA(a: Hand, b: Hand): Result = d(a,b)



  // Примечание: используйте определение Animals

  // e) Верните вес (weight: Int) объекта Mammal, иначе верните -1.
def e(e:Animal): Int = e match{
  case mammal: Mammal => mammal.weight
  case _ => -1
}


  // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
  def testExtractMammalWeight(animal: Animal): Int = e(animal)


  // f) Измените поле еда объектов классов Fishes и Birds на Plants, класс Mammels оставьте неизмененным.
def f(f:Animal): Animal = f match{
  case fish: Fish => Fish(fish.name, Plants)
  case bird: Bird => Bird(bird.name, Plants)
  case _ => f
}


  // примените функцию из пункта (f) здесь, не изменяйте сигнатуру
  def testUpdateFood(animal: Animal): Animal = f(animal)


  def main(args: Array[String]): Unit ={
    println(testIntToString(  10))
    println(testIntToString(  1))
    println(testIsMaxAndMoritz("max"))
    println(testIsMaxAndMoritz("miu"))
    println(testIsEven(2))
    println(testIsEven(17))
    println(testWinsA(Paper,Rock))
    println(testWinsA(Paper,Scissor))
    val cat = Mammal("cat", Meat, 5)
    val parrot = Bird("parrot", Vegetables)
    val karas = Fish("karas", Vegetables)
    println(testExtractMammalWeight(cat))
    println(testUpdateFood(parrot).food)

  }

}
