package exercise1

/* This task has no tests. It is an exercise for you to write different class structures.
 * 
 -a) Создать класс Animal, который имеет следующие поля:
 *      - name: String (название)
 *      - species: String (вид)
 *      - food: String
 * 
 *    Синтаксис: class MyClass(val publicField: Int, privateField: String) {
 *              // остальные поля и методы
 *            }
 *
 * -b) Создайте объект-компаньон для класса Animal и добавьте следующие сущности как поля:
 *      - cat, mammal, meat
 *      - parrot, bird, vegetables
 *      - goldfish, fish, plants
 * 
 *    Синтаксис: object MyClass {
 *              // статические поля и методы
 *            }
 * 
 * -c) Добавьте следующие метод в Animals:
 *      def eats(food: String): Boolean
 *    
 *     который проверяет ест ли животное определенную пищу
 * 
 * -d) Переопределите ваш класс Animal как трейт и создайте объекты класса-образца для Mammals, Birds и Fishs.
 *    Вам все еще нужно поле `species`?
 * 
 * -e) Добавьте следующие функции в объект-компаньон Animal:
 *      def knownAnimal(name: String): Boolean  // true если это имя одного из трех животных из (b)
 *      def apply(name: String): Option[Animal] // возвращает одно из трех животных в соответствии с именем (Some) или ничего (None), см. ниже
 * 
 * f) Создайте трейт Food со следующими классами-образцами:
 *      - Meat
 *      - Vegetables
 *      - Plants
 *   и добавьте это в определение Animal. Так же добавьте объект-компаньон с методом apply():
 *      def apply(food: String): Option[Food]
 */
object Classes {
  class Animals(name: String, species: String, food: String) {
    def eats(food: String): Boolean = this.food == food
  }

  object Animals {
    val cat = new Animals("cat", "mammal", "meat")
    val parrot = new Animals("parrot", "bird", "vegetables")
    val goldfish = new Animals("goldfish", "fish", "plants")
  }

  trait Animal {
    val name: String
    val food: Food

    def eats(food: Food): Boolean = this.food == food
  }


  trait Food

  case class Meat() extends Food

  case class Vegetables() extends Food

  case class Plants() extends Food

  case class Mammals(name: String, food: Food) extends Animal

  case class Birds(name: String, food: Food) extends Animal

  case class Fish(name: String, food: Food) extends Animal

  object Animal {
    def knownAnimal(name: String): Boolean = name match {
      case "cat" | "parrot" | "goldfish" => true
      case _ => false
    }

    def apply(name: String): Option[Animal] = name match {
      case "cat" => Some(Mammals("cat", Meat()))
      case "parrot" => Some(Birds("parrot", Vegetables()))
      case "goldfish" => Some(Fish("goldfish", Plants()))
      case _ => None()
    }
  }

  object Food {
    def apply(food: String): Option[Food] = food match {
      case "meat" => Some(Meat())
      case "vegetables" => Some(Vegetables())
      case "plants" => Some(Plants())
      case _ => None()
    }
  }

  sealed trait Option[A] {

    def isEmpty: Boolean
  }

  case class Some[A](a: A) extends Option[A] {
    val isEmpty = false
  }

  case class None[A]() extends Option[A] {
    val isEmpty = true
  }

  def main(args: Array[String]): Unit = {
    println(Animal.knownAnimal("cat"))
    println(Animal.knownAnimal("elefant"))
    println(Animal.apply("cat"))

  }
}

