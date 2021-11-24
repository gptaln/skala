

/** Напишите вашу реализацию в тестовые функции.
 *
 * https://docs.scala-lang.org/overviews/collections/maps.html
 */
object Maps {

  case class User(name: String, age: Int)

  /* a) В данной Seq[User] сгруппируйте пользователей по имени (`groupBy`) и вычислите средний возраст: `name -> averageAge`
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testGroupUsers(users: Seq[User]): Map[String, Int] = {
    val averageAge = users.map(_.age).sum / users.length
    users.groupBy(_.name).map(x => (x._1, averageAge))
  }

  /* b) Дана `Map[String, User]` состоящая из имен пользователей `User`, сколько имен пользователей, содержащихся в Map, содержат подстроку "Adam"?
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testNumberFrodos(map: Map[String, User]): Int = {
    map.map(x=>x._2.name).filter(x=>x.contains("Adam")).size
  }

  /* c) Удалите всех пользователей возраст которых менее 35 лет.
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testUnderaged(map: Map[String, User]): Map[String, User] = {
    map.filter(x=>x._2.age > 35)
  }
  def main(args: Array[String]): Unit = {
    val users: Seq[User] = Seq(User("John", 20), User("Bob", 40), User("Alex", 60))
    val map = Map("1" -> User("Alex",20),"2" -> User("John", 20),"3" -> User("Adam", 40))
    println(testGroupUsers(users))
    println(testNumberFrodos(map))
    println(testUnderaged(map))
  }
}
