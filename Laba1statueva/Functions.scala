package exercise1

/** Напишите отдельные функции, решающие поставленную задачу. 
  * 
  * Синтаксис:
  *   // метод
  *   def myFunction(param0: Int, param1: String): Double = // тело
  * 
  *   // значение
  *   val myFunction: (Int, String) => Double (param0, param1) => // тело
  */
object Functions {
def Circle(r: Double): Double = r * r * 3.14
  /* a) Напишите функцию, которая рассчитывает площадь окружности
   *    r^2 * Math.PI
   */



  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testCircle(r: Double): Double = Circle(r)




  /* b) Напишите карированную функцию которая рассчитывает площадь прямоугольника a * b.
   */
  def RectangleCurried(a:Double)(b: Double): Double = a * b




  // примените вашу функцию из пукта (b) здесь, не изменяя сигнатуру
  def testRectangleCurried(a: Double, b: Double): Double = RectangleCurried(a)(b)


  // c) Напишите не карированную функцию для расчета площади прямоугольника.
  def RectangleUc(a:Double, b: Double): Double = a * b



  // примените вашу функцию из пункта (c) здесь, не изменяя сигнатуру
  def testRectangleUc(a: Double, b: Double): Double = RectangleUc(a,b)

  def main(args: Array[String]): Unit = {
    println(testCircle(6))
    println(testRectangleCurried(3,6))
    println(testRectangleUc(6, 4))
  }
}
