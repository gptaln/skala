import scala.language.postfixOps
import scala.math.BigInt.probablePrime
import scala.util.Random

//2  Этот метод находится в StringOps
"crazy" * 3

//3
10 max 2 //RichInt определяет максимальное число

//4
BigInt(2).pow(1024)

//5
//Нужно импортировать scala.util.Random
//scala.math.BigInt, scala.math.BigInt.probableInt

//6
// для работы со строками  скала опирается на класс java.lang.string
probablePrime(100, Random) toString 36

//7
"Hello"(0)
"Hello".head
"Hello".take(1)
//последний символ
"Hello".reverse(0)
"Hello".last
"Hello".takeRight(1)

//8
"Hello".take(4) //Hell выводит  символы от начала строки до нашего числа
"Hello".drop(6) //o выводит символы от нашего числа до конца строки
"Hello".takeRight(2) // lo выводит символы от конца до нашего числа с обратной стороны
"Hello".dropRight(2) //Hel выводит символы от начала до нашего числа с конца строки

//9
def signum(num: Int) {
  if (num > 0) print(1) else if (num < 0) print(-1) else print(0)
}
BigInt(10).signum

//10
val t = {} //Значение () Тип Unit

//11
//Java：
//for (int i = 10; i >= 0; i –) System.out.println(i)
//Scala：
for (i <- 10 to 0 by -1) print(i)

//12
def countdown(n: Int) = for (i <- n to 0 by -1) println(i)
countdown(5)

//13
def multWord(str: String): Long ={
  var t: Long = 1
  for (i <- str) {
    t = t * i.toLong
  }
  t
}


//14 без применения цикла
def multWord1(str: String): Long ={
  var t: Long = 1
  str.foreach(t *= _.toLong)
  t
}

//15
def product(s: String): Long = { //
  var t: Long = 1
  for (i <- s) {
    t *= i.toLong
  }
  t
}
product("Hello")

//16 рекурсия
def product(s: String): Long = {
  if (s.length == 1) return s.charAt(0).toLong
  else s.take(1).charAt(0).toLong * product(s.drop(1))
}
product("Hello")

//17
def pow(x: Double, n: Int): Double = {
  if (n == 0) 1
  else if (n < 0) 1 / (x - n)
  else if (n % 2 == 1) x * pow(x, n - 1)
  else {
    val y = pow(x, n / 2)
    y * y
  }
}
pow(2, 0)

//18
def symb(n:Int): Boolean ={
  var list1:List[Int]=List()
  var num:Int = n
  while(num>0){
  var ch:Int=num%10
  if(list1.contains(ch))return false
  else list1=list1:+ch
  num=num/10
}
return true}
def sum(m:Int,n:Int):Int = {
  var sum:Int=0
  for(i<-m to n){
    if(symb(i)) sum+=i
  }
  sum
}
sum(1,11)

//19
def flatten(ls: List[Any]): List[Any] = ls flatMap {
  case i: List[_] => flatten(i)
  case e => List(e)
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))

//21
def f21(x: List[Any], k: Int): Unit = {
  var spisok: List[Any] = List()
  for (i <- x) {
    for (j <- 1 to k) {
      spisok = spisok :+ i
    }
  }
  println(spisok)
}
f21(List(1, 3, 4), 3)

//24   наименьшее общее кратное
def divider(num1:Int, num2:Int):Int = {
  if(num1==0|num2==0) return num1+num2
  else if(num1>num2) return divider (num1%num2,num2)
  else return divider (num1,num2%num1)
}
def multiple(num1:Int,num2:Int):Int={
  num1*num2/divider(num1,num2)
}

//25
def delete(n: List[Any], del: Int): Unit = {
  var res: List[Any] = List()
  var count: Int = 0
  for (i <- n) {
    count += 1
    if (count < del) res = res :+ i
    else count = 0
  }
  println(res)
}
delete(List(1, 2, 3, 4, 5, 6, 7, 8, 9), 2)

//26
def fact(n: Int): Unit = {
  var res: Int = 1
  for (i <- 1 to n) res *= i
  res}

def placement(n: Int, k: Int): Int = {
  if (k > n) -1
  else {
    fact(n)/fact(n-k)}
}
placement(4, 3)

//27
def printList(n:List[Any]): Unit ={
  for(i<-n) print(i,", ")
  println()
}
def position(n:List[Any], moveLeft:Int): Unit ={
  val indLength = n.length - 1
  var ans: List[Any] = n
  if(moveLeft>=0){
    val left = ans.take(moveLeft)
    val right = ans.takeRight(indLength - moveLeft)
    ans = right ::: left
  }else{
    val right = ans.take(-moveLeft)
    val left = ans.takeRight(indLength + moveLeft)
    ans = right ::: left

  }
  printList(ans)
}


//28
def perfect(n: Int): Boolean = {
  var summa = 0
  var res: Boolean = false
  for (i <- 1 to n-1 reverse) {
    if (n % i == 0) summa += i
  }
  if(summa == n) res=true
  res
}
def sumPerfect(n: Int): Int = {
  for (i <- 1 to n reverse) {
    if (perfect(i)) return i
  }
  1
}
sumPerfect(512)

//29
def evenOrd(n: List[Any]): Unit ={
  var index = n.length - 1
  var list1: List[Any] = List()
  var list2: List[Any] = List()

  for(i <- 0 to index){
    if(i % 2 == 0)list1 = list1 :+ n(i)
    else list2 = list2 :+ n(i)
  }
  println(list1)
  println(list2)
}
evenOrd(List(0,2,3,1,23,35,45,65,7,57,67,67,6))

//30
def sumSymb(n:Int):Int = {
  var sum:Int = 0
  var cn: Int=n
  while(cn>0){
    sum+=cn%10
    cn=cn/10
  }
  sum
}
def isPow(n:Int):Boolean={
  var num:Int=sumSymb(n)
  var ch: Int = num
  if(ch==n)return true
  while(ch<n){
    ch*=num
    if(ch==n)return true
  }
  false
}
def greatestNum(n:Int):Int={
  for(i<-1 to n reverse){
    if(isPow(i))return i
  }
  -1
}

//31
def listRespons(n:List[Any]): Unit ={
  val len=n.length - 1
  var list1,list2:List[Any]=List()
  for (i<-0 to len) {
    list1 = list1 :+ n(i).asInstanceOf[List[Any]](0)
    list2 = list2 :+ n(i).asInstanceOf[List[Any]](1)
  }
  println(list1)
  println(list2)
}