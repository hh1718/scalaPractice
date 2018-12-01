import scala.language.higherKinds
 
//http://chopl.in/post/2012/11/06/introduction-to-typeclass-with-scala/
trait Who[T] {
  def who(x: T): String
}
implicit def WhoInt = new Who[Int] {
  def who(x: Int) = "Int"
}
implicit object WhoDouble extends Who[Double] {
  def who(x: Double) = "Double"
}
def sayWho[T](x: T)(implicit instance: Who[T]) = println(instance.who(x))
 
/*----------------------self made---------------------------*/
/*WhoFree1*/
trait WhoFree1[F[_]] {
  def who[A](x:F[A]): String
  def map[A,B](x:F[A])(f: A => B): F[B]
}
implicit def WhoList = new WhoFree1[List] {
  def who[A](x:List[A]) = "List[A]"
  def map[A,B](x:List[A])(f: A => B) = x map f
}
implicit def whoOption = new WhoFree1[Option] {
  def who[A](x:Option[A]) = x match {
    case Some(a) => "Some(A)"
    case _ => "None"
  }
  def map[A,B](x:Option[A])(f: A => B) = x map f
}
def sayWhoFree1[F[_],A](x:F[A])(implicit instance: WhoFree1[F]) = println(instance.who(x))
def mapWhoFree1[F[_],A,B](x:F[A])(f: A => B)(implicit instance: WhoFree1[F]) = println(instance.map(x)(f))
object WhoFree1{
  val WhoList = new WhoFree1[List] {
    def who[A](x: List[A]): String = "List[A]"
    def map[A,B](x: List[A])(f: A => B): List[B] = x map f
  }
}
 
/*whoFree2*/
trait WhoFree2[F[_,_]] {
  def who[A,B](x:F[A,B]): String
}
implicit def WhoMap = new WhoFree2[Map] {
  def who[A,B](x:Map[A,B]) = "Map[A,B]"
}
def sayWhoFree2[F[_,_],A,B](x:F[A,B])(implicit instance: WhoFree2[F]) = println(instance.who(x))
 
/*WhoFree*/
trait WhoFree[F[_],A]{
  def who(x:F[A]): String
  def sum(x:F[A]): A
}
implicit def WhoListInt = new WhoFree[List,Int] {
  def who(x:List[Int]) = "ListInt"
  def sum(x:List[Int]) = x.sum
}
implicit def WhoListDouble = new WhoFree[List,Double] {
  def who(x:List[Double]) = "ListDouble"
  def sum(x:List[Double]) = x.sum
}
implicit def WhoListString = new WhoFree[List,String] {
  def who(x:List[String]) = "ListString"
  def sum(x:List[String]) = x.foldLeft("")((v1,v2) => v1 + v2)
}
implicit def whoOptionInt = new WhoFree[Option,Int] {
  def who(x:Option[Int]) = "OptionInt"
  def sum(x:Option[Int]) = x.getOrElse(0)
}
implicit def whoOptionDouble = new WhoFree[Option,Double] {
  def who(x:Option[Double]) = "OptionDouble"
  def sum(x:Option[Double]) = x.getOrElse(0)
}
implicit def whoOptionStrinig = new WhoFree[Option,String] {
  def who(x:Option[String]) = "OptionString"
  def sum(x:Option[String]) = x.getOrElse("None")
}
implicit def whoOptionNull = new WhoFree[Option,Null] {
  def who(x:Option[Null]) = "None"
  def sum(x:Option[Null]) = null
}
def sayWhoFree[F[_],A](x:F[A])(implicit instance: WhoFree[F,A]) = println(instance.who(x))
def sumWhoFree[F[_],A](x:F[A])(implicit instance: WhoFree[F,A]) = println(instance.sum(x))
 
 
 
trait Additive[A] {
  def plus(a: A, b: A): A
  def zero: A
}
 
object Additive {
  implicit object IntAdditive extends Additive[Int] {
    def plus(a: Int, b: Int): Int = a + b
    def zero: Int = 0
  }
  implicit object DoubleAdditive extends Additive[Double] {
    def plus(a: Double, b: Double): Double = a + b
    def zero: Double = 0.0
  }
}
 
object Nums {
  trait Num[A] {
    def plus(a: A, b: A): A
    def minus(a: A, b: A): A
    def multiply(a: A, b: A): A
    def divide(a: A, b: A): A
    def zero: A
  }
  object Num{
    implicit object IntNum extends Num[Int] {
      def plus(a: Int, b: Int): Int = a + b
      def minus(a: Int, b: Int): Int = a - b
      def multiply(a: Int, b: Int): Int = a * b
      def divide(a: Int, b: Int): Int = a / b
      def zero: Int = 0
    }
    implicit object DoubleNum extends Num[Double] {
      def plus(a: Double, b: Double): Double = a + b
      def minus(a: Double, b: Double): Double = a - b
      def multiply(a: Double, b: Double): Double = a * b
      def divide(a: Double, b: Double): Double = a / b
      def zero: Double = 0.0
    }
  }
}
 
object FromInts {
  trait FromInt[A] {
    def to(from: Int): A
  }
  object FromInt {
    implicit object FromIntToInt extends FromInt[Int] {
      def to(from: Int): Int = from
    }
    implicit object FromIntToDouble extends FromInt[Double] {
      def to(from: Int): Double = from
    }
  }
}
import Nums._
import FromInts._
 
def average[A](lst: List[A])(implicit a: Num[A], b: FromInt[A]): A = {
  val length: Int = lst.length
  val sum: A  = lst.foldLeft(a.zero)((x, y) => a.plus(x, y))
  a.divide(sum, b.to(length))
}
