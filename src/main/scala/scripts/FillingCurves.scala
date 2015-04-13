package scripts

import scala.util._
import scala.math.Pi
//import breeze.linalg._
import breeze.math._
import breeze.numerics._
import scala.annotation._
import scala.concurrent._
import ExecutionContext.Implicits.global

/**
 * @author gadgil
 */
object FillingCurves {
  lazy val random = new Random
  
  def randInt = Try(round(1 / random.nextDouble).toInt).getOrElse(0)
  
  
  case class Param(g: Int, m: Int, y: Double){
    assert(g > 0 && (m < g) && (m > 0) && (y > Pi/( 4 * m)) && (y < Pi/2))
    
    val lhs = acosh(1.0 + 2.0 * cos (Pi / ((4 * g) - 2)) )
    
    val rhsFirst  = ((4.0 * m) / ((8.0 * g) - 4)) * acosh (
                            (pow(sin (y), 2) + cos(Pi / (2 * m)))/
                            (pow(cos(y), 2))
                            )
    
   val num1 = pow(sin(((Pi * g) - (2.0 * m * y))/((4.0 * g) - (2.0 * m))), 2)
             
   val num2 = cos(Pi/ ((4 *g) - (2 * m)))
   
   val den = pow(cos(((Pi * g) - (2.0 * m * y))/((4.0 * g) - (2.0 * m))), 2)
     
    val rhsSecond =     (((8.0 * g) - (4.0 * m)) / ((8.0 * g) - 4.0)) * acosh(
        (num1 + num2)/den
        )
    
    val ineq = lhs <= (rhsFirst + rhsSecond)
  }
  
  object Param{
    def nextRandom = {
      val g = 1 + randInt
      val m = random.nextInt(g -1) + 1
      val y = Pi/(4 * m) + (random.nextDouble * ((Pi/2) - (Pi/(4 * m))))
      Param(g, m, y)
    }
  }
  
  @tailrec def check(n: Int) : Option[Param] = {
    if (n < 1) None
    else {
        val param = Param.nextRandom
        if (param.ineq) check(n -1) else Some(param)      
        }
  }
  
  def concCheck(n: Int, copies: Int) = {
    val results = (1 to copies) map ((_) => Future(check(n)))
    results map ((res) => res.onSuccess({case resOpt => println(resOpt)}))
  }
}