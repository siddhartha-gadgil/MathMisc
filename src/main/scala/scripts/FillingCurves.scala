package scripts

import scala.util._
import scala.math.Pi
//import breeze.linalg._
import breeze.math._
import breeze.numerics._
import scala.annotation._

/**
 * @author gadgil
 */
object FillingCurves {
  lazy val random = new Random
  
  def randInt = Try(round(1 / random.nextDouble).toInt).getOrElse(0)
  
  
  case class Param(g: Int, m: Int, y: Double){
    def ineq = (8 * g -4) * acosh(1 + 2 * cos (Pi / (4 * g - 2)) ) <=
                        4 * m * acosh (
                            (pow(sin (y), 2) + cos(Pi / (2 * m)))/
                            (pow(cos(y), 2))
                            ) +
    (8 * g - 4 * m) * acosh(
        (pow(sin((Pi * g - 2 * m * y)/(4 * g - 2 * m)), 2) + cos(Pi/ (4 *g - 2 *m)))/
        (pow(cos((Pi * g - 2 * m * y)/(4 * g - 2 * m)), 2))
        )
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
}