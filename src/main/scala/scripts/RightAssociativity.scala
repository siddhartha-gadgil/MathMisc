object RightAssociativity {


  class RHS{
    object ->:{
      def apply(n: Int) = 2 * n

      def unapply(n : Int) = if (n % 2 == 0) Some (n/2) else None
    }
  }

  val R = new RHS
  import R._

  3 ->: R //> res0: Int = 6
  val divBy2: Int => Int = {
    case R.->:(a) => a

//    case a ->: R => a
  }
  divBy2(6) //> res1: Int = 3
}
