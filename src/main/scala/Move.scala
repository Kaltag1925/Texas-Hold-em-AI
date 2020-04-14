
case class Raise(betAmt: Int) extends Move(betAmt)
case class Fold() extends Move(0)
case class Check() extends Move(0)
case class Call(betAmt: Int) extends Move(betAmt)
case class AllIn(betAmt: Int) extends Move(betAmt)

abstract class Move(betAmt: Int)