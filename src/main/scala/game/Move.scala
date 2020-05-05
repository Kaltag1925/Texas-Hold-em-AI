
package game

case class Raise(betAmt: Int) extends Move(betAmt) {
  override def toString: String = {
    s"Raise $betAmt"
  }
}
case class Fold() extends Move(0) {
  override def toString: String = {
    s"Fold"
  }
}
case class Check() extends Move(0) {
  override def toString: String = {
    s"Check"
  }
}
case class Call(betAmt: Int) extends Move(betAmt) {
  override def toString: String = {
    s"Call $betAmt"
  }
}
case class AllIn(betAmt: Int) extends Move(betAmt) {
  override def toString: String = {
    s"All In"
  }
}

abstract class Move(betAmt: Int)