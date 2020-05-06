
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

case class Call(betAmt: Int) extends Move(betAmt) {
  override def toString: String = {
    s"Call $betAmt"
  }
}

abstract class Move(betAmt: Int)