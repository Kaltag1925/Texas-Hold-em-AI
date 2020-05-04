package game

import java.io.{BufferedWriter, File, FileWriter}

object Test extends App {
  val x =  Card.deck.combinations(5).map(WinningHand.apply).toList.sorted.map(_.hand.map(cardToString).mkString("") + "\n")
  println(x.size)
  writeFile("hands.txt",x)
  val y =  Card.deck.combinations(5).map(WinningHand.apply).toList.sorted
  println(y.groupBy(_.getClass).view.mapValues(_.size).toMap)

  def cardToString(card: Card): String = {
    card.num + "," + card.suit + "\t"
  }

  def writeFile(filename: String, lines: Seq[String]): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    for (line <- lines) {
      bw.write(line)
    }
    bw.close()
  }
}
