package game

import agent.Agent

import scala.collection.mutable

class PlayerList(players: List[Agent]) {
  import PlayerList._
  private val _inGame: mutable.Set[Agent] = mutable.Set()
  _inGame.addAll(players)
  private var _size = players.size
  private var first = new Node(null, players.head, null)

  {
    var c = first
    for (p <- players.tail.init) {
      c.next = new Node(c, p, null)
      c = c.next
    }
    c.next = new Node(c, players.last, first)
  }

  private var current = first

  def isFirst: Boolean = current == first

  def reset(): Unit = current = first

  def hasNext: Boolean = current.next != first

  def updateStart(): Unit = first = current

  def next(): Agent = {
    current = current.next
    current.data
  }

  def size: Int = _size

  def remove(): Unit = {
    _inGame.remove(current.data)
    if (current == first) {
      first = current.next
    }
    current = current.next
    current.prev = current.prev.prev
    current.prev.next = current
    _size -=1
  }

  def foreach(f: Agent => Unit): Unit = {
    _inGame.foreach(f)
  }

  def map[A](f: Agent => A): List[A] = {
    _inGame.toList.map(f)
  }

  def forall(f: Agent => Boolean): Boolean = {
    _inGame.forall(f)
  }

  def inGame: List[Agent] = _inGame.toList
  def currentTurn: Agent = current.data
}

object PlayerList {
  private class Node(var prev: Node, var data: Agent, var next: Node)
}