package org.liberalist.schulze.prototype

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.annotation.tailrec

import scala.collection.JavaConverters._

object PrototypeApp extends App {
  val path = Paths.get(args(0))
  val charset = StandardCharsets.UTF_8
  val ballotLines = Files.readAllLines(path, charset).asScala

  case class Ballot(candidates: String*) {
    if (candidates != candidates.distinct) throw new RuntimeException("Duplicate candidate")
  }

  val natoPhonetic = Map(
      'A' -> "Alpha",
      'B' -> "Bravo",
      'C' -> "Charlie",
      'D' -> "Delta",
      'E' -> "Echo",
      'F' -> "Foxtrot",
      'G' -> "Golf",
      'H' -> "Hotel",
      'I' -> "India",
      'J' -> "Juliet",
      'K' -> "Kilo",
      'L' -> "Lima",
      'M' -> "Mike",
      'N' -> "November",
      'O' -> "Oscar",
      'P' -> "Papa",
      'Q' -> "Quebec",
      'R' -> "Romeo",
      'S' -> "Sierra",
      'T' -> "Tango",
      'U' -> "Uniform",
      'V' -> "Victor",
      'W' -> "Whiskey",
      'X' -> "X-ray",
      'Y' -> "Yankee",
      'Z' -> "Zulu"
  )

  def letterToCandidate(letter:Char):String = {
    natoPhonetic(letter.toUpper)
  }

  def lettersToBallot(letters:String):Ballot = {
    Ballot(letters.map(letterToCandidate):_*)
  }

  def createBallots(quantity:Int, letters:String):Seq[Ballot] = {
    val ballot = lettersToBallot(letters)
    Stream.continually(ballot).take(quantity)
  }

  val LinePattern = """(\d+) (\w+)""".r

  def lineToBallots(line:String):Seq[Ballot] = {
    val LinePattern(quantityString, letters) = line
    val quantity = quantityString.toInt
    createBallots(quantity, letters)
  }

  val ballots = ballotLines.flatMap(lineToBallots)

  def candidatesFromBallots(ballots: Seq[Ballot]): Seq[String] = {
    ballots.flatMap(candidatesFromBallot).distinct.sorted
  }

  def candidatesFromBallot(ballot: Ballot): Seq[String] = ballot.candidates

  val candidates = candidatesFromBallots(ballots)

  class Matrix(val preferences: Map[String, Map[String, Int]]) {
    def addPreference(winner: String, loser: String): Matrix = {
      val oldValue = preferences(winner)(loser)
      val newValue = oldValue + 1
      setPreference(winner, loser, newValue)
    }

    def preference(winnerIndex: Int, loserIndex: Int): Int = {
      val winner = candidates(winnerIndex)
      val loser = candidates(loserIndex)
      preference(winner,loser)
    }

    def preference(winner: String, loser: String): Int = {
      preferences(winner)(loser)
    }

    def setPreference(winnerIndex: Int, loserIndex: Int, value: Int): Matrix = {
      val winner = candidates(winnerIndex)
      val loser = candidates(loserIndex)
      setPreference(winner, loser, value)
    }

    def setPreference(winner: String, loser: String, value: Int): Matrix = {
      val oldLossMap = preferences(winner)
      val newLossMap = oldLossMap.updated(loser, value)
      val newPreferences = preferences.updated(winner, newLossMap)
      new Matrix(newPreferences)
    }

    def strongestPaths: Matrix = {
      var result = Matrix.Empty
      for {
        i <- candidates.indices
        j <- candidates.indices
        if i != j
      } {
        if (preference(i, j) > preference(j, i)) {
          result = result.setPreference(i, j, preference(i, j))
        } else {
          result = result.setPreference(i, j, 0)
        }
      }
      for {
        i <- candidates.indices
        j <- candidates.indices
        if i != j
        k <- candidates.indices
        if i != k && j != k
      } {
        val min = Math.min(result.preference(j, i), result.preference(i, k))
        val max = Math.max(result.preference(j, k), min)
        result = result.setPreference(j, k, max)
      }
      result
    }

    def lines():Seq[String] = {
      val header = Seq("") ++ candidates
      val body = for {
        winner <- candidates
      } yield {
        Seq(winner) ++ candidates.map(loser => preferences(winner)(loser))
      }
      val rows = Seq(header) ++ body
      TableUtil.createTable(rows)
    }

    def rankings:Seq[Seq[String]] = {
      val soFar = Seq()
      rankingsRecursive(soFar)
    }

    @tailrec
    private def rankingsRecursive(soFar:Seq[Seq[String]]):Seq[Seq[String]] = {
      val unranked = findUnranked(soFar)
      if(unranked.isEmpty){
        soFar
      } else {
        val undefeated = findUndefeated(unranked)
        rankingsRecursive(soFar :+ undefeated)
      }
    }

    private def findUnranked(soFar:Seq[Seq[String]]):Seq[String] = {
      candidates.filter(isUnranked(_, soFar))
    }

    private def isUnranked(candidate:String, soFar:Seq[Seq[String]]):Boolean = {
      soFar.forall(_.forall(candidate != _))
    }

    private def findUndefeated(unranked:Seq[String]):Seq[String] = {
      unranked.filter(isUndefeated(_, unranked))
    }

    private def isUndefeated(candidate:String, unranked:Seq[String]):Boolean = {
      unranked.forall(isUndefeated(candidate, _))
    }

    private def isUndefeated(candidate:String, other:String):Boolean = {
      if(candidate == other){
        true
      } else if(preference(candidate, other) < preference(other, candidate)) {
        false
      } else {
        true
      }
    }
  }

  object Matrix {
    private val EmptyLoss: Map[String, Int] = Map().withDefaultValue(0)
    private val EmptyWinLoss: Map[String, Map[String, Int]] = Map().withDefaultValue(EmptyLoss)
    val Empty: Matrix = new Matrix(EmptyWinLoss)

    def add(left:Matrix, right:Matrix):Matrix = {
      var result = Empty
      for {
        winner <- candidates
        loser <- candidates
        if winner != loser
      }{
        val leftValue = left.preferences(winner)(loser)
        val rightValue = right.preferences(winner)(loser)
        val newValue = leftValue + rightValue
        result = result.setPreference(winner, loser, newValue)
      }
      result
    }
  }

  def ballotToMatrix(ballot: Ballot): Matrix = {
    var matrix = Matrix.Empty
    for {
      winnerIndex <- ballot.candidates.indices
      loserIndex <- winnerIndex+1 until ballot.candidates.length
    } {
      matrix = matrix.addPreference(ballot.candidates(winnerIndex), ballot.candidates(loserIndex))
    }
    matrix
  }

  val matrix = ballots.map(ballotToMatrix).foldLeft(Matrix.Empty)(Matrix.add)

  def place(index:Int):String = {
    val s = index+1 match {
      case 1 => "1st"
      case 2 => "2nd"
      case 3 => "3rd"
      case x => x.toString + "th"
    }
    s + " place"
  }

  def formatRanking(candidates:Seq[String], index:Int):String = {
    if(candidates.size == 1){
      place(index) + ": " + candidates.head
    } else {
      "Tie for " + place(index) + ": " + candidates.mkString(", ")
    }
  }

  def formatRankings(rankings:Seq[Seq[String]]):Seq[String] = {
    rankings.zipWithIndex.map((formatRanking _).tupled)
  }

  println("Ballots")
  ballotLines.foreach(println)
  println("Matrix of pairwise preferences")
  matrix.lines().foreach(println)
  println("Strengths of the strongest paths")
  val strongestPaths = matrix.strongestPaths
  strongestPaths.lines().foreach(println)
  val rankings = strongestPaths.rankings
  println("Rankings")
  formatRankings(rankings).foreach(println)
}
