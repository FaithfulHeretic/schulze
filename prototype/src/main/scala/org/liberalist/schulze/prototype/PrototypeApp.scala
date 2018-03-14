package org.liberalist.schulze.prototype

object PrototypeApp extends App {

  case class Ballot(candidates: String*) {
    if (candidates != candidates.distinct) throw new RuntimeException("Duplicate candidate")
  }

  //  val ballots = Seq(
  //    Ballot("kappa", "beta", "alpha", "gamma", "delta"),
  //    Ballot("kappa", "alpha", "delta", "gamma", "beta"),
  //    Ballot("beta", "kappa", "alpha", "delta", "gamma"),
  //    Ballot("delta", "gamma", "alpha", "beta", "kappa"),
  //    Ballot("delta", "alpha", "beta", "gamma", "kappa"),
  //    Ballot("gamma", "alpha", "beta", "kappa", "delta"),
  //    Ballot("kappa", "gamma", "delta", "alpha", "beta"),
  //    Ballot("delta", "alpha", "kappa", "gamma", "beta"),
  //    Ballot("kappa", "gamma", "delta", "beta", "alpha"),
  //    Ballot("kappa", "gamma", "delta", "beta", "alpha"),
  //    Ballot("alpha", "kappa", "delta", "beta", "gamma"),
  //    Ballot("delta", "beta", "gamma", "alpha", "kappa"),
  //    Ballot("delta", "beta", "alpha", "gamma", "kappa"),
  //    Ballot("delta", "alpha", "beta", "gamma", "kappa"),
  //    Ballot("beta", "alpha", "kappa", "gamma", "delta"),
  //    Ballot("gamma", "delta", "kappa", "beta", "alpha"),
  //    Ballot("alpha", "kappa", "gamma", "beta", "delta"),
  //    Ballot("alpha", "delta", "gamma", "kappa", "beta"),
  //    Ballot("kappa", "gamma", "alpha", "beta", "delta"),
  //    Ballot("beta", "alpha", "delta", "kappa", "gamma"),
  //    Ballot("beta", "kappa", "alpha", "delta", "gamma"),
  //    Ballot("alpha", "gamma", "kappa", "beta", "delta"),
  //    Ballot("gamma", "alpha", "beta", "delta", "kappa"),
  //    Ballot("gamma", "beta", "alpha", "delta", "kappa"),
  //    Ballot("beta", "kappa", "delta", "alpha", "gamma"),
  //    Ballot("alpha", "delta", "gamma", "kappa", "beta"),
  //    Ballot("kappa", "beta", "gamma", "alpha", "delta"),
  //    Ballot("beta", "gamma", "alpha", "kappa", "delta"),
  //    Ballot("beta", "kappa", "alpha", "gamma", "delta"),
  //    Ballot("kappa", "gamma", "alpha", "beta", "delta"),
  //    Ballot("alpha", "beta", "delta", "gamma", "kappa"),
  //    Ballot("kappa", "gamma", "alpha", "beta", "delta"),
  //    Ballot("gamma", "kappa", "delta", "beta", "alpha"),
  //    Ballot("gamma", "kappa", "alpha", "beta", "delta"),
  //    Ballot("kappa", "delta", "alpha", "beta", "gamma")
  //  )

  def letterToCandidate(letter:Char):String = {
    letter match {
      case 'A' => "alpha"
      case 'B' => "beta"
      case 'C' => "charlie"
      case 'D' => "delta"
      case 'E' => "echo"
    }
  }

  def lettersToBallot(letters:String):Ballot = {
    Ballot(letters.map(letterToCandidate):_*)
  }

  def createBallots(quantity:Int, letters:String):Seq[Ballot] = {
    val ballot = lettersToBallot(letters)
    Stream.continually(ballot).take(quantity)
  }

  val ballots =
    createBallots(5, "ACBED") ++
      createBallots(5, "ADECB") ++
      createBallots(8, "BEDAC") ++
      createBallots(3, "CABED") ++
      createBallots(7, "CAEBD") ++
      createBallots(2, "CBADE") ++
      createBallots(7, "DCEBA") ++
      createBallots(8, "EBADC")

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

  println("Matrix of pairwise preferences")
  matrix.lines().foreach(println)
  println("Strengths of the strongest paths")
  matrix.strongestPaths.lines().foreach(println)
}
