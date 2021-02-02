package game.io.tests

import game._
import game.io._

import java.io._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * This file is provided as a base for your own tests, and is similar
  * to the tests used to grade your submission. Modify it to test that
  * your code works properly!
  * You can read more on unit testing in chapter 16
  */

class ChunkIOTest extends AnyFlatSpec with Matchers {

  private def assertGameHasPieces (game: Game, pieces: Seq[((Int, Int), (PieceType, Color))]) = {
    /**
      * A helper method that checks that `game` has the correct `pieces` in
      * the correct places. Use this instead of checking each piece one-by-one
      */
    val board = game.board
    for (((column, row), (pieceType, color)) <- pieces) {
      val player = (if (color == White) game.getWhite else game.getBlack).get
      val locationStr = s"at (${column}, ${row})"
      val piece = board.getPiece(column, row)
      withClue(s"A piece was missing: expected a ${color} ${pieceType} ${locationStr} but") {
        piece should not be None
      }
      withClue(s"Wrong color piece ${locationStr}:") {
        piece.get.getOwner should equal (player)
      }
      withClue(s"Wrong piece ${locationStr}:") {
        piece.get.getType should equal (pieceType)
      }
    }
  }

  // Store the chess "files" and the expected results in Maps, so that we
  // don't have to repeat them when making several tests using the same file
  // Add your own modified versions to test other cases!
  private val chessFiles: Map[String, String] = Map(
    // A few tricks to make including the files in the scala source a bit neater
    // """Triple quotes""" allow us to put a string literal on multiple lines
    // stripMargin removes leading spaces from the start of each line until '|'
    // We also remove all control characters (which include \n) from ChunkIOExample
    // so we can use newlines when writing the test files.
    "ChunkIOExample" -> """
        |CHESS013
        |05072001
        |CMT52Lauri's revenge, not doing great this time either...
        |PLR17B5MarkoKa4Ra6b3c3
        |PLR13W5LauriKd3Nf1
        |END00
        |""".stripMargin.filter(_ >= ' '),
    "HumanIOExample" -> ("""
        |CHESS 1.3 Save file
        |
        |#Game metadata
        |
        |Date : 5.7.2001
        |Black : Marko
        |White : Lauri
        |
        |#Comments
        |
        |Lauri's revenge, not doing great this time either...
        |
        |#Black
        |
        |King : a4
        |Rook : a6
        |Pawn : b3
        |Queen : c8
        |
        |#White
        |
        |King : d3
        |Knight : f1
        |
        |""".stripMargin drop 1)
  )
  private val expectedPieces: Map[String, Seq[((Int, Int), (PieceType, Color))]] = Map(
    // Each element (which we've written on one row each) is a tuple containing
    // the coordinates and the piece we expect, both of which are tuples themselves
    "ChunkIOExample" -> Vector(
      ((0, 3), (King, Black)),
      ((0, 5), (Rook, Black)),
      ((1, 2), (Pawn, Black)),
      ((2, 2), (Pawn, Black)),
      ((3, 2), (King, White)),
      ((5, 0), (Knight, White))
    ),
    "HumanIOExample" -> Vector(
      ((0, 3), (King, Black)),
      ((0, 5), (Rook, Black)),
      ((1, 2), (Pawn, Black)),
      ((2, 7), (Queen, Black)),
      ((3, 2), (King, White)),
      ((5, 0), (Knight, White))
    )
  )

  "ChunkIO.loadGame" should "be able to load a correctly formatted file" in {
    // Run the code we want to test. NOTE that you usually shouldn't try to
    // catch exceptions in tests, because we want the test to fail in that case
    val testInput: Reader = new StringReader(chessFiles("ChunkIOExample"))
    val game = ChunkIO.loadGame(testInput)

    withClue("Loading data failed. Black player missing or wrong name:") {
      game.getBlack.map( _.name ) should equal (Some("Marko"))
    }

    assertGameHasPieces(game, expectedPieces("ChunkIOExample"))
  }


  // You can also make sure that an exception is thrown when needed
  it should "throw a CorruptedChessFileException when encountering an IOException" in {
    // Adding a brokenreader allows throwing simulated exceptions
    val testInput:Reader = new BrokenReader(new StringReader(chessFiles("ChunkIOExample")), 26)

    // Note that initially your code does not read past the file header
    // so this test will fail. 26 bytes is past the header and date (16 bytes)
    intercept[CorruptedChessFileException] {
      ChunkIO.loadGame(testInput)
    }
  }
  // These tests are a start, but you'll want to expand it: check the white
  // player also, test HumanWritableIO, test with different files, etc.
  // Whenever you find a bug in your code, try writing a test for it first
}
