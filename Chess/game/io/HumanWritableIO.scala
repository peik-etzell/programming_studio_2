package game.io

import java.io.BufferedReader
import java.io.IOException
import java.io.Reader
import game._

/**********************************************************************
 *
 *   This file is returned in Exercise 3.2
 *
 *   The idea is to read a file written manually by
 *   someone. There can be useless whitespace all around the file.
 *
 **********************************************************************/

object HumanWritableIO {

    def loadGame(input: Reader): Game = {

        /**
         * This is the game object this method will fill with data. The object
         * is returned when the file ends and everything is ok.
         */

        val board = new Board()
        val game  = new Game(board)

        /*
        You might want to keep track of different required parts of the file
         if some of these are missing, the file is not valid and a CorruptedChessFileException should be thrown
         If you figure out something better, you don't have to keep these variables.
        */
        var infoRead = false
        var whiteRead = false
        var blackRead = false



        // BufferedReader allows us to read line by line (readLine method)
        val lineReader = new BufferedReader(input)


        try {

            /*
             * Read the first line, i.e. the header.
             * You can also use this variable for reading all the section headers,
             * or you can do better and use only vals and get rid of this.
             */

            var currentLine = lineReader.readLine().trim.toLowerCase

            // Process the header we just read.
            // NOTE: To test the line below you must test the class once with a
            // broken header

            if (!((currentLine startsWith "chess") && (currentLine endsWith "save file"))) {
                throw new CorruptedChessFileException("Unknown file type")
            }
            
            while (currentLine != null) {
                currentLine.trim.toLowerCase match {
                    case "#game metadata" => meta(); infoRead = true
                    case "#black" => pieces(Black); blackRead = true
                    case "#white" => pieces(White); whiteRead = true
                    case other => read()
                }
            }

            //Reads the next defined line of text
            def read(): Unit = {
                currentLine = lineReader.readLine()
                if (currentLine != null && currentLine.isEmpty) read()
            }

            def meta() = {
                read()
                while (currentLine != null && currentLine.head != '#') {
                    val split = currentLine.split(':').map(_.trim)
                    if (split.length == 2) {
                        val name = split(1)
                        val tag = split(0)
                        tag match {
                            case "White" => game.addPlayer(new Player(name, White))
                            case "Black" => game.addPlayer(new Player(name, Black))
                            case _ =>
                        }
                    }
                    read()
                }
            }

            def pieces(color: Color) = {
                val player = try {
                    color match {
                        case White => game.getWhite.get
                        case Black => game.getBlack.get
                    }
                } catch {
                    case e:NoSuchElementException => throw new CorruptedChessFileException(s"no $color player")
                }
                read()
                while (currentLine != null && currentLine.head != '#') {
                    if (currentLine.nonEmpty) {
                        try {
                            val split = currentLine.split(':').map(_.trim)
                            val piece = split(0) match {
                                case "King" => new King(player)
                                case "Queen" => new Queen(player)
                                case "Rook" => new Rook(player)
                                case "Bishop" => new Bishop(player)
                                case "Knight" => new Knight(player)
                                case "Pawn" => new Pawn(player)
                                case other => throw new CorruptedChessFileException("Bad piece type.")
                            }
                            val pos = split(1)
                            val (column, row) = (Board.columnCharToInteger(pos(0)), Board.rowCharToInteger(pos(1)))
                            try {
                                board.setPiece(piece, column, row)
                            } catch {
                                case e:ArrayIndexOutOfBoundsException => throw new CorruptedChessFileException("Bad piece positions.")
                            }
                        }
                    }
                    read()
                }
            }

            if (infoRead && whiteRead && blackRead) game else throw new CorruptedChessFileException("Bad info.")
        } catch {
            case e: IOException =>


            // To test this part the stream would have to cause an
            // IOException. That's a bit complicated to test. Therefore we have
            // given you a "secret tool", class BrokenReader, which will throw
            // an IOException at a requested position in the stream.
            // Throw the exception inside any chunk, but not in the chunk
            // header.

            val chessExc = new CorruptedChessFileException("Reading the chess data failed.")

            // Append the information about the initial cause for use in
            // debugging. Otherwise the programmer cannot know the method or
            // line number causing the problem.

            chessExc.initCause(e)

            throw chessExc
        }
    }
}
