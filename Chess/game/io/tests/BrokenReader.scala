package game.io.tests

import java.io.IOException
import java.io.Reader

/**
 * This class is used to artificially create problems. :)
 *
 * The class will take a normal Reader object and serve
 * all data read from that Reader until a given breaking
 * point is reached. At this point the read method will throw
 * an IOException.
 *
 * The constructor constructs a new BrokenReader from a given normal Reader.
 *
 * @param realReader The real reader object used to serve data until the breaking point is reached.
 * @param throwExceptionAt The breaking point position.
 */

class BrokenReader(realReader: Reader, throwExceptionAt: Int) extends Reader {

    private var throwCounter = throwExceptionAt

    /*
     * If the throwCounter is set high enough, all data can be read.
     * In this case closing the stream will throw the IOException instead.
     */
    override def close() = {
        realReader.close()
        throw new IOException("Simulated I/O problem")
    }

    /*
     * Works as a normal read-method until the breaking point is reached.
     * It then throws a simulated IOException.
     */
    override def read(cbuf: Array[Char], off: Int, len: Int): Int = {
        if (throwCounter <= len){
            throw new IOException("Simulated I/O problem")
        }

        throwCounter -= len
        realReader.read(cbuf, off, len)
    }

    /*
     * Works as a normal read-method until the breaking point is reached.
     * It then throws a simulated IOException.
     */
    override def read(): Int = {
        if (throwCounter == 1){
            throw new IOException("Simulated I/O problem")
        }

        throwCounter -= 1
        realReader.read()
    }

    override def skip(len: Long) = {
        if (throwCounter <= len){
            throw new IOException("Simulated I/O problem")
        }
        throwCounter -= len.toInt
        realReader.skip(len)
    }


}
