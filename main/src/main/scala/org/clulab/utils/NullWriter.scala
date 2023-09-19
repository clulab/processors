package org.clulab.utils

import java.io.Writer

// Java 11 has thigs built in: Writer.nullWriter()
class NullWriter extends Writer {
  override def write(cbuf: Array[Char], off: Int, len: Int): Unit = ()
  override def flush(): Unit = ()
  override def close(): Unit = ()
}
