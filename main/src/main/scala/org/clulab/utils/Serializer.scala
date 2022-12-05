package org.clulab.utils

import org.clulab.utils.Closer.Releasable

import scala.language.implicitConversions
import java.io._

object Serializer {

  def using[Resource: Releasable, Result](resource: Resource)(function: Resource => Result): Result = {
    Closer.autoClose(resource)(function)
  }

  /** serialize object to output stream */
  def save[A](obj: A, outputStream: OutputStream): Unit = {
    using(new ObjectOutputStream(outputStream)) { oos =>
      oos.writeObject(obj)
    }
  }

  /** serialize object to file */
  def save[A](obj: A, file: File): Unit = {
    using(new BufferedOutputStream(new FileOutputStream(file))) { fos =>
      save(obj, fos)
    }
  }

  /** serialize object to file */
  def save[A](obj: A, filename: String): Unit = {
    using(new BufferedOutputStream(new FileOutputStream(filename))) { fos =>
      save(obj, fos)
    }
  }

  /** serialize object to byte array */
  def save[A](obj: A): Array[Byte] = {
    using(new ByteArrayOutputStream()) { baos =>
      save(obj, baos)
      baos.toByteArray
    }
  }

  /* deserialize from input stream */
  def load[A](inputStream: InputStream): A = {
    load[A](inputStream, getClass().getClassLoader())
  }

  /* deserialize from input stream */
  def load[A](inputStream: InputStream, classLoader: ClassLoader): A = {
    using(new ClassLoaderObjectInputStream(classLoader, inputStream)) { ois =>
      ois.readObject().asInstanceOf[A]
    }
  }

  /* deserialize from file */
  def load[A](file: File): A = {
    load[A](file, getClass().getClassLoader())
  }

  /* deserialize from file */
  def load[A](file: File, classLoader: ClassLoader): A = {
    using(new BufferedInputStream(new FileInputStream(file))) { fis =>
      load[A](fis, classLoader)
    }
  }

  /* deserialize from file */
  def load[A](filename: String): A = {
    load[A](filename, getClass().getClassLoader())
  }

  /* deserialize from file */
  def load[A](filename: String, classLoader: ClassLoader): A = {
    using(new BufferedInputStream(new FileInputStream(filename))) { fis =>
      load[A](fis, classLoader)
    }
  }

  /* deserialize from byte array */
  def load[A](bytes: Array[Byte]): A = {
    load[A](bytes, getClass().getClassLoader())
  }

  /* deserialize from byte array */
  def load[A](bytes: Array[Byte], classLoader: ClassLoader): A = {
    using(new ByteArrayInputStream(bytes)) { bais =>
      load[A](bais, classLoader)
    }
  }

}
