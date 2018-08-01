package org.clulab.utils

import java.io._

object Serializer {

  /** serialize object to output stream */
  def save[A](obj: A, outputStream: OutputStream): Unit = {
    val oos = new ObjectOutputStream(outputStream)
    oos.writeObject(obj)
    oos.close()
  }

  /** serialize object to file */
  def save[A](obj: A, file: File): Unit = {
    val fos = new FileOutputStream(file)
    save(obj, fos)
    fos.close()
  }

  /** serialize object to file */
  def save[A](obj: A, filename: String): Unit = {
    val fos = new FileOutputStream(filename)
    save(obj, fos)
    fos.close()
  }

  /** serialize object to byte array */
  def save[A](obj: A): Array[Byte] = {
    val baos = new ByteArrayOutputStream()
    save(obj, baos)
    val bytes = baos.toByteArray
    baos.close()
    bytes
  }

  /* deserialize from input stream */
  def load[A](inputStream: InputStream): A = {
    load(inputStream, getClass().getClassLoader())
  }

  /* deserialize from input stream */
  def load[A](inputStream: InputStream, classLoader: ClassLoader): A = {
    val ois = new ClassLoaderObjectInputStream(classLoader, inputStream)
    val obj = ois.readObject().asInstanceOf[A]
    ois.close()
    obj
  }

  /* deserialize from file */
  def load[A](file: File): A = {
    load(file, getClass().getClassLoader())
  }

  /* deserialize from file */
  def load[A](file: File, classLoader: ClassLoader): A = {
    val fis = new FileInputStream(file)
    val obj = load[A](fis, classLoader)
    fis.close()
    obj
  }

  /* deserialize from file */
  def load[A](filename: String): A = {
    load(filename, getClass().getClassLoader())
  }

  /* deserialize from file */
  def load[A](filename: String, classLoader: ClassLoader): A = {
    val fis = new FileInputStream(filename)
    val obj = load[A](fis, classLoader)
    fis.close()
    obj
  }

  /* deserialize from byte array */
  def load[A](bytes: Array[Byte]): A = {
    load(bytes, getClass().getClassLoader())
  }

  /* deserialize from byte array */
  def load[A](bytes: Array[Byte], classLoader: ClassLoader): A = {
    val bais = new ByteArrayInputStream(bytes)
    val obj = load[A](bais, classLoader)
    bais.close()
    obj
  }

}
