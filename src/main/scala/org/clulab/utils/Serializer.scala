package org.clulab.utils

import java.io._

object Serializer {

  /** saves object to file */
  def save[A](obj: A, filename: String): Unit = {
    val fos = new FileOutputStream(filename)
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(obj)
    oos.close()
  }

  /** loads object from file */
  def load[A](filename: String): A = {
    val cl = getClass().getClassLoader()
    val fis = new FileInputStream(filename)
    val ois = new ClassLoaderObjectInputStream(cl, fis)
    val obj = ois.readObject().asInstanceOf[A]
    ois.close()
    obj
  }

}
