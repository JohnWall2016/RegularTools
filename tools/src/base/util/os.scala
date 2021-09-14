package base.util

object os {
  def isWindows = System.getProperty("os.name").toLowerCase().startsWith("windows")
}