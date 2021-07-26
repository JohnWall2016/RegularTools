import base.command._
import java.nio.file.Path

object Main {
  def main(args: Array[String]) = new Download(args).runCommand()
}

class Download(args: collection.Seq[String]) extends Command(args) {
  banner("下载程序")

  val chengWeiShi = new Subcommand("chengWeiShi") with InputDir {

    val startIndex = trailArg[Int](descr = "开始下载索引")

    val endIndex = opt[Int](descr = "终止下载索引", default = Some(137))

    val abc = opt[String](descr = "开始字母", default = Some("A"))

    val retry = opt[Int](descr = "容错次数", default = Some(3))

    def execute(): Unit = {
      val session = requests.Session()

      def downloadFile(fileName: String) = {
        val url =
          s"http://ftp2.budaedu.org/newGhosa/C007/T020Y/audio-low/$fileName"

        println(s"下载 $url")
        os.write(
          os.Path(inputDir()) / fileName,
          session.get(
            url,
            proxy = ("127.0.0.1", 1080)
          )
        )
      }

      var retryTimes = 0
      while (true) {
        try {
          if (startIndex() <= endIndex()) {
            List("AM.mp3", "BM.mp3", "CM.mp3")
              .dropWhile(_ != abc() + "M.mp3")
              .foreach(suffix => downloadFile(f"20Y${startIndex()}%03d$suffix"))
          }
          for {
            index <- (startIndex() + 1) to endIndex()
            suffix <- List("AM.mp3", "BM.mp3", "CM.mp3")
          } downloadFile(f"20Y$index%03d$suffix")
        } catch {
          case e: Exception =>
            if (retryTimes <= retry()) {
              println(e)
              retryTimes += 1
            } else {
              throw e
            }
        }
      }
    }
  }

  addSubCommand(chengWeiShi)
}
