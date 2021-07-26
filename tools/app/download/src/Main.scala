import base.command._
import java.nio.file.Path
import java.net.SocketTimeoutException

object Main {
  def main(args: Array[String]) = new Download(args).runCommand()
}

class Download(args: collection.Seq[String]) extends Command(args) {
  banner("下载程序")

  val chengWeiShi = new Subcommand("chengWeiShi") with InputDir {

    val startIndex = trailArg[Int](descr = "开始下载索引")

    val endIndex = opt[Int](descr = "终止下载索引", default = Some(137))

    val side = opt[String](descr = "开始字母", default = Some("A"))

    val retry = opt[Int](descr = "容错次数", default = Some(3))

    val sides = Seq("A", "B", "C")

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

      case class Record(index: Int, side: String)

      var currentRecord = Record(startIndex(), side())

      var continue = true
      while (continue) {
        val sideIndex = sides.indexOf(currentRecord.side)

        if (currentRecord.index > endIndex() || sideIndex == -1) {
          continue = false
        } else {
          val fileName =
            f"20Y${currentRecord.index}%03d${currentRecord.side}M.mp3"

          try {
            downloadFile(fileName)
          } catch {
            case e: SocketTimeoutException =>
              if (retryTimes <= retry()) {
                println(e)
                retryTimes += 1
              } else {
                throw e
              }
            case e: requests.RequestFailedException =>
              if (retryTimes <= retry()) {
                println(e)
                retryTimes += 1
                nextRecord()
              } else {
                throw e
              }
          }

          def nextRecord() = {
            val newSideIndex = sideIndex + 1
            currentRecord = Record(
              if (newSideIndex == sides.size) {
                currentRecord.index + 1
              } else {
                currentRecord.index
              },
              if (newSideIndex == sides.size) {
                sides(0)
              } else {
                sides(newSideIndex)
              }
            )
          }
        }
      }
    }
  }

  addSubCommand(chengWeiShi)
}
