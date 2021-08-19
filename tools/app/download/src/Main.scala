import base.command._
import java.nio.file.Path
import java.net.SocketTimeoutException

object Main {
  def main(args: Array[String]) = new Download(args).runCommand()
}

class Download(args: collection.Seq[String]) extends Command(args) {
  banner("下载程序")

  def fetch(urls: Iterable[String], outDir: String, retry: Int = 6) = {
    val session = requests.Session()
    def download(url: String, retry: Int): Unit = {
      try {
        println(s"下载 $url")
        os.write(
          os.Path(outDir) / url.split("/").last,
          session.get(
            url,
            proxy = ("127.0.0.1", 1080)
          )
        )
      } catch {
        case e: SocketTimeoutException =>
          if (retry > 0) {
            println(e)
            download(url, retry - 1)
          } else {
            throw e
          }
      }
    }

    var retryTimes = 0
    val iter = urls.iterator
    while (iter.hasNext) {
      val url = iter.next()
      try {
        download(url, retry)
      } catch {
        case e: requests.RequestFailedException =>
          if (retryTimes <= retry) {
            println(e)
            retryTimes += 1
          } else {
            throw e
          }
      }
    }
  }

  val chengWeiShi = new Subcommand("chengWeiShi") with InputDir {

    val startIndex = trailArg[Int](descr = "开始下载索引")

    val endIndex = opt[Int](descr = "终止下载索引", default = Some(137))

    val side = opt[String](descr = "开始字母", default = Some("A"))

    val retry = opt[Int](descr = "容错次数", default = Some(6))

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
            nextRecord()
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

  val weiShi30Song = new Subcommand("weiShi30Song") with InputDir {
    val startIndex = trailArg[Int](descr = "开始下载索引")

    val endIndex = opt[Int](descr = "终止下载索引", default = Some(43))

    val baseUrl = """http://ftp2.budaedu.org/newGhosa/C007/T034Z/audio-low"""

    def execute(): Unit = {
      val urls = (for (index <- (startIndex() to endIndex())) yield {
        if (index < 30) {
          List(
            f"$baseUrl/34Z0$index%02dAM.mp3",
            f"$baseUrl/34Z0$index%02dBM.mp3"
          )
        } else {
          List(f"$baseUrl/34Z0$index%02dP.mp3")
        }
      }).flatMap(_.iterator)

      fetch(urls, inputDir())
    }
  }

  val weiShi30Song2 = new Subcommand("weiShi30Song2") with InputDir {
    val startIndex =
      trailArg[Int](descr = "开始下载索引", required = false, default = Some(1))

    val endIndex =
      trailArg[Int](descr = "终止下载索引", required = false, default = Some(14))
    //val endIndex = opt[Int](descr = "终止下载索引", default = Some(14))

    val baseUrl = """http://ftp.budaedu.org/ghosa/C007/T0952/audio-low"""

    def execute(): Unit = {
      val urls = (for (index <- (startIndex() to endIndex())) yield {
        List(
          f"$baseUrl/9520$index%02dAM.mp3",
          f"$baseUrl/9520$index%02dBM.mp3"
        )
      }).flatMap(_.iterator)

      fetch(urls, inputDir())
    }
  }

  addSubCommand(chengWeiShi)
  addSubCommand(weiShi30Song)
  addSubCommand(weiShi30Song2)
}
