import base.command._
import java.nio.file.Path
import java.net.SocketTimeoutException

object Main {
  def main(args: Array[String]) = new Download(args).runCommand()
}

class Download(args: collection.Seq[String]) extends Command(args) {
  banner("下载程序")

  def fetch(
      urls: Iterable[String],
      outDir: String,
      retry: Int = 6,
      proxy: Option[(String, Int)] = None
  ) = {
    val session = requests.Session()
    def download(url: String, retry: Int): Unit = {
      try {
        println(s"下载 $url")
        os.write(
          os.Path(outDir) / url.split("/").last,
          if (proxy.isEmpty) {
            session.get(url)
          } else {
            session.get(
              url,
              proxy = proxy.get
            )
          }
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

  def budaeduDownload(name: String, baseUrl: String, start: Int, end: Int)(
      getUrls: (String, Int) => List[String]
  ) = {
    new Subcommand(name) with InputDir {
      val startIndex =
        trailArg[Int](descr = "开始下载索引", required = false, default = Some(start))

      val endIndex =
        trailArg[Int](descr = "终止下载索引", required = false, default = Some(end))

      def execute(): Unit = {
        val urls = (for (index <- (startIndex() to endIndex())) yield {
          getUrls(baseUrl, index)
        }).flatMap(_.iterator)

        fetch(urls, inputDir(), proxy = Some(("127.0.0.1", 1080)))
      }
    }
  }

  val weiShi30Song = budaeduDownload(
    "weiShi30Song",
    """"http://ftp2.budaedu.org/newGhosa/C007/T034Z/audio-low"""",
    1,
    43
  ) { (baseUrl, index) =>
    if (index < 30) {
      List(
        f"$baseUrl/34Z0$index%02dAM.mp3",
        f"$baseUrl/34Z0$index%02dBM.mp3"
      )
    } else {
      List(f"$baseUrl/34Z0$index%02dP.mp3")
    }
  }

  val weiShi30Song2 = budaeduDownload(
    "weiShi30Song2",
    """http://ftp.budaedu.org/ghosa/C007/T0952/audio-low""",
    1,
    14
  ) { (baseUrl, index) =>
    List(
      f"$baseUrl/9520$index%02dAM.mp3",
      f"$baseUrl/9520$index%02dBM.mp3"
    )
  }

  val weiShi30Song3 = budaeduDownload(
    "weiShi30Song3",
    """http://ftp2.budaedu.org/newGhosa/C007/T027X/audio-low""",
    1,
    118
  ) { (baseUrl, index) =>
    List(
      f"$baseUrl/27X$index%03dP.mp3"
    )
  }

  val ruPuShaXinLun = budaeduDownload(
    "ruPuShaXinLun",
    """http://ftp4.budaedu.org/ghosa4/C025/TI001359/audio-low""",
    1,
    91
  ) { (baseUrl, index) =>
    List(
      f"$baseUrl/TI001359-$index%05d-P.mp3"
    )
  }

  val faXiangZong1 = budaeduDownload(
    "faXiangZong1",
    """http://ftp3.budaedu.org/ghosa3/C038/T043S/audio-low""",
    1,
    11
  ) { (baseUrl, index) =>
    List(
      f"$baseUrl/43S$index%03dP.mp3"
    )
  }

  val weiShiRengShi = budaeduDownload(
    "weiShiRengShi", // 唯識的認識與修證哲學─境行果[T032D]
    """http://ftp2.budaedu.org/newGhosa/C007/T032D/audio-low""",
    1,
    19
  ) { (baseUrl, index) =>
    List(
      f"$baseUrl/32D$index%03dAM.mp3",
      f"$baseUrl/32D$index%03dBM.mp3"
    )
  }

  def fayunDownload(name: String, file: String, start: Int, end: Int) = {
    new Subcommand(name) with InputDir {
      val startIndex =
        trailArg[Int](descr = "开始下载索引", required = false, default = Some(start))

      val endIndex =
        trailArg[Int](descr = "终止下载索引", required = false, default = Some(end))

      val baseUrl =
        s"""https://fayun.org/public/php/download.php?file=media/釋論/瑜伽師地論・本地分/$file/audio/瑜伽師地論-"""

      def execute(): Unit = {
        val urls = (for (index <- (startIndex() to endIndex())) yield {
          List(
            f"$baseUrl$index%03da.m4a",
            f"$baseUrl$index%03db.m4a",
            f"$baseUrl$index%03dc.m4a"
          )
        }).flatMap(_.iterator)

        fetch(urls, inputDir(), proxy = None)
      }
    }
  }

  addSubCommand(chengWeiShi)
  addSubCommand(weiShi30Song)
  addSubCommand(weiShi30Song2)
  addSubCommand(weiShi30Song3)
  addSubCommand(ruPuShaXinLun)
  addSubCommand(faXiangZong1)
  addSubCommand(weiShiRengShi)

  addSubCommand(fayunDownload("yuQieShiDiLun1", "初發論端", 1, 7))
  addSubCommand(fayunDownload("yuQieShiDiLun2", "卷01", 8, 21))
}
