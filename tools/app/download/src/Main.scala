import java.net.SocketTimeoutException
import java.net.URI
import java.net.URLEncoder
import java.nio.file.Files
import java.nio.file.Path
import javax.net.ssl.SSLException

import base.command._

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
  ): Unit = {
    fetchByUrlAndNames(
      urls.map(it => (it, it.split("/").last)),
      outDir,
      retry,
      proxy
    )
  }

  def fetchByUrlAndNames(
      urlAndNames: Iterable[(String, String)],
      outDir: String,
      retry: Int = 6,
      proxy: Option[(String, Int)] = None
  ): Unit = {
    val session = requests.Session()
    def download(url: String, name: String, retry: Int): Unit = {
      try {
        println(s"下载 $url -> $outDir")
        os.write(
          os.Path(outDir) / name,
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
        case e @ (_: SocketTimeoutException | _: SSLException) =>
          if (retry > 0) {
            println(e)
            download(url, name, retry - 1)
          } else {
            throw e
          }
      }
    }

    var retryTimes = 0
    val iter = urlAndNames.iterator
    while (iter.hasNext) {
      val (url, name) = iter.next()
      try {
        download(url, name, retry)
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

  def yuQieShiDiLunDownload(
      name: String,
      subdir: String,
      start: Int,
      end: Int,
      getFiles: Int => List[String] = { index =>
        List(
          f"瑜伽師地論-$index%03da.m4a",
          f"瑜伽師地論-$index%03db.m4a",
          f"瑜伽師地論-$index%03dc.m4a"
        )
      }
  ) = {
    val rootDir = if (base.util.os.isWindows) {
      """f:/瑜伽師地論_玅境長老"""
    } else {
      """/Users/wangjiong/Documents/瑜伽師地論_玅境長老"""
    }

    new Subcommand(name) {
      val startIndex =
        trailArg[Int](descr = "开始下载索引", required = false, default = Some(start))

      val endIndex =
        trailArg[Int](descr = "终止下载索引", required = false, default = Some(end))

      val inputDir = trailArg[String](
        descr = "文件导入路径",
        required = false,
        default = Some(s"""$rootDir/$subdir""")
      )

      def execute(): Unit = {
        val urlAndNames = (for (index <- (startIndex() to endIndex())) yield {
          getFiles(index).map { it =>
            val uri = new URI(
              "https",
              "fayun.org",
              "/public/php/download.php",
              s"file=media/釋論/瑜伽師地論・本地分/${subdir.split("""\.""").last}/audio/$it",
              null
            )
            (uri.toASCIIString, it)
          }
        }).flatMap(_.iterator)

        val outputPath = Path.of(inputDir())
        if (!Files.exists(outputPath)) {
          Files.createDirectories(outputPath)
        }

        fetchByUrlAndNames(urlAndNames, outputPath.toString(), proxy = None)
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

  addSubCommand(
    yuQieShiDiLunDownload(
      "yuQieShiDiLun1",
      "1.初發論端",
      1,
      7
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload(
      "yuQieShiDiLun2",
      "2.卷01",
      8,
      21
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload(
      "yuQieShiDiLun3",
      "3.卷02",
      22,
      34
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload(
      "yuQieShiDiLun4",
      "4.卷03",
      35,
      42
    )
  )

  def yuQieShiDiLunDownload2(
      name: String,
      subdir: String,
      start: Int,
      end: Int
  ) = {
    yuQieShiDiLunDownload(
      name,
      subdir,
      start,
      end,
      index => {
        List(
          f"瑜伽師地論-$index%03d.m4a",
          f"瑜伽師地論-$index%03dQA.m4a"
        )
      }
    )
  }

  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun5",
      "5.卷04",
      43,
      47
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun6",
      "6.卷05",
      48,
      57
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun7",
      "7.卷06",
      58,
      66
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun8",
      "8.卷07",
      67,
      74
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun9",
      "9.卷08",
      75,
      84
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun10",
      "10.卷09",
      85,
      98
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun11",
      "11.卷10",
      99,
      112
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload(
      "yuQieShiDiLun12",
      "12.卷11",
      113,
      129,
      index => {
        List(
          f"T$index%03d.mp3",
          f"T$index%03dQ.mp3"
        )
      }
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload(
      "yuQieShiDiLun13",
      "13.卷12至13-1",
      130,
      155,
      index => {
        List(
          f"T$index%03d.mp3",
          f"T$index%03dQ.mp3"
        )
      }
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun14",
      "13.卷13-2",
      156,
      163
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun15",
      "14.卷14",
      164,
      183
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun16",
      "15.卷15",
      184,
      194
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun17",
      "16.卷16",
      195,
      207
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun18",
      "17.卷17",
      208,
      216
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun19",
      "18.卷18",
      217,
      224
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun20",
      "19.卷19",
      225,
      233
    )
  )
  addSubCommand(
    yuQieShiDiLunDownload2(
      "yuQieShiDiLun21",
      "20.卷20",
      234,
      245
    )
  )
}
