import base.command._
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.Path
import com.mpatric.mp3agic.Mp3File
import com.mpatric.mp3agic.ID3v24Tag
import java.nio.file.CopyOption
import java.nio.file.StandardCopyOption

object Main {
  def main(args: Array[String]) = new Mp3Tool(args).runCommand()
}

// https://github.com/mpatric/mp3agic

class Mp3Tool(args: collection.Seq[String]) extends Command(args) {
  banner("mp3处理程序")

  val changeTag = new Subcommand("changeTag") with InputDir {
    descr("修改同一目录下mp3文件的tag信息")

    val album = trailArg[String](descr = "专辑名")

    val artist = trailArg[String](descr = "艺人名")

    def execute(): Unit = {
      Files.list(Path.of(inputDir())).forEach { file =>
        if (file.getFileName().toString().toLowerCase().endsWith(".mp3")) {
          println(s"修改 $file")
          val mp3 = new Mp3File(file)
          val tag = if (mp3.hasId3v2Tag()) {
            mp3.getId3v2Tag()
          } else {
            val tag = new ID3v24Tag
            mp3.setId3v2Tag(tag)
            tag
          }
          tag.setAlbum(album())
          tag.setArtist(artist())
          tag.setAlbumArtist(artist())
          val tmp = Files.createTempFile("", ".mp3")
          mp3.save(tmp.toString())
          Files.move(tmp, file, StandardCopyOption.REPLACE_EXISTING)
        }
      }
    }
  }

  val moveFiles = new Subcommand("moveFiles") with InputDir {
    descr("将多个子目录下的文件移到根目录下")

    def execute(): Unit = {
      val rootPath = Paths.get(inputDir())

      def moveFilesInDir(sourceDir: Path, targetDir: Path): Unit = {
        Files.list(sourceDir).forEach { path =>
          if (Files.isDirectory(path)) {
            moveFilesInDir(path, targetDir)
          } else if (Files.isRegularFile(path)) {
            println(s"移动 $path 到 $targetDir")
            Files.move(
              path,
              targetDir.resolve(path.getFileName())
            )
          }
        }
      }

      Files.list(rootPath).forEach { path =>
        if (Files.isDirectory(path)) {
          moveFilesInDir(path, rootPath)
        }
      }
    }
  }

  val trimPrefix = new Subcommand("trimPrefix") with InputDir {
    descr("删除目录下所有文件的文件名前缀")

    val prefix = trailArg[String](descr = "前缀字符")

    def execute(): Unit = {
      val dir = Path.of(inputDir())

      Files.list(dir).forEach { path =>
        val fileName = path.getFileName().toString().stripPrefix(prefix())
        val target = dir.resolve(fileName)
        println(s"移动 $path 到 $target")
        Files.move(path, target)
      }
    }
  }

  val replaceSuffix = new Subcommand("replaceSuffix") with InputDir {
    descr("替换目录下所有文件的文件名后缀")

    val postfix = trailArg[String](descr = "后缀字符")

    val replace = trailArg[String](descr = "替换字符")

    def execute(): Unit = {
      println(s"${postfix()} => ${replace()}")
      import base.text.String.StringOps

      val dir = Path.of(inputDir())

      Files.list(dir).forEach { path =>
        val fileName = path
          .getFileName()
          .toString()
          .replace((postfix() + "$").r, replace())
        val target = dir.resolve(fileName)
        println(s"移动 $path 到 $target")
        Files.move(path, target)
      }
    }
  }

  addSubCommand(moveFiles)
  addSubCommand(changeTag)
  addSubcommand(trimPrefix)
  addSubcommand(replaceSuffix)
}
