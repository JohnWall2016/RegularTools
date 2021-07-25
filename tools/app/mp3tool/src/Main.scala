import base.command._

object Main {
  def main(args: Array[String]) = new Mp3Tool(args).runCommand()
}

// https://github.com/mpatric/mp3agic

class Mp3Tool(args: collection.Seq[String]) extends Command(args) {
  banner("mp3处理程序")

  val changeTag = new Subcommand("changeTag") with InputDir {
    descr("修改tag信息")

    def execute(): Unit = {

    }
  }
}