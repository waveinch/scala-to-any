
import java.io.File

import scopt.OParser
import Exporters.Exporters
import converter.{Dart, Typescript}

import scala.meta._


object Exporters extends Enumeration {
  type Exporters = Value
  val Typescript, Dart = Value
}


case class Config(
                   exporter: Exporters = Exporters.Dart,
                   source: File = new File("."),
                   out: File = new File(".")
                 )

object Converter extends App {
  implicit val exportersRead: scopt.Read[Exporters.Value] = scopt.Read.reads(Exporters withName _)

  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("scala-to"),
      head("scala-to", "0.1"),
      // option -f, --foo
      opt[File]('s',"source").required()
        .action((x, c) => c.copy(source = x))
        .text("source scala file"),
      opt[File]('o',"out").required()
        .action((x, c) => c.copy(out = x))
        .text("output file"),
      opt[Exporters]('t',"type").required()
        .action((x,c) => c.copy(exporter = x))
        .text("Export type: Typescript | Dart")
      // more options here...
    )
  }

  // OParser.parse returns Option[Config]
  OParser.parse(parser1, args, Config()) match {
    case Some(config) => {
      val ast = config.source.parse[Source].get
      val converter = config.exporter match {
        case Exporters.Typescript => new Typescript(Seq(ast))
        case Exporters.Dart => new Dart(Seq(ast))
      }

      reflect.io.File(config.out).writeAll(converter.export())
    }
    case _ =>
      println("Error parsing parameters")
  }


}
