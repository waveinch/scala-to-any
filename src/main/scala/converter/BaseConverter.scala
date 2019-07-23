package converter

import scala.meta._

abstract class BaseConverter(sources: Seq[Source]) {


  protected def definedTypes(): Seq[String] = {
    sources.flatMap(skipPackage).filter {
      case cls: Defn.Class => filter(cls)
      case _ => false
    }.map { case cls: Defn.Class => cls.name.value }
  }

  protected def typeMapping(t: String): String


  protected def filter(stat: Stat): Boolean


  private def generateInterface(stat: Stat): String = {
    val name: String = stat match {
      case o: Defn.Object => o.name.value
      case o: Defn.Class => o.name.value
    }


    val termsToExport: Seq[Tree] = stat match {
      case o: Defn.Object => o.templ.stats.filter(filter)
      case o: Defn.Class => {

        o.ctor.paramss.flatten ++
          o.templ.stats.filter(filter)
      }
    }

    interfaceCode(name,generateBody(name, termsToExport))

  }

  protected def interfaceCode(name:String, terms:String): String


  protected def generateBody(name:String, stats: Seq[Tree]): String




  private def skipPackage(s: Source): Seq[Stat] = {
    s.stats.headOption match {
      case Some(p: Pkg) => p.stats
      case _ => s.stats
    }
  }

  protected def head:String = ""

  protected def generateExtra(stat:Stat):String = ""

  def export(): String = {
    val classes = sources.flatMap(skipPackage).filter(filter).map { stat =>
      s"""${generateInterface(stat)}
         |
         |${generateExtra(stat)}
         |
       """.stripMargin.trim
    }.mkString("\n\n")

    s"""$head
       |
       |$classes""".stripMargin.trim

  }


}