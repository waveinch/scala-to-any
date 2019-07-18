package converter
import scala.meta._
import scala.util.Try

class Dart(sources: Seq[Source]) extends BaseConverter(sources) {

  override protected def typeMapping(t: String): String = t match {
    case "String" => "String"
    case "Boolean" => "bool"
    case "Unit" => "any"
    case "Int" | "Long" => "int"
    case "Double" => "double"
    case "Option" => ""
    case "Seq" | "List" => "List"
    case "JsValue" => "Map<String,dynamic>"
    case s:String if definedTypes().contains(s) => s
    case _=> {
      println(s"not found type: $t, known types are: ${definedTypes()}")
      "Object"
    }
  }

  override protected def filter(stat: Stat): Boolean = {
    stat match {
      case d:Defn.Def => true
      case obj:Defn.Object => false
      case cls:Defn.Class => true
      case v:Defn.Val => true
      case _ => false
    }
  }

  override protected def interfaceCode(name:String, body:String): String = {
    s"""
       |class $name{
       |$body
       |}
     """.stripMargin.trim
  }

  private def toType(typ: Type): String = {

    typ match {
      case t: Type.Function => {
        val params = t.params.map(toType(_)).zipWithIndex.map { case (p, i) => s"x$i:$p" }.mkString("(", ",", ")")
        s"$params => ${toType(t.res)}"
      }
      case t: Type.Name => typeMapping(t.value)
      case t: Type.Apply if toType(t.tpe) == "" => toType(t.args.head)
      case t: Type.Apply => toType(t.tpe) + "<" + t.args.map(toType(_)).mkString(",") + ">"
      case t: Type.Select => toType(t.name)
    }
  }



  private def generateFields(stat: Tree): Option[String] = {
    stat match {
      case v: Defn.Val if v.decltpe.isDefined => {
        val returnType = toType(v.decltpe.get)
        val name = v.pats.headOption match {
          case Some(p: Pat.Var) => p.name.value
          case _ => "noname";
        }
        Some(s"final $returnType $name;")
      }
      case t: Term.Param => {
        val returnType = toType(t.decltpe.get)
        Some(s"final $returnType ${t.name.value};")
      }
      case _ => None
    }
  }

  private def generateContructorFields(stat: Tree): Option[String] = Try{
    val name = stat match {
      case v: Defn.Val => {
        v.pats.headOption match {
          case Some(p: Pat.Var) => p.name.value
          case _ => "noname";
        }

      }
      case t: Term.Param => t.name.value
    }
    s"@required this.$name,"
  }.toOption

  private def jsonBaseType(t:String):Boolean = {
    t match {
      case "bool" | "String" | "int" | "double" => true
      case _ => false
    }
  }

  private def castToType(base:String, typ: Type): String = {

    typ match {
      case t: Type.Name => {
        val typ = typeMapping(t.value)
        if(jsonBaseType(typ) || typ == "Map<String,dynamic>") {
          base
        } else {
          s"$base != null ? $typ.fromJson($base) : null"
        }
      }
      case t: Type.Apply => {
        (jsonBaseType(toType(t.args.head)),toType(t.tpe)) match {
          case (_,"") => castToType(base,t.args.head); //option
          case (true,_) => s"$base != null ? ($base as ${toType(t.tpe)}).cast<${toType(t.args.head)}>() : null"
          case (false,_) => s"$base != null ? ($base as ${toType(t.tpe)}).map((x) => ${toType(t.args.head)}.fromJson(x)) : null"
        }
      }
      case t: Type.Select => castToType(base, t.name)
    }
  }

  private def generateJsonDeserializer(stat: Tree): Option[String] = {
    stat match {
      case t: Term.Param => {
        val name = t.name.value
        val base = s"json['$name']"
        Some(s"$name: ${castToType(base,t.decltpe.get)},")
      }
      case _ => None
    }

  }

  override protected def generateBody(name:String, stats: Seq[Tree]): String = {

    s"""
       |  ${stats.flatMap(generateFields).mkString("\n  ")}
       |
       |  $name({
       |    ${stats.flatMap(generateContructorFields).mkString("\n    ")}
       |  });
       |
       |  factory $name.fromJson(Map<String,dynamic> json) {
       |    return $name(
       |      ${stats.flatMap(generateJsonDeserializer).mkString("\n      ")}
       |    );
       |  }
       |""".stripMargin
  }
}
