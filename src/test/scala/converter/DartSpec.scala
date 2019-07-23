package converter

import org.scalatest._

import scala.meta._

class DartSpec extends FlatSpec with Matchers {
  "Object" should "be exported in dart" in {
    val program =
      """
        |
        |case class Api(a:String, b:String)""".stripMargin
    val tree = program.parse[Source].get

    new Dart(Seq(tree)).export() shouldBe
      """import 'package:meta/meta.dart';
        |
        |class Api{
        |
        |  final String a;
        |  final String b;
        |
        |  Api({
        |    @required this.a,
        |    @required this.b,
        |  });
        |
        |  factory Api.fromJson(Map<String,dynamic> json) {
        |    return Api(
        |      a: json['a'],
        |      b: json['b'],
        |    );
        |  }
        |
        |}
      """.stripMargin.trim
  }

  "Object with option" should "be exported in dart" in {
    val program =
      """
        |
        |case class Api(a:String, b:Option[String])""".stripMargin
    val tree = program.parse[Source].get

    val export = new Dart(Seq(tree)).export()
    export shouldBe
      """import 'package:meta/meta.dart';
        |
        |class Api{
        |
        |  final String a;
        |  final String b;
        |
        |  Api({
        |    @required this.a,
        |    @required this.b,
        |  });
        |
        |  factory Api.fromJson(Map<String,dynamic> json) {
        |    return Api(
        |      a: json['a'],
        |      b: json['b'],
        |    );
        |  }
        |
        |}
      """.stripMargin.trim
  }

  "Object with jsvalue" should "be exported in dart" in {
    val program =
      """
        |
        |case class Api(a:String,b:JsValue)""".stripMargin
    val tree = program.parse[Source].get

    val export = new Dart(Seq(tree)).export()
    export shouldBe
      """import 'package:meta/meta.dart';
        |
        |class Api{
        |
        |  final String a;
        |  final Map<String,dynamic> b;
        |
        |  Api({
        |    @required this.a,
        |    @required this.b,
        |  });
        |
        |  factory Api.fromJson(Map<String,dynamic> json) {
        |    return Api(
        |      a: json['a'],
        |      b: json['b'],
        |    );
        |  }
        |
        |}
      """.stripMargin.trim
  }


  "Object with option list" should "be exported in dart" in {
    val program =
      """
        |
        |case class Api(a:String,b:Option[Seq[String]])""".stripMargin
    val tree = program.parse[Source].get

    val export = new Dart(Seq(tree)).export()
    println(export)
    export shouldBe
      """import 'package:meta/meta.dart';
        |
        |class Api{
        |
        |  final String a;
        |  final List<String> b;
        |
        |  Api({
        |    @required this.a,
        |    @required this.b,
        |  });
        |
        |  factory Api.fromJson(Map<String,dynamic> json) {
        |    return Api(
        |      a: json['a'],
        |      b: json['b'] != null ? (json['b'] as List).cast<String>() : null,
        |    );
        |  }
        |
        |}
      """.stripMargin.trim
  }

    "Object with double" should "be exported in dart" in {
      val program =
        """
          |
          |case class Api(a:String,b:Double)""".stripMargin
      val tree = program.parse[Source].get

      val export = new Dart(Seq(tree)).export()
      println(export)
      export shouldBe
        """import 'package:meta/meta.dart';
          |
          |class Api{
          |
          |  final String a;
          |  final double b;
          |
          |  Api({
          |    @required this.a,
          |    @required this.b,
          |  });
          |
          |  factory Api.fromJson(Map<String,dynamic> json) {
          |    return Api(
          |      a: json['a'],
          |      b: json['b'] != null ? ((json['b'] is int) ? (json['b'] as int).toDouble() : json['b']) : null,
          |    );
          |  }
          |
          |}
        """.stripMargin.trim
  }





  "Complex object" should "be exported in dart" in {
    val program = """
                    |case class Table(
                    |                id:String,
                    |                name:String,
                    |                columns: List[String],
                    |                rows: List[String]
                    |                  )
                    |
                    |case class UI(
                    |             tables: Seq[Table]
                    |)""".stripMargin

    val tree = program.parse[Source].get

    new Dart(Seq(tree)).export() shouldBe
      """import 'package:meta/meta.dart';
        |
        |class Table{
        |
        |  final String id;
        |  final String name;
        |  final List<String> columns;
        |  final List<String> rows;
        |
        |  Table({
        |    @required this.id,
        |    @required this.name,
        |    @required this.columns,
        |    @required this.rows,
        |  });
        |
        |  factory Table.fromJson(Map<String,dynamic> json) {
        |    return Table(
        |      id: json['id'],
        |      name: json['name'],
        |      columns: json['columns'] != null ? (json['columns'] as List).cast<String>() : null,
        |      rows: json['rows'] != null ? (json['rows'] as List).cast<String>() : null,
        |    );
        |  }
        |
        |}
        |
        |class UI{
        |
        |  final List<Table> tables;
        |
        |  UI({
        |    @required this.tables,
        |  });
        |
        |  factory UI.fromJson(Map<String,dynamic> json) {
        |    return UI(
        |      tables: json['tables'] != null ? (json['tables'] as List).map((x) => Table.fromJson(x)).toList() : null,
        |    );
        |  }
        |
        |}
      """.stripMargin.trim
  }

}