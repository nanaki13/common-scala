package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.Page.Response
import bon.jo.datamodeler.model.Page.*
import bon.jo.datamodeler.model.Page.given
import bon.jo.datamodeler.model.Page
import bon.jo.datamodeler.model.ForFun.*
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
class TestPage extends AnyFlatSpec with should.Matchers {
  "A page" can " be create" in {

      val rep1 : Page.Response[String]= Page.Response(List("a"),10,10)
      val rep2 : Page.Response[String] = Page.Response(List("b"),10,10)
      val rep3  : Page.Response[String]  = rep1 combine rep2
      val p4 = rep3.flatMap{
        str => Response(str.toList,rep3.pageNumber,rep3.pageCount)
      }
      p4 should be (Response(List('a','b'),0,0))

      val test = for (pe <- p4) yield pe
    }
}
