package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.Page.Response
import bon.jo.datamodeler.model.Page.*
import bon.jo.datamodeler.model.Page.given
import bon.jo.datamodeler.model.Page
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
class TestPage extends AnyFlatSpec with should.Matchers {
  "A page" can " be create" in {
      val rep1 : PageList[String]= Response(1,100,5,List("a"))
      val rep2 : PageList[String] = Response(1,100,5,List("b"))
      val rep3  : PageList[String]  = rep1 ++ rep2
      val p4 = rep3.flatMap{
        str => Response(str.size,0,0,str.toList)
      }
      p4 should be (Response(2,100,5,List('a','b')))
    }
}
