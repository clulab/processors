package controllers

import org.clulab.wm.eidoscommon.utils.Resourcer
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 *
 * For more information, see https://www.playframework.com/documentation/latest/ScalaTestingWithScalaTest
 */
class HomeControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting {

  "HomeController GET" should {

    "render the index page from a new instance of controller" in {
      val controller = new HomeController(stubControllerComponents())
      val home = controller.index().apply(FakeRequest(GET, "/"))

      status(home) mustBe OK
      contentType(home) mustBe Some("text/html")
      contentAsString(home) must include ("World Modelers Visualizer")
    }

    "render the index page from the application" in {
      val controller = inject[HomeController]
      val home = controller.index().apply(FakeRequest(GET, "/"))

      status(home) mustBe OK
      contentType(home) mustBe Some("text/html")
      contentAsString(home) must include ("World Modelers Visualizer")
    }

    "render the index page from the router" in {
      val request = FakeRequest(GET, "/")
      val home = route(app, request).get

      status(home) mustBe OK
      contentType(home) mustBe Some("text/html")
      contentAsString(home) must include ("World Modelers Visualizer")
    }
  }

  "HomeController POST" should {
    "accept request with text parameter and return JSON" in {

      // Note that the request fails because the JSON does not have key 'text' but instead has key 'text123'
      // This is because testing an actual run requires initialization which takes too long

      val testJson = Json.parse("""{ "text123": "Drought causes regional instability." }""")
      val request = FakeRequest(POST, "/process_text").withJsonBody(testJson)
      val result = route(app, request).get

      contentAsString(result) must include ("Missing parameter [text]")
    }

    "be able to reground" in {
      val name = "test"
      // This was simply chosen because it is the smallest.
      val ontologyYaml = Resourcer.getText("/org/clulab/wm/eidos/english/ontologies/un_properties.yml")
      val texts = Array(
        "Rainfall in the summer causes good crop yields in the fall.",
        "This is another text that should be grounded."
      )
      val filter = true
      val topk = 5
      val isAlreadyCanonicalized = false
      val regroundRequest = JsObject { Map(
        "name" -> JsString(name),
        "ontologyYaml" -> JsString(ontologyYaml),
        "texts" -> JsArray(texts.map(JsString)),
        "filter" -> JsBoolean(filter),
        "topk" -> JsNumber(topk),
        "isAlreadyCanonicalized" -> JsBoolean(isAlreadyCanonicalized)
      ) }
      val request = FakeRequest(POST, "/reground").withJsonBody(regroundRequest)
      val regroundResponse = contentAsJson(route(app, request).get)

      val outerJsArray = regroundResponse.as[JsArray]
      outerJsArray.value.size must be (texts.length)

      outerJsArray.value.foreach { jsValue: JsValue =>
        val innerJsArray = jsValue.as[JsArray]
        innerJsArray.value.size must be (topk)

        innerJsArray.value.foreach { jsValue =>
          val jsObject = jsValue.as[JsObject]
          val grounding = (jsObject \ "grounding").as[String]
          val score = (jsObject \ "score").as[Double]

          grounding.nonEmpty mustBe (true)
          score > 0 mustBe (true)
        }
      }
    }
  }
}
