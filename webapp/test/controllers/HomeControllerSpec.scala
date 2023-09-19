package controllers

import org.clulab.processors.webapp.controllers.HomeController
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.libs.json._
import play.api.test._
import play.api.test.Helpers._

/**
 * Add your spec here.
 * You can mock out a whole application including requests, plugins etc.
 *
 * For more information, see https://www.playframework.com/documentation/latest/ScalaTestingWithScalaTest
 */
class HomeControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting {
  val homeContent = "processors visualizer"
  val fakeRequest = FakeRequest(GET, "/")

  "HomeController GET" must {

    "render the index page from the application" in {
      val controller = inject[HomeController]
      val response = controller.index().apply(fakeRequest)

      status(response) mustBe OK
      contentType(response) mustBe Some("text/html")
      contentAsString(response) must include(homeContent)
    }

    "render the parse from the application" in {
      val text = "John eats cake."
      val controller = inject[HomeController]
      val json = Json.parse(s"""{ "text": "$text" }""")
      val request = FakeRequest(GET, "/parseText").withJsonBody(json)
      val response = controller.parseText(text).apply(request)

      status(response) mustBe OK
      contentType(response) mustBe Some("application/json")
      contentAsString(response) must include(text)
    }
  }
}
