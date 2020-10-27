package conpad.f8t7.com

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportTopLevel, JSImport}
import scala.scalajs.LinkingInfo

import slinky.core._
import slinky.web.ReactDOM
import slinky.hot

import org.scalajs.dom

@JSImport("resources/index.css", JSImport.Default)
@js.native
object IndexCSS extends js.Object

object Main {
  val css = IndexCSS

  @JSExportTopLevel("main")
  def main(): Unit = {
    if (LinkingInfo.developmentMode) {
      hot.initialize()
    }

    val container = Option(dom.document.getElementById("root")).getOrElse {
      val elem = dom.document.createElement("div")
      elem.id = "root"
      dom.document.body.appendChild(elem)
      elem
    }

    // ReactDOM.render(App(), container)
    // ReactDOM.render(ConvexHull(startFrom=50), container)
    // ReactDOM.render(ConvexHullClass(startFrom=50), container)
    ReactDOM.render(CanvasComponent(), container)
  }
}
