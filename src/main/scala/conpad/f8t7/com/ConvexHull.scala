package conpad.f8t7.com

import slinky.core.annotations.react //nodisplay
import slinky.core.Component //nodisplay
import slinky.core.FunctionalComponent //nodisplay
import slinky.core.facade.ReactElement //nodisplay
import slinky.web.html._ //nodisplay
import slinky.core.facade.Hooks._

import org.scalajs.dom.window._ //nodisplay
import org.scalajs.dom.html.Canvas
import slinky.core.facade.React
import org.scalajs.dom.raw.CanvasRenderingContext2D
import org.scalajs.dom.MouseEvent
import ConvexHullAlgorithm._
import scalajs.js.Dynamic
import scalajs.js

object ConvexHullAlgorithm {
  case class Point(x: Int, y: Int) {
    import scala.math.{pow, sqrt}
    def +(p: Point) = Point(x + p.x, y + p.y)
    def /(d: Int) = Point(x / d, y / d)
    def distance(p: Point): Double = sqrt(pow(p.x - x, 2.0) + pow(p.y - y, 2.0))
    def cos_angle(p: Point): Double = {
      val magnitude = distance(p)
      val cos = (p.x - x) / magnitude
      cos
    }
  }

  /*
  counter clock wise from $\vec(p0p1)$ to $\vec(p1p2)$ judgement
  using the determinant method;
   */
  def ccw(p0: Point, p1: Point, p2: Point): Boolean = {
    val r =
      p1.x * p2.y - p2.x * p1.y - p0.x * p2.y + p2.x * p0.y + p0.x * p1.y - p1.x * p0.y
    println(p0, p1, p2, s"r=${r}")
    if (r > 0) true else false
  }

  def grahamScan(ps: Seq[Point]): Seq[Point] = {
    if (ps.size < 3) {
      println("too few points,no need to compute")
      return ps
    }
    println("graham start:")
    println(ps)
    val ps1: List[Point] = ps.sortWith((a, b) => (a.y < b.y)).toList
    println("sort smallest y first")
    println(ps1)
    val t = ps1.head
    val pts_angle_sorted =
      ps1.tail.sortWith((p1, p2) => p1.cos_angle(t) < p2.cos_angle(t))
    println("angle sorted as:")
    println(
      pts_angle_sorted,
      pts_angle_sorted.map(p => " " + p.cos_angle(t)).mkString
    )

    var stk = List[Point](t)
    println("start with convex points: ", stk.toString())
    for (p2 <- pts_angle_sorted) {
      println("push point ", p2)
      stk = p2 +: stk //push stack
      println("stack now is ", stk)
      while (stk.size > 3 && !ccw(stk(2), stk(1), stk(0))) {
        println("drop inner point ", stk(1))
        stk = stk.head +: stk.drop(2)
        println("stack now is ", stk)
      }
    }
    println("convex points: ", stk.toString())
    stk
  }

}

@react class CanvasComponent extends Component {
  type Props = Unit
  case class State(points: Seq[Point], status: String, hull: Seq[Point])
  val myRef = React.createRef[Canvas]
  var down = false
  override def initialState =
    State(points = List.empty, status = "", hull = List.empty)
  override def componentDidMount() = { setupCanvas() }

  private def drawConvexHull(canvas: Canvas): Unit = {
    val renderer =
      canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    val hull = grahamScan(this.state.points)
    val raw = this.state.points
    setState(prev => State(prev.points, prev.status, hull = hull))

    renderer.clearRect(0, 0, canvas.width, canvas.height);
    renderer.fillStyle = "#f8f0f8"
    renderer.fillRect(0, 0, canvas.width, canvas.height)
    renderer.fillStyle = "green"
    // redraw raw points(green) and connections (black thin line)
    for (p <- raw) {
      renderer.beginPath()
      renderer.arc(p.x, p.y, 5, 0, 2 * Math.PI)
      renderer.closePath()
      renderer.fill()
    }
      renderer.beginPath()
    renderer.moveTo(raw.head.x, raw.head.y)
    for(p<-raw.tail){
      renderer.lineTo(p.x,p.y)
    }
    renderer.stroke()
    //redraw hull vertices(red) and polygon(yellow)
    renderer.fillStyle = "red"
    renderer.strokeStyle= "red"
    for (p <- hull) {
      renderer.beginPath()
      renderer.arc(p.x, p.y, 5, 0, 2 * Math.PI)
      renderer.closePath()
      renderer.fill()
    }
      renderer.beginPath()
    renderer.moveTo(hull.head.x, hull.head.y)
    for (p <- hull.tail) {
      renderer.lineTo(p.x, p.y)
    }
    renderer.closePath()
    renderer.stroke()
    renderer.fillStyle = "green"
    renderer.strokeStyle= "black"
  }

  private def handleMouseClick(e: MouseEvent, canvas: Canvas): Unit = {
    down = true
    val rect = canvas.getBoundingClientRect()
    val renderer =
      canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    if (e.button == 2) {
      drawConvexHull(canvas)
    }
    if (e.button != 0) return
    val posx = (e.clientX - rect.left).toInt;
    val posy = (e.clientY - rect.top).toInt;
    val status =
      s"button:${e.button} | clientX:${e.clientX},clientY:${e.clientY},rect.left:${rect.left},rect.top:${rect.top}"
    this.setState(preState =>
      State(
        status = status,
        points = preState.points.appended(Point(posx, posy)),
        hull = preState.hull
      )
    )

    renderer.fillStyle = "#00ff00"
    renderer.beginPath()
    renderer.arc(posx, posy, 5, 0, 2 * Math.PI)
    renderer.fill()
    renderer.fillText(posx.toString, posx - 6, posy - 6)

    if (this.state.points.size <= 1) return

    val previous_point = this.state.points.reverse(1)
    val last_point = this.state.points.last
    renderer.moveTo(previous_point.x, previous_point.y)
    renderer.lineTo(last_point.x, last_point.y)
    renderer.stroke()
  }

  def setupCanvas(): Unit = {
    val canvas = myRef.current
    val rect = canvas.getBoundingClientRect()
    val renderer =
      canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
    renderer.fillStyle = "#f8f0f8"
    renderer.fillRect(0, 0, canvas.width, canvas.height)
    renderer.fillStyle = "black"
    canvas.addEventListener(
      "mousedown",
      (e: MouseEvent) => { handleMouseClick(e, canvas); e.preventDefault() },
      false
    )
    canvas.addEventListener(
      "contextmenu",
      (e: MouseEvent) => { e.preventDefault() }
    )
    // canvas.onmouseup = (e: MouseEvent) => down = false
    canvas.onmousemove = { (e: MouseEvent) =>
      val status =
        s"button:${e.button} | clientX:${e.clientX},clientY:${e.clientY}"
      this.setState(prevState =>
        State(points = prevState.points, status = status, hull = prevState.hull)
      )

    }

  }

  override def render(): ReactElement = {
    div(
      // p("status:" + this.state.status),
      // Timer(),
      h1("ConvexHull, click to add point, right click to check"),
      div(
        canvas(ref := myRef, width := "800", height := "800")
      )
    )
  }
}
