package org.jetbrains.plugins.scala.compilationCharts

import java.awt.geom.Rectangle2D
import java.awt.{Color, Font, Graphics, Graphics2D, Point, Polygon, Shape}

import scala.annotation.tailrec
import scala.util.Using
import scala.util.Using.Releasable

// TODO better place for this utils?
package object ui {

  implicit object GraphicsReleasable extends Releasable[Graphics] {
    override def release(resource: Graphics): Unit = resource.dispose()
  }

  implicit class GraphicsExt(private val graphics: Graphics2D) extends AnyVal {

    /**
     * Width of the vertical and horizontal lines.
     */
    def linePixels: (Double, Double) = { // TODO better function?
      val transform = graphics.getTransform
      val xWidth = 1 / transform.getScaleX
      val yWidth = 1 / transform.getScaleY
      (xWidth, yWidth)
    }

    def doInClip[A](clip: Shape)
                   (action: Graphics2D => A): A =
      Using.resource(graphics.create().asInstanceOf[Graphics2D]) { clipGraphics =>
        clipGraphics.setClip(clip)
        action(clipGraphics)
      }

    def printPolygon(polygon: Polygon, color: Color): Unit = {
      graphics.setColor(color)
      graphics.fillPolygon(polygon)
    }

    def printRect(color: Color, border: Option[(Color, LineStroke)] = None): Unit = {
      val clipBounds = graphics.getClipBounds
      graphics.setColor(color)
      graphics.fillRect(clipBounds.x, clipBounds.y, clipBounds.width, clipBounds.height)
      border.foreach { case (borderColor, stroke) =>
        val oldStroke = graphics.getStroke
        graphics.setStroke(stroke.toStroke)
        graphics.setColor(borderColor)
        graphics.drawRect(clipBounds.x, clipBounds.y, clipBounds.width, clipBounds.height)
        graphics.setStroke(oldStroke)
      }
    }

    // TODO getTextRect + printText(text, color, font, x, y) pair is quite strange...
    def getTextRect(text: String, font: Font, align: TextAlign = TextAlign.Center): Rectangle2D = {
      graphics.setFont(font)
      val clipBounds = graphics.getClipBounds
      val fontMetrics = graphics.getFontMetrics(graphics.getFont)
      val stringBounds = fontMetrics.getStringBounds(text, graphics)
      val centeredDeltaX = align match {
        case TextAlign.Center => clipBounds.width / 2 - stringBounds.getBounds.width / 2
        case TextAlign.Left => 0
        case TextAlign.Right => clipBounds.width - stringBounds.getBounds.width
      }
      val x = clipBounds.x + centeredDeltaX
      val y = clipBounds.y + clipBounds.height / 2 + fontMetrics.getAscent * 2 / 5
      new Rectangle2D.Double(x, y, stringBounds.getWidth, stringBounds.getHeight)
    }

    def printText(text: String, color: Color, font: Font, align: TextAlign = TextAlign.Center): Unit = {
      val textRect = getTextRect(text, font, align)
      printText(text, color, font, textRect.getX, textRect.getY)
    }

    def printText(text: String, color: Color, font: Font, x: Double, y: Double): Unit = if (text.nonEmpty) {
      graphics.setColor(color)
      graphics.setFont(font)
      graphics.drawString(text, x.toFloat, y.toFloat)
    }

    def printReducedText(text: String, color: Color, font: Font, align: TextAlign): Rectangle2D = {
      val clipBounds = graphics.getClipBounds
      val fontMetrics = graphics.getFontMetrics(graphics.getFont)

      @tailrec
      def rec(fragment: String, isFull: Boolean): Rectangle2D = if (fragment.nonEmpty) {
        val toPrint = if (isFull) fragment else s"$fragment…"
        val stringBounds = fontMetrics.getStringBounds(toPrint, graphics)
        if (stringBounds.getWidth <= clipBounds.getWidth) {
          graphics.printText(toPrint, color, font, align)
          stringBounds
        } else {
          rec(fragment.init, isFull = false)
        }
      } else {
        new Rectangle2D.Double(clipBounds.x, clipBounds.y, 0.0, clipBounds.getHeight)
      }

      rec(text, isFull = true)
    }

    def printLine(x1: Int, y1: Int, x2: Int, y2: Int, color: Color, stroke: LineStroke): Unit = {
      val oldStroke = graphics.getStroke
      graphics.setStroke(stroke.toStroke)
      graphics.setColor(color)
      graphics.drawLine(x1, y1, x2, y2)
      graphics.setStroke(oldStroke)
    }

    def printVerticalLine(point: Point, length: Int, color: Color, stroke: LineStroke): Unit =
      printVerticalOrHorizontalLine(point, length, color, stroke, vertical = true)

    def printHorizontalLine(point: Point, length: Int, color: Color, stroke: LineStroke): Unit =
      printVerticalOrHorizontalLine(point, length, color, stroke, vertical = false)

    private def printVerticalOrHorizontalLine(point: Point,
                                              length: Int,
                                              color: Color,
                                              stroke: LineStroke,
                                              vertical: Boolean): Unit = {
      val x1 = point.x
      val y1 = point.y
      val x2 = if (vertical) x1 else x1 + length
      val y2 = if (vertical) y1 + length else y1

      printLine(x1, y1, x2, y2, color, stroke)
    }
  }

  // TODO maybe add functions like withFont, withColor, withStroke, withClip?
}
