package org.jetbrains.plugins.scala.compilationCharts.ui

import java.awt.geom.Rectangle2D
import java.awt.{Color, Dimension, Font, Graphics, Graphics2D, Point, Polygon, Rectangle}
import java.math.{MathContext, RoundingMode}
import java.text.NumberFormat

import com.intellij.openapi.project.Project
import com.intellij.ui.components.JBPanelWithEmptyText
import com.intellij.ui.table.JBTable
import com.intellij.util.ui.UIUtil
import org.jetbrains.plugins.scala.compilationCharts.ui.DiagramsComponent.Zoom
import org.jetbrains.plugins.scala.compilationCharts.{CompilationProgressInfo, CompilationProgressState, CompilationProgressStateManager, CompileServerMemoryState, CompileServerMetricsStateManager, Memory, Timestamp}
import org.jetbrains.plugins.scala.compiler.{CompilationUnitId, ScalaCompileServerSettings}

import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, DurationLong, FiniteDuration}

class DiagramsComponent(project: Project,
                        panelColor: Color,
                        textColor: Color,
                        inactiveTextColor: Color,
                        normalFont: Font,
                        smallFont: Font,
                        defaultZoom: Zoom)
  extends JBPanelWithEmptyText {

  import DiagramsComponent._

  private var currentZoom = defaultZoom

  def setZoom(zoom: Zoom): Unit = {
    currentZoom = zoom
    CompilationChartsToolWindowFactory.refresh(project)
  }

  override def paintComponent(g: Graphics): Unit = {
    val graphics = g.asInstanceOf[Graphics2D]
    val clipBounds = g.getClipBounds

    val progressState = CompilationProgressStateManager.get(project)
    val metricsState = CompileServerMetricsStateManager.get(project)

    val settings = ScalaCompileServerSettings.getInstance
    val parallelism = if (settings.COMPILE_SERVER_PARALLEL_COMPILATION) settings.COMPILE_SERVER_PARALLELISM else 1 // TODO

    calculateDiagrams(progressState, metricsState) match {
      case Some(Diagrams(progressDiagram, memoryDiagram, progressTime)) =>
        val preferredWidth = currentZoom.toPixels(progressTime + 10.seconds) // TODO

        val progressDiagramClip = new Rectangle(
          clipBounds.x,
          clipBounds.y,
          clipBounds.width,
          parallelism * RowHeight
        )
        graphics.doInClip(progressDiagramClip)(printProgressDiagram(_, progressDiagram, progressTime, preferredWidth))

        val delimiterClip = new Rectangle(
          clipBounds.x,
          progressDiagramClip.y + progressDiagramClip.height,
          clipBounds.width,
          DelimiterHeight
        )
        graphics.doInClip(delimiterClip)(printDelimiter)

        val memoryDiagramClip = new Rectangle(
          clipBounds.x,
          delimiterClip.y + delimiterClip.height,
          clipBounds.width,
          MemoryDiagramHeight
        )
        graphics.doInClip(memoryDiagramClip)(printMemoryDiagram(_, memoryDiagram, preferredWidth, progressTime))

        val durationAxisClip = new Rectangle(
          clipBounds.x,
          memoryDiagramClip.y + memoryDiagramClip.height,
          clipBounds.width,
          DurationAxisHeight
        )
        graphics.doInClip(durationAxisClip)(printDurationAxis(_, preferredWidth))

        setPreferredSize(new Dimension(preferredWidth, clipBounds.height))
        revalidate()
      case None =>
        super.paintComponent(graphics)
    }
  }

  private def printProgressDiagram(graphics: Graphics2D,
                                   progressDiagram: ProgressDiagram,
                                   progressTime: FiniteDuration,
                                   preferredWidth: Int): Unit = {
    val clipBounds = graphics.getClipBounds
    val borderThickness = NormalStroke.thickness

    def printSegment(segment: Segment, row: Int): Unit = {
      val Segment(CompilationUnitId(moduleName, testScope), from, to, progress) = segment
      val segmentClip = new Rectangle2D.Double(
        currentZoom.toPixels(from),
        clipBounds.height - RowHeight * row + borderThickness,
        currentZoom.toPixels(to - from) - borderThickness,
        RowHeight - borderThickness
      )
      val color = if (testScope) TestModuleColor else ProdModuleColor
      graphics.doInClip(segmentClip) { segmentGraphics =>
        segmentGraphics.printRect(color)
        segmentGraphics.printReducedText(s" $moduleName", textColor, normalFont, TextAlign.Left)
      }
      if (progress < 1.0) {
        val textClipX = segmentClip.x + segmentClip.width + borderThickness
        val textClip = new Rectangle2D.Double(
          textClipX,
          segmentClip.y,
          preferredWidth - textClipX,
          segmentClip.height
        )
        val text = s" ${(progress * 100).round}%"
        graphics.doInClip(textClip)(_.printText(text, textColor, normalFont, TextAlign.Left))
      }
    }

    printDiagramBasics(graphics, preferredWidth, progressTime)
    progressDiagram.segmentGroups.zipWithIndex.foreach { case (group, row) =>
      group.foreach(printSegment(_, row + 1))
    }
  }

  private def printDelimiter(graphics: Graphics2D): Unit =
    graphics.printRect(panelColor)

  private def printMemoryDiagram(graphics: Graphics2D,
                                 memoryDiagram: MemoryDiagram,
                                 preferredWidth: Int,
                                 progressTime: FiniteDuration): Unit = {
    printDiagramBasics(graphics, preferredWidth, progressTime)
    val clipBounds = graphics.getClipBounds
    val MemoryDiagram(points, maxMemory) = memoryDiagram

    def getExtraPoints(edgePoint: Option[MemoryPoint],
                       extraPointTime: FiniteDuration,
                       firstPoint: Boolean): Seq[MemoryPoint] = edgePoint match {
      case Some(MemoryPoint(`extraPointTime`, 0)) =>
        Seq.empty
      case Some(MemoryPoint(`extraPointTime`, _)) | None =>
        Seq(MemoryPoint(extraPointTime, 0L))
      case Some(point) =>
        val extraPoints = Seq(point.copy(time = extraPointTime), MemoryPoint(extraPointTime, 0L))
        if (firstPoint) extraPoints.reverse else extraPoints
    }

    def toPlotPoint(memoryPoint: MemoryPoint): (Int, Int) = {
      val MemoryPoint(time, memory) = memoryPoint
      val x = currentZoom.toPixels(time)
      val y = clipBounds.y + clipBounds.height - (memory.toDouble / maxMemory * clipBounds.height).round.toInt
      (x, y)
    }

    val leftExtraPoints = getExtraPoints(points.headOption, Duration.Zero, firstPoint = true)
    val rightExtraPoints = getExtraPoints(points.lastOption, progressTime, firstPoint = false)
    val allPoints = leftExtraPoints ++ points ++ rightExtraPoints
    val plotPoints = allPoints.map(toPlotPoint)
    val (xPolygonValues, yPolygonValues) = plotPoints.unzip
    val polygon = new Polygon(xPolygonValues.toArray, yPolygonValues.toArray, plotPoints.length)
    graphics.printPolygon(polygon, MemoryFillColor)

    if (plotPoints.size >= 2)
      plotPoints.sliding(2).foreach { case Seq((x1, y1), (x2, y2)) =>
        if (x1 != x2)
          graphics.printLine(x1, y1, x2, y2, MemoryLineColor, NormalStroke)
      }
    points.lastOption.foreach { point =>
      val x = currentZoom.toPixels(progressTime)
      val (_, y) = toPlotPoint(point)
      val clip = new Rectangle(x, y - clipBounds.height, clipBounds.width, 2 * clipBounds.height)
      val text = " " + stringify(point.memory)
      graphics.doInClip(clip)(_.printText(text, textColor, normalFont, TextAlign.Left))
    }
  }

  private def printDurationAxis(graphics: Graphics2D, preferredWidth: Int): Unit = { // TODO
    val clipBounds = graphics.getClipBounds

    graphics.printRect(panelColor)
    durationXIterator(clipBounds.width, preferredWidth).zipWithIndex.foreach { case (x, i) =>
      val point = new Point(x, clipBounds.y)
      graphics.printVerticalLine(point, DashLength, inactiveTextColor, NormalStroke)
      if (i % currentZoom.durationLabelPeriod == 0) {
        val text = " " + stringify(i * currentZoom.durationStep)
        val textClip = new Rectangle(point.x, clipBounds.y, clipBounds.width, clipBounds.height)
        val textRect = graphics.doInClip(textClip)(_.getTextRect(text, smallFont, TextAlign.Left))
        graphics.printText(text, textColor, smallFont, point.x, point.y + textRect.getHeight)
      }
    }
  }

  private def printDiagramBasics(graphics: Graphics2D, preferredWidth: Int, progressTime: FiniteDuration): Unit = {
    graphics.printRect(BackgroundColor, border = Some((inactiveTextColor, NormalStroke)))
    val clipBounds = graphics.getClipBounds
    durationXIterator(clipBounds.width, preferredWidth).zipWithIndex.foreach { case (x, i) =>
      if (i % currentZoom.durationLabelPeriod == 0) {
        val point = new Point(x, clipBounds.y)
        graphics.printVerticalLine(point, clipBounds.height, inactiveTextColor, DashedStroke)
      }
    }
    val progressLinePoint = currentZoom.toPixels(progressTime)
    val linePoint = new Point(progressLinePoint, clipBounds.y)
    graphics.printVerticalLine(linePoint, clipBounds.height, textColor, ThickStroke)
  }

  private def durationXIterator(clipBoundsWidth: Int, preferredWidth: Int): Range =
    Range(0, clipBoundsWidth max preferredWidth, currentZoom.stepPixels)
}

object DiagramsComponent {

  final case class Zoom(durationStep: FiniteDuration,
                        durationLabelPeriod: Int) {

    private lazy val scale = 5.5e7 / durationStep.toNanos

    def toPixels(duration: FiniteDuration): Int = (scale * duration.toMillis).round.toInt

    def stepPixels: Int = toPixels(durationStep)
  }

  private final val TestModuleColor = new Color(98, 181, 67, 127)
  private final val ProdModuleColor = new Color(64, 182, 224, 127)
  private final val MemoryFillColor = new Color(231, 45, 45, 13)
  private final val MemoryLineColor = new Color(231, 45, 45)
  private final val BackgroundColor = UIUtil.getTreeBackground

  private final val RowHeight = (new JBTable().getRowHeight * 1.5).round.toInt

  private final val MemoryDiagramHeight = RowHeight * 4
  private final val DelimiterHeight = RowHeight
  private final val DurationAxisHeight = RowHeight

  private final val DashLength = RowHeight / 4

  private final val NormalStroke = new LineStroke(1.0F)
  private final val ThickStroke = new LineStroke(0.5F)
  private final val DashedStroke = new LineStroke(0.5F, dashLength = Some((RowHeight / 5).toFloat))

  private final case class Diagrams(progressDiagram: ProgressDiagram,
                                    memoryDiagram: MemoryDiagram,
                                    progressTime: FiniteDuration)

  private final case class ProgressDiagram(segmentGroups: Seq[Seq[Segment]])

  private final case class Segment(unitId: CompilationUnitId,
                                   from: FiniteDuration,
                                   to: FiniteDuration,
                                   progress: Double)

  private final case class MemoryDiagram(points: Seq[MemoryPoint], maxMemory: Memory)

  private final case class MemoryPoint(time: FiniteDuration, memory: Memory)

  private def calculateDiagrams(progressState: CompilationProgressState,
                                metricsState: CompileServerMemoryState): Option[Diagrams] =
    for {
      (minTimestamp, maxTimestamp) <- getMinMaxTimestamps(progressState)
      progressDiagram = calculateProgressDiagram(progressState, minTimestamp, maxTimestamp)
      progressTime <- progressDiagram.segmentGroups.flatten.map(_.to).maxOption
      memoryDiagram = calculateMemoryDiagram(metricsState, minTimestamp, maxTimestamp, progressTime)
    } yield Diagrams(
      progressDiagram = progressDiagram,
      memoryDiagram = memoryDiagram,
      progressTime = progressTime
    )

  private def getMinMaxTimestamps(progressState: CompilationProgressState): Option[(Timestamp, Timestamp)] = {
    val timestamps = progressState.values.flatMap { info =>
      Seq(info.startTime, info.updateTime) ++ info.finishTime.toSeq
    }
    for {
      min <- timestamps.minOption
      max <- timestamps.maxOption
    } yield (min, max)
  }

  private def calculateProgressDiagram(progressState: CompilationProgressState,
                                       minTimestamp: Timestamp,
                                       maxTimestamp: Timestamp): ProgressDiagram = {
    def groupSegments(segments: Seq[Segment]): Seq[Seq[Segment]] = {
      @tailrec
      def rec(groups: Seq[Seq[Segment]],
              segments: Seq[Segment]): Seq[Seq[Segment]] = segments match {
        case Seq() => groups
        case Seq(interval, remainIntervals@_*) => rec(insert(groups, interval), remainIntervals)
      }

      def insert(groups: Seq[Seq[Segment]],
                 segment: Segment): Seq[Seq[Segment]] = groups match {
        case Seq() =>
          Seq(Seq(segment))
        case Seq(group, remainGroups@_*) =>
          if (group.last.to < segment.from)
            (group :+ segment) +: remainGroups
          else
            group +: insert(remainGroups, segment)
      }

      rec(Seq.empty, segments)
    }

    val sortedState = progressState.toSeq.sortBy(_._2.startTime)
    val segments = sortedState.flatMap { case (unitId, CompilationProgressInfo(startTime, finishTime, _, progress)) =>
      val from = (startTime - minTimestamp).nanos
      val to = (finishTime.getOrElse(maxTimestamp) - minTimestamp).nanos
      if (from.length >= 0 && to.length >= 0)
        Some(Segment(
          unitId = unitId,
          from = from,
          to = to,
          progress = progress
        ))
      else
        None
    }
    ProgressDiagram(groupSegments(segments))
  }

  private def calculateMemoryDiagram(metricsState: CompileServerMemoryState,
                                     minTimestamp: Timestamp,
                                     maxTimestamp: Timestamp,
                                     progressTime: FiniteDuration): MemoryDiagram = {
    val points = metricsState.heapUsed.map { case (timestamp, memory) =>
      val fixedTimestamp = if (timestamp < minTimestamp)
        minTimestamp
      else if (timestamp > maxTimestamp)
        maxTimestamp
      else
        timestamp
      fixedTimestamp -> memory
    }.map { case (timestamp, memory) =>
      val time = (timestamp - minTimestamp).nanos
      MemoryPoint(time, memory)
    }.toSeq.sortBy(_.time)

    val maxMemory = metricsState.maxHeapSize
    MemoryDiagram(points, maxMemory)
  }

  private def stringify(duration: FiniteDuration): String = {
    val minutes = duration.toMinutes
    val seconds = duration.toSeconds % 60
    val minutesStr = Option(minutes).filter(_ > 0).map(_.toString + "m")
    val secondsStr = Option(seconds).filter(_ > 0).map(_.toString + "s")
    val result = Seq(minutesStr, secondsStr).flatten.mkString(" ")
    if (result.nonEmpty) result else "0s"
  }

  private def stringify(bytes: Memory): String = {
    val gigabytes = bytes.toDouble / 1024 / 1024 / 1024
    s"${MemoryFormatter.format(gigabytes)} GB"
  }

  private val MemoryFormatter = {
    val formatter = NumberFormat.getNumberInstance
    formatter.setMaximumFractionDigits(2)
    formatter
  }
}
