package org.jetbrains.plugins.scala.compilationCharts.ui

import java.awt.Color

import com.intellij.icons.AllIcons
import com.intellij.openapi.actionSystem.{ActionManager, AnActionEvent, DefaultActionGroup, Presentation, Separator}
import com.intellij.openapi.actionSystem.ex.CustomComponentAction
import com.intellij.openapi.project.DumbAwareAction
import com.intellij.openapi.util.NlsActions
import com.intellij.ui.components.JBLabel
import com.intellij.util.ui.components.BorderLayoutPanel
import javax.swing.{Icon, JComponent}
import org.jetbrains.annotations.NotNull
import org.jetbrains.plugins.scala.compilationCharts.ui.DiagramsComponent.Zoom

import scala.concurrent.duration.DurationInt

class TopPanel(setZoom: Zoom => Unit, color: Color)
  extends BorderLayoutPanel {

  import TopPanel._

  private var currentZoomIndex: Int = DefaultZoomIndex

  setBackground(color)
  addToRight(createActionToolbar())

  private def createActionToolbar(): JComponent = {
    val actionGroup = new DefaultActionGroup
    val actions = Seq(LegendAction, new Separator, ResetZoomAction, ZoomOutAction, ZoomInAction)
    actions.foreach(actionGroup.add)
    val place = CompilationChartsComponent.getClass.getSimpleName
    val toolbar = ActionManager.getInstance.createActionToolbar(place, actionGroup, true)
    val component = toolbar.getComponent
    component.setBackground(color)
    component
  }

  private object LegendAction
    extends DumbAwareAction
      with CustomComponentAction {

    override def createCustomComponent(presentation: Presentation, place: String): JComponent = {
      new JBLabel("Legend here")
    }

    override def actionPerformed(e: AnActionEvent): Unit = ()
  }

  private abstract class BasicZoomAction(@NotNull @NlsActions.ActionText text: String,
                                         @NotNull @NlsActions.ActionDescription description: String,
                                         @NotNull icon: Icon)
    extends DumbAwareAction(text, description, icon) {

    final override def update(e: AnActionEvent): Unit =
      e.getPresentation.setEnabled(isEnabled)

    final override def actionPerformed(e: AnActionEvent): Unit = {
      currentZoomIndex = newZoomIndex
      val newZoom = AvailableZooms(currentZoomIndex)
      setZoom(newZoom)
    }

    protected def isEnabled: Boolean

    protected def newZoomIndex: Int
  }

  private object ResetZoomAction
    extends BasicZoomAction("reset zoom", "RESET ZOOM", AllIcons.General.ActualZoom) {

    override protected def isEnabled: Boolean = currentZoomIndex != DefaultZoomIndex
    override protected def newZoomIndex: Int = DefaultZoomIndex
  }

  private object ZoomOutAction
    extends BasicZoomAction("zoom out", "ZOOM OUT", AllIcons.General.ZoomOut) {

    override protected def isEnabled: Boolean = currentZoomIndex > 0
    override protected def newZoomIndex: Int = currentZoomIndex - 1
  }

  private object ZoomInAction
    extends BasicZoomAction("zoom in", "ZOOM IN", AllIcons.General.ZoomIn) {

    override protected def isEnabled: Boolean = currentZoomIndex < AvailableZooms.size - 1
    override protected def newZoomIndex: Int = currentZoomIndex + 1
  }
}

object TopPanel {

  private final val AvailableZooms = Seq(
    Zoom(1.minute, 6),
    Zoom(30.second, 4),
    Zoom(10.second, 6),
    Zoom(5.second, 6),
    Zoom(1.second, 5),
    Zoom(500.millis, 6),
    Zoom(250.millis, 4),
  ).sortBy(_.durationStep).reverse

  private final val DefaultZoomIndex = AvailableZooms.size / 2

  def defaultZoom: Zoom = AvailableZooms(DefaultZoomIndex)
}
