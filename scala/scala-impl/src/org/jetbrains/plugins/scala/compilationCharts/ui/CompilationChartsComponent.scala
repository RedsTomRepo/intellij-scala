package org.jetbrains.plugins.scala.compilationCharts.ui

import com.intellij.openapi.project.Project
import com.intellij.ui.components.JBScrollPane
import com.intellij.util.ui.{JBUI, UIUtil}
import com.intellij.util.ui.UIUtil.{FontColor, FontSize}
import com.intellij.util.ui.components.BorderLayoutPanel
import javax.swing.{JViewport, ScrollPaneConstants}

class CompilationChartsComponent(project: Project)
  extends BorderLayoutPanel {

  import CompilationChartsComponent._

  locally {
    val panelColor = UIUtil.getBgFillColor(this)
    val diagramsComponent = new DiagramsComponent(
      project = project,
      panelColor = panelColor,
      textColor = TextColor,
      inactiveTextColor = InactiveTextColor,
      normalFont = NormalFont,
      smallFont = SmallFont,
      defaultZoom = TopPanel.defaultZoom
    )
    val diagramsScrollPane = new JBScrollPane(diagramsComponent)
    diagramsScrollPane.setHorizontalScrollBarPolicy(ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED)
    diagramsScrollPane.setVerticalScrollBarPolicy(ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER)
    diagramsScrollPane.setBorder(JBUI.Borders.empty)
    diagramsScrollPane.getViewport.setScrollMode(JViewport.SIMPLE_SCROLL_MODE)

    addToLeft(new LeftPanel(panelColor))
    addToRight(new RightPanel(panelColor))
    addToTop(new TopPanel(diagramsComponent.setZoom, panelColor))
    addToCenter(diagramsScrollPane)
  }
}

object CompilationChartsComponent {

  private final val TextColor = UIUtil.getLabelFontColor(FontColor.NORMAL)
  private final val InactiveTextColor = UIUtil.getInactiveTextColor
  private final val NormalFont = UIUtil.getLabelFont(FontSize.NORMAL)
  private final val SmallFont = UIUtil.getLabelFont(FontSize.SMALL)
}
