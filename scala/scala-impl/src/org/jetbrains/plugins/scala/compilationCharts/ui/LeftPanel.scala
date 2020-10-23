package org.jetbrains.plugins.scala.compilationCharts.ui

import java.awt.Color

import com.intellij.ui.components.{JBLabel, JBPanel}

class LeftPanel(color: Color)
  extends JBPanel {

  setBackground(color)
  add(new JBLabel("Test"))
}
