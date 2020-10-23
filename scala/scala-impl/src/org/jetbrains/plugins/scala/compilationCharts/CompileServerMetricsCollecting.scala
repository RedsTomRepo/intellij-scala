package org.jetbrains.plugins.scala.compilationCharts

import java.util.UUID
import java.util.concurrent.ScheduledExecutorService

import com.intellij.compiler.server.BuildManagerListener
import com.intellij.openapi.Disposable
import com.intellij.openapi.compiler.{CompileContext, CompileTask}
import com.intellij.openapi.components.{Service, ServiceManager}
import com.intellij.openapi.project.Project
import com.intellij.util.concurrency.AppExecutorUtil
import org.jetbrains.plugins.scala.compilationCharts.CompileServerMetricsCollector.{CollectDelay, TerminationTimeout}
import org.jetbrains.plugins.scala.compiler.CompileServerClient

import scala.concurrent.duration.DurationInt

class CompileServerMetricsCollecting
  extends CompileTask
    with BuildManagerListener {

  // BEFORE
  override def execute(context: CompileContext): Boolean = {
    val project = context.getProject
    CompileServerMetricsStateManager.reset(project)
    val collector = CompileServerMetricsCollector.get(project)
    collector.stopCollection()
    collector.startCollection()
    true
  }

  override def buildFinished(project: Project, sessionId: UUID, isAutomake: Boolean): Unit =
    CompileServerMetricsCollector.get(project).stopCollection()
}

@Service
private final class CompileServerMetricsCollector(project: Project)
  extends Disposable {

  @volatile private var optionScheduler: Option[ScheduledExecutorService] = None

  def startCollection(): Unit =
    startScheduler()

  def stopCollection(): Unit =
    stopScheduler()

  override def dispose(): Unit =
    stopScheduler()

  private def startScheduler(): Unit = synchronized {
    val schedulerName = "CompileServerMetricsCollector"
    val newScheduler = AppExecutorUtil.createBoundedScheduledExecutorService(schedulerName, 1)
    val compileServerClient = CompileServerClient.get(project)
    val runnable: Runnable = { () =>
      val metrics = compileServerClient.getMetrics()
      val timestamp = System.nanoTime()
      val state = CompileServerMetricsStateManager.get(project)
      val newState = state.copy(
        maxHeapSize = metrics.maxHeapSize,
        heapUsed = state.heapUsed.updated(timestamp, metrics.heapUsed)
      )
      CompileServerMetricsStateManager.update(project, newState)
    }
    newScheduler.scheduleWithFixedDelay(runnable, CollectDelay.length, CollectDelay.length, CollectDelay.unit)
    optionScheduler = Some(newScheduler)
  }

  private def stopScheduler(): Unit = synchronized {
    optionScheduler.foreach { scheduler =>
      scheduler.shutdownNow()
      scheduler.awaitTermination(TerminationTimeout.length, TerminationTimeout.unit)
    }
    optionScheduler = None
  }
}

private object CompileServerMetricsCollector {

  private final val TerminationTimeout = 3.seconds
  private final val CollectDelay = 3.seconds

  def get(project: Project): CompileServerMetricsCollector =
    ServiceManager.getService(project, classOf[CompileServerMetricsCollector])
}