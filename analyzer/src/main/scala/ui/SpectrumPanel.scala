package ui

import domain.{AnalysisPipeline, BandSnapshot, Beat}

import java.awt.{Color, Dimension, Graphics, Graphics2D, RenderingHints, BasicStroke, Font}
import javax.swing.JPanel

class SpectrumPanel extends JPanel:

  private var outcome: Option[AnalysisPipeline.Outcome] = None

  setPreferredSize(new Dimension(900, 540))
  setBackground(new Color(0x16, 0x18, 0x1d))

  /** Установить новые данные и перерисоваться. Вызывается из EDT. */
  def show(o: AnalysisPipeline.Outcome): Unit =
    outcome = Some(o)
    repaint()

  override def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

    outcome match
      case None    => drawPlaceholder(g2)
      case Some(o) => drawOutcome(g2, o)

  private def drawPlaceholder(g2: Graphics2D): Unit =
    g2.setColor(new Color(0x6b, 0x70, 0x7b))
    g2.setFont(new Font("SansSerif", Font.PLAIN, 16))
    g2.drawString("Откройте WAV-файл для анализа (кнопка сверху)", 40, getHeight / 2)

  private def drawOutcome(g2: Graphics2D, o: AnalysisPipeline.Outcome): Unit =
    val w = getWidth
    val h = getHeight
    val marginL = 50
    val marginR = 20
    val plotW = w - marginL - marginR

    val chartTop = 20
    val chartH = (h * 0.55).toInt
    val beatTop = chartTop + chartH + 30
    val beatH = 40

    drawEnergyChart(g2, o.bands, marginL, chartTop, plotW, chartH)
    drawBeatTimeline(g2, o.result.beats, o.result.header.durationSeconds,
                     marginL, beatTop, plotW, beatH)
    drawSummary(g2, o, marginL, beatTop + beatH + 30)

  /** График энергии по диапазонам — три линии поверх сетки. */
  private def drawEnergyChart(
      g2: Graphics2D,
      bands: Vector[BandSnapshot],
      x: Int, y: Int, w: Int, h: Int
  ): Unit =
    g2.setColor(new Color(0x1e, 0x21, 0x28))
    g2.fillRect(x, y, w, h)
    g2.setColor(new Color(0x2c, 0x30, 0x3a))
    for i <- 0 to 4 do
      val gy = y + h * i / 4
      g2.drawLine(x, gy, x + w, gy)

    if bands.isEmpty then return

    val maxE = bands.flatMap(b => List(b.bass, b.mid, b.high)).maxOption.getOrElse(1.0).max(1e-9)

    def drawSeries(pick: BandSnapshot => Double, color: Color): Unit =
      g2.setColor(color)
      g2.setStroke(new BasicStroke(1.6f))
      var prevX = -1
      var prevY = -1
      val n = bands.length
      for i <- bands.indices do
        val px = x + (w.toLong * i / math.max(1, n - 1)).toInt
        val norm = math.sqrt(pick(bands(i)) / maxE)
        val py = y + h - (norm * h).toInt
        if prevX >= 0 then g2.drawLine(prevX, prevY, px, py)
        prevX = px
        prevY = py

    drawSeries(_.bass, new Color(0xe0, 0x5a, 0x4f))
    drawSeries(_.mid,  new Color(0xe0, 0xb0, 0x4f))
    drawSeries(_.high, new Color(0x4f, 0xc0, 0xe0))

    g2.setFont(new Font("SansSerif", Font.PLAIN, 12))
    g2.setColor(new Color(0xe0, 0x5a, 0x4f)); g2.drawString("бас", x + 8, y + 16)
    g2.setColor(new Color(0xe0, 0xb0, 0x4f)); g2.drawString("средние", x + 48, y + 16)
    g2.setColor(new Color(0x4f, 0xc0, 0xe0)); g2.drawString("высокие", x + 120, y + 16)

  /** Таймлайн битов — вертикальные штрихи по времени трека. */
  private def drawBeatTimeline(
      g2: Graphics2D,
      beats: Vector[Beat],
      durationSec: Double,
      x: Int, y: Int, w: Int, h: Int
  ): Unit =
    g2.setColor(new Color(0x1e, 0x21, 0x28))
    g2.fillRect(x, y, w, h)
    g2.setColor(new Color(0x9a, 0xa0, 0xac))
    g2.setFont(new Font("SansSerif", Font.PLAIN, 12))
    g2.drawString(s"биты (${beats.length})", x, y - 6)

    if durationSec <= 0 then return
    g2.setColor(new Color(0x5a, 0xd0, 0x8a))
    g2.setStroke(new BasicStroke(1.0f))
    for b <- beats do
      val px = x + (w * (b.timeSeconds / durationSec)).toInt
      g2.drawLine(px, y, px, y + h)

  /** Текстовая сводка под графиками. */
  private def drawSummary(
      g2: Graphics2D,
      o: AnalysisPipeline.Outcome,
      x: Int, y: Int
  ): Unit =
    val s = o.summary
    val hdr = o.result.header
    g2.setFont(new Font("Monospaced", Font.PLAIN, 13))
    g2.setColor(new Color(0xd6, 0xda, 0xe2))
    val lines = Vector(
      f"BPM: ${o.result.bpm}%.1f      битов: ${o.result.beats.length}      кадров FFT: ${o.result.frames.length}",
      f"длительность: ${hdr.durationSeconds}%.2f с    sample rate: ${hdr.sampleRate} Hz    каналов: ${hdr.channels}",
      f"баланс — бас: ${s.avgBass}%.3f   средние: ${s.avgMid}%.3f   высокие: ${s.avgHigh}%.3f",
      s"жанровая характеристика: ${domain.Report.classify(s)}"
    )
    lines.zipWithIndex.foreach { (line, i) =>
      g2.drawString(line, x, y + i * 20)
    }
