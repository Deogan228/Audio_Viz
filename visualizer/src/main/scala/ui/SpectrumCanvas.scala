package ui

import domain.{Renderer, VisualMode}

import java.awt.{Color, Dimension, Graphics, Graphics2D, RenderingHints, BasicStroke, Font}
import javax.swing.{JPanel, SwingUtilities}

// Холст, на котором рисуются наши красивые столбики
// Получает готовые кадры и перерисовывается — никакой логики синхронизации здесь нет
class SpectrumCanvas extends JPanel:

  @volatile private var frame: Option[Renderer.Frame] = None

  setPreferredSize(new Dimension(820, 460))
  setBackground(new Color(0x12, 0x14, 0x18))

  /** Принять кадр от ZIO-цикла и запланировать перерисовку на Swing-потоке */
  def showFrame(f: Renderer.Frame): Unit =
    frame = Some(f)
    SwingUtilities.invokeLater(() => repaint())

  override def paintComponent(g: Graphics): Unit =
    super.paintComponent(g)
    val g2 = g.asInstanceOf[Graphics2D]
    g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    frame match
      case None    => drawIdle(g2)
      case Some(f) => drawFrame(g2, f)

  private def drawIdle(g2: Graphics2D): Unit =
    g2.setColor(new Color(0x6b, 0x70, 0x7b))
    g2.setFont(new Font("SansSerif", Font.PLAIN, 16))
    g2.drawString("Ожидание воспроизведения…", 40, getHeight / 2)

  private def drawFrame(g2: Graphics2D, f: Renderer.Frame): Unit =
    val w = getWidth
    val h = getHeight

    drawStatus(g2, f, w)

    val plotTop = 70
    val plotH = h - plotTop - 30
    f.mode match
      case VisualMode.Bars3 => drawBars3(g2, f, plotTop, plotH, w)
      case _                => drawSpectrum(g2, f, plotTop, plotH, w)

    drawLegend(g2, w, h - 22)

  // Верхняя строка: время, BPM, режим, индикатор бита
  private def drawStatus(g2: Graphics2D, f: Renderer.Frame, w: Int): Unit =
    g2.setFont(new Font("Monospaced", Font.BOLD, 14))
    g2.setColor(new Color(0xe6, 0xe8, 0xec))
    val modeStr = f.mode match
      case VisualMode.Spectrum    => "спектр"
      case VisualMode.Bars3       => "3 полосы"
      case VisualMode.Spectrogram => "спектрограмма"
    g2.drawString(f"время: ${f.timeSec}%6.2f с    BPM: ${f.bpm}%5.1f    режим: $modeStr", 20, 30)

    if f.beatActive then
      g2.setColor(new Color(0xe0, 0x4f, 0x4f))
      g2.fillRoundRect(w - 90, 14, 64, 24, 8, 8)
      g2.setColor(Color.WHITE)
      g2.drawString("BEAT", w - 78, 31)

  // Режим "спектр": три зоны во всю ширину, высота — корень из энергии
  private def drawSpectrum(g2: Graphics2D, f: Renderer.Frame, top: Int, h: Int, w: Int): Unit =
    val s = f.snapshot
    val maxE = math.max(math.max(s.bass, s.mid), s.high).max(1e-9)
    val third = w / 3

    def bar(x0: Int, bw: Int, value: Double, color: Color): Unit =
      val bh = (math.sqrt(value / maxE) * h).toInt
      g2.setColor(color)
      g2.fillRect(x0, top + h - bh, bw, bh)

    bar(0,          third,         s.bass, new Color(0xe0, 0x5a, 0x4f))
    bar(third,      third,         s.mid,  new Color(0xe0, 0xb0, 0x4f))
    bar(third * 2,  w - third * 2, s.high, new Color(0x4f, 0xc0, 0xe0))

  // Режим "3 полосы": столбики с зазорами
  private def drawBars3(g2: Graphics2D, f: Renderer.Frame, top: Int, h: Int, w: Int): Unit =
    val s = f.snapshot
    val maxE = math.max(math.max(s.bass, s.mid), s.high).max(1e-9)
    val gap = 40
    val barW = (w - gap * 4) / 3

    def bar(x0: Int, value: Double, color: Color): Unit =
      val bh = (math.sqrt(value / maxE) * h).toInt
      g2.setColor(color)
      g2.fillRoundRect(x0, top + h - bh, barW, bh, 10, 10)

    bar(gap,                     s.bass, new Color(0xe0, 0x5a, 0x4f))
    bar(gap * 2 + barW,          s.mid,  new Color(0xe0, 0xb0, 0x4f))
    bar(gap * 3 + barW * 2,      s.high, new Color(0x4f, 0xc0, 0xe0))

  // Подписи внизу
  private def drawLegend(g2: Graphics2D, w: Int, y: Int): Unit =
    g2.setFont(new Font("SansSerif", Font.PLAIN, 12))
    val third = w / 3
    g2.setColor(new Color(0xe0, 0x5a, 0x4f)); centered(g2, "БАС", 0, third, y)
    g2.setColor(new Color(0xe0, 0xb0, 0x4f)); centered(g2, "СРЕДНИЕ", third, third, y)
    g2.setColor(new Color(0x4f, 0xc0, 0xe0)); centered(g2, "ВЫСОКИЕ", third * 2, w - third * 2, y)

  private def centered(g2: Graphics2D, text: String, x: Int, width: Int, y: Int): Unit =
    val tw = g2.getFontMetrics.stringWidth(text)
    g2.drawString(text, x + (width - tw) / 2, y)
