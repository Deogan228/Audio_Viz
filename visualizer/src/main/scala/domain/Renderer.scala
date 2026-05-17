package domain

import monads.{IO, Reader, State}

/** Рендеринг кадров визуализации.
  *
  * Использует Reader[RenderConfig, ...] для проброса параметров отображения,
  * и State[AnimationState, ...] для управления состоянием анимации
  * (текущий кадр, индикатор бита, режим визуализации).
  *
  * Поддерживается несколько режимов визуализации, переключаемых через
  * изменение состояния AnimationState.activeMode.
  */
object Renderer:

  // ANSI-коды
  private val Clear      = "\u001b[2J"
  private val Home       = "\u001b[H"
  private val ClearLine  = "\u001b[K"
  private val Reset      = "\u001b[0m"
  private val Bold       = "\u001b[1m"
  private val HideCursor = "\u001b[?25l"
  private val ShowCursor = "\u001b[?25h"

  private val ColorBass   = "\u001b[31m"
  private val ColorMid    = "\u001b[33m"
  private val ColorHigh   = "\u001b[36m"
  private val ColorWhite  = "\u001b[97m"
  private val ColorBeatBg = "\u001b[41m"

  /** Подготовка терминала: очистка, скрытие курсора */
  def setup: IO[Unit] = IO.delay {
    print(Clear)
    print(Home)
    print(HideCursor)
    System.out.flush()
  }

  /** Восстановление терминала: показать курсор, перевод строки */
  def teardown: IO[Unit] = IO.delay {
    print(ShowCursor)
    println()
    println("Готово.")
  }

  /** Рендер одного кадра через Reader[RenderConfig, String].
    *
    * Зависит от:
    *   - RenderConfig (height, width, флаги) — через Reader
    *   - AnimationState (режим, индикатор бита) — передаётся параметром
    */
  def renderFrame(
      snapshot: BandSnapshot,
      bpm: Double,
      animState: AnimationState
  ): Reader[RenderConfig, String] =
    Reader.asks { cfg =>
      animState.activeMode match
        case VisualMode.Spectrum    => renderSpectrum(snapshot, bpm, animState, cfg)
        case VisualMode.Bars3       => renderBars3(snapshot, bpm, animState, cfg)
        case VisualMode.Spectrogram => renderSpectrum(snapshot, bpm, animState, cfg)
          // Spectrogram реализуется как накопление — сложнее, пока fallback
    }

  /** Режим 1: спектр с делением на три цветовые зоны */
  private def renderSpectrum(
      s: BandSnapshot,
      bpm: Double,
      anim: AnimationState,
      cfg: RenderConfig
  ): String =
    val width = cfg.width
    val height = cfg.height

    // Разбиваем ширину между диапазонами: 1/3 на каждый
    val third = width / 3
    val rest = width - third * 2

    // Высоты столбиков
    val maxEnergy = math.max(math.max(s.bass, s.mid), s.high).max(1e-9)
    val bassH = (math.sqrt(s.bass / maxEnergy) * height).toInt
    val midH  = (math.sqrt(s.mid / maxEnergy) * height).toInt
    val highH = (math.sqrt(s.high / maxEnergy) * height).toInt

    val sb = new StringBuilder
    sb.append(statusLine(s.timeSec, bpm, anim, cfg))

    for row <- 0 until height do
      val rowFromBottom = height - row
      // Бас
      val bassChar = if bassH >= rowFromBottom then '█' else ' '
      sb.append(ColorBass)
      for _ <- 0 until third do sb.append(bassChar)
      // Средние
      val midChar = if midH >= rowFromBottom then '█' else ' '
      sb.append(ColorMid)
      for _ <- 0 until third do sb.append(midChar)
      // Высокие
      val highChar = if highH >= rowFromBottom then '█' else ' '
      sb.append(ColorHigh)
      for _ <- 0 until rest do sb.append(highChar)

      sb.append(Reset).append(ClearLine).append('\n')

    sb.append(legendLine(width)).append(ClearLine).append('\n')
    sb.toString

  /** Режим 2: три крупных столбика — упрощённый вид */
  private def renderBars3(
      s: BandSnapshot,
      bpm: Double,
      anim: AnimationState,
      cfg: RenderConfig
  ): String =
    val width = cfg.width
    val height = cfg.height
    val barWidth = (width - 6) / 3
    val gap = 3

    val maxEnergy = math.max(math.max(s.bass, s.mid), s.high).max(1e-9)
    val bassH = (math.sqrt(s.bass / maxEnergy) * height).toInt
    val midH  = (math.sqrt(s.mid  / maxEnergy) * height).toInt
    val highH = (math.sqrt(s.high / maxEnergy) * height).toInt

    val sb = new StringBuilder
    sb.append(statusLine(s.timeSec, bpm, anim, cfg))

    for row <- 0 until height do
      val rowFromBottom = height - row
      sb.append(' ').append(ColorBass)
      val bassC = if bassH >= rowFromBottom then '█' else ' '
      for _ <- 0 until barWidth do sb.append(bassC)
      sb.append(Reset)
      for _ <- 0 until gap do sb.append(' ')
      sb.append(ColorMid)
      val midC = if midH >= rowFromBottom then '█' else ' '
      for _ <- 0 until barWidth do sb.append(midC)
      sb.append(Reset)
      for _ <- 0 until gap do sb.append(' ')
      sb.append(ColorHigh)
      val highC = if highH >= rowFromBottom then '█' else ' '
      for _ <- 0 until barWidth do sb.append(highC)
      sb.append(Reset).append(ClearLine).append('\n')

    sb.append(legendLine(width)).append(ClearLine).append('\n')
    sb.toString

  /** Верхняя строка статуса: время, BPM, доминирующая частота, индикатор бита */
  private def statusLine(timeSec: Double, bpm: Double, anim: AnimationState, cfg: RenderConfig): String =
    val timeStr = f"$timeSec%6.2fs"
    val bpmStr  = f"BPM: $bpm%5.1f"
    val modeStr = anim.activeMode match
      case VisualMode.Spectrum    => "[режим: спектр]"
      case VisualMode.Bars3       => "[режим: 3 полосы]"
      case VisualMode.Spectrogram => "[режим: спектрограмма]"
    val beatStr = if anim.beatFlashFrames > 0 then s"$ColorBeatBg$Bold BEAT $Reset" else "      "
    s"$Bold$ColorWhite$timeStr  $bpmStr  $modeStr  $beatStr$Reset$ClearLine\n$ClearLine\n"

  /** Нижняя строка с подписями диапазонов */
  private def legendLine(width: Int): String =
    val third = width / 3
    val rest = width - third * 2
    val bass = center("БАС", third)
    val mid  = center("СРЕДНИЕ", third)
    val high = center("ВЫСОКИЕ", rest)
    s"$ColorWhite$bass$mid$high$Reset"

  private def center(s: String, width: Int): String =
    if s.length >= width then s.take(width)
    else
      val pad = width - s.length
      val left = pad / 2
      val right = pad - left
      " " * left + s + " " * right

  /** Обновление состояния анимации.
    *
    * State демонстрирует функциональное изменение состояния:
    * на каждом тике передвигаем currentSnapshotIdx, обновляем
    * beatFlashFrames в зависимости от наличия бита в текущем кадре.
    */
  def tickAnimation(
      newSnapshotIdx: Int,
      isBeat: Boolean,
      cfg: RenderConfig
  ): State[AnimationState, AnimationState] =
    State { st =>
      val newFlash =
        if isBeat then cfg.beatFlashDurationFrames
        else math.max(0, st.beatFlashFrames - 1)
      val newSt = st.copy(
        currentSnapshotIdx = newSnapshotIdx,
        beatFlashFrames = newFlash
      )
      (newSt, newSt)
    }

  /** Переключить режим визуализации (для будущего интерактива) */
  def switchMode(newMode: VisualMode): State[AnimationState, Unit] =
    State { st => (st.copy(activeMode = newMode), ()) }
