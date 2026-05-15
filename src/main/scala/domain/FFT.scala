package domain

/** Быстрое преобразование Фурье (алгоритм Кули-Тьюки).
  * Работает с комплексными числами как с парами (re, im).
  * Размер входа должен быть степенью двойки.
  */
object FFT:

  /** Комплексное число */
  case class Complex(re: Double, im: Double):
    def +(o: Complex): Complex = Complex(re + o.re, im + o.im)
    def -(o: Complex): Complex = Complex(re - o.re, im - o.im)
    def *(o: Complex): Complex = Complex(re * o.re - im * o.im, re * o.im + im * o.re)
    def magnitude: Double = math.sqrt(re * re + im * im)

  /** Рекурсивный FFT.
    * Вход: вектор длины n = 2^k.
    * Выход: вектор комплексных коэффициентов той же длины.
    */
  def fft(xs: Vector[Complex]): Vector[Complex] =
    val n = xs.length
    if n <= 1 then xs
    else
      require((n & (n - 1)) == 0, s"Длина должна быть степенью двойки, получено $n")
      // Разделяем на чётные и нечётные индексы
      val even = fft(xs.zipWithIndex.collect { case (x, i) if i % 2 == 0 => x })
      val odd  = fft(xs.zipWithIndex.collect { case (x, i) if i % 2 == 1 => x })

      // Объединяем по формуле бабочки
      val half = n / 2
      val combined = Vector.tabulate(n) { k =>
        val angle = -2.0 * math.Pi * k / n
        val twiddle = Complex(math.cos(angle), math.sin(angle))
        val idx = k % half
        if k < half then even(idx) + twiddle * odd(idx)
        else even(idx) - Complex(math.cos(-2.0 * math.Pi * idx / n),
                                  math.sin(-2.0 * math.Pi * idx / n)) * odd(idx)
      }
      combined

  /** Окно Хэннинга — сглаживает края кадра, уменьшает спектральные утечки */
  def hannWindow(n: Int): Vector[Double] =
    Vector.tabulate(n)(i => 0.5 * (1.0 - math.cos(2.0 * math.Pi * i / (n - 1))))

  /** Разбивает сэмплы на кадры по fftSize, применяет окно и FFT.
    * Возвращает спектр (амплитуды) для каждого кадра.
    * Используется только первая половина бинов (вторая зеркальна).
    */
  def analyze(samples: Vector[Double], sampleRate: Int, fftSize: Int): Vector[SpectrumFrame] =
    val window = hannWindow(fftSize)
    val numFrames = samples.length / fftSize

    Vector.tabulate(numFrames) { frameIdx =>
      val start = frameIdx * fftSize
      val frame = (0 until fftSize).map(i => samples(start + i) * window(i))
      val complexInput = frame.map(d => Complex(d, 0.0)).toVector
      val spectrum = fft(complexInput)
      // Берём только первую половину — вторая зеркальна для вещественного сигнала
      val bins = spectrum.take(fftSize / 2).map(_.magnitude)
      SpectrumFrame(bins, sampleRate, fftSize)
    }