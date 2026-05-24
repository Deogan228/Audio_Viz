package domain

object FFT:

  case class Complex(re: Double, im: Double):
    def +(o: Complex): Complex = Complex(re + o.re, im + o.im)
    def -(o: Complex): Complex = Complex(re - o.re, im - o.im)
    def *(o: Complex): Complex = Complex(re * o.re - im * o.im, re * o.im + im * o.re)
    def magnitude: Double = math.sqrt(re * re + im * im)

  def fft(xs: Vector[Complex]): Vector[Complex] =
    val n = xs.length
    if n <= 1 then xs
    else
      require((n & (n - 1)) == 0, s"Длина должна быть степенью двойки, получено $n")
      val even = fft(xs.zipWithIndex.collect { case (x, i) if i % 2 == 0 => x })
      val odd  = fft(xs.zipWithIndex.collect { case (x, i) if i % 2 == 1 => x })

      val half = n / 2
      Vector.tabulate(n) { k =>
        val idx = k % half
        val angle = -2.0 * math.Pi * idx / n
        val twiddle = Complex(math.cos(angle), math.sin(angle))
        if k < half then even(idx) + twiddle * odd(idx)
        else even(idx) - twiddle * odd(idx)
      }

  def hannWindow(n: Int): Vector[Double] =
    Vector.tabulate(n)(i => 0.5 * (1.0 - math.cos(2.0 * math.Pi * i / (n - 1))))

  def analyze(samples: Vector[Double], sampleRate: Int, fftSize: Int): Vector[SpectrumFrame] =
    val window = hannWindow(fftSize)
    val numFrames = samples.length / fftSize

    Vector.tabulate(numFrames) { frameIdx =>
      val start = frameIdx * fftSize
      val frame = (0 until fftSize).map(i => samples(start + i) * window(i))
      val complexInput = frame.map(d => Complex(d, 0.0)).toVector
      val spectrum = fft(complexInput)
      val bins = spectrum.take(fftSize / 2).map(_.magnitude)
      SpectrumFrame(bins, sampleRate, fftSize)
    }
