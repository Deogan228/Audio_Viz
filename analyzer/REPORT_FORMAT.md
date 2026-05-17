# Формат отчёта анализатора

Анализатор сохраняет результаты в JSON. Визуализатор читает этот файл и использует его для рендеринга.

## Структура

```json
{
  "version": 1,
  "source": {
    "path": "track.wav",
    "sampleRate": 44100,
    "channels": 2,
    "bitsPerSample": 16,
    "durationSeconds": 247.34,
    "numSamples": 10907694
  },
  "analysis": {
    "fftSize": 1024,
    "framesCount": 10651,
    "frameDurationSec": 0.0232,
    "bpm": 128.5,
    "beatsCount": 528,
    "beatThreshold": 1.3,
    "beatWindow": 43
  },
  "beats": [
    { "frameIndex": 12, "timeSec": 0.278, "energy": 0.0341 },
    { "frameIndex": 35, "timeSec": 0.812, "energy": 0.0289 }
  ],
  "bands": [
    {
      "timeSec": 0.0,
      "bass": 0.45,
      "mid": 0.31,
      "high": 0.12,
      "dominantFreq": 110.5
    }
  ]
}
```

## Поля

### `source` — данные исходного файла
- `path` — путь к WAV-файлу
- `sampleRate` — частота дискретизации (Hz)
- `channels` — каналов (1 = моно, 2 = стерео)
- `bitsPerSample` — битность сэмплов
- `durationSeconds` — длительность в секундах
- `numSamples` — общее число сэмплов

### `analysis` — параметры и сводка анализа
- `fftSize` — размер окна FFT (степень двойки)
- `framesCount` — количество FFT-кадров
- `frameDurationSec` — длительность одного кадра (= fftSize / sampleRate)
- `bpm` — определённый темп
- `beatsCount` — количество найденных битов
- `beatThreshold`, `beatWindow` — параметры детектора

### `beats` — список обнаруженных битов
- `frameIndex` — индекс FFT-кадра
- `timeSec` — момент бита в секундах
- `energy` — энергия в момент бита

### `bands` — энергия по диапазонам по времени
По одной записи на каждый FFT-кадр:
- `timeSec` — момент времени
- `bass` — энергия в диапазоне 30–250 Hz
- `mid` — энергия 250–4000 Hz
- `high` — энергия 4000+ Hz
- `dominantFreq` — доминирующая частота кадра (Hz)
