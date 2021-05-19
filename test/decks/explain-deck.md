---
explain:
  camHeight: 720
  camWidth: 1280
  greenScreenBackground: /test/static/06-metal.png
  greenScreenKey:
    b: 0
    g: 255
    r: 0
  greenScreenSimilarity: 0.4
  greenScreenSmoothness: 0.08
  recHeight: 1080
  recWidth: 1920
  useGreenScreen: true
title: Explain Things with Video
---

# Greenscreen

## Configuration

``` {.yaml}
explain:
    useGreenScreen: true
    greenScreenBackground: 'data/background.jpg'
    greenScreenKey: { r: 0, g: 255, b: 0 }
    greenScreenSimilarity: 0.4
    greenScreenSmoothness: 0.08
```

## Invocation

1.  Type `r` to enable recording
2.  Type `v` to enable video
