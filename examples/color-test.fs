configuration:
  size: 300x100
  title: Pick a color scheme
  vertical-contents:
  - selection:
      choices:
      - rainbow
      - arenberg
      - romao
      - bamo
      - broco
      - corko
      - viko
      - ice
      - fire
      - rose
      - wheat
      - forest
      - ocean
      - winter
      - spring
      - summer
      - fall
      current-choice: 0
      label: Color scheme
      variable: index
  - text-entry:
      label: 'Iterations limit: '
      type: Z
      value: 100
      variable: max_iters
  - text-entry:
      label: 'Iterations per color cycle: '
      type: Z
      value: 20
      variable: speed
  - checkbox:
      label: Use smooth coloring?
      value: true
      variable: smooth
  - checkbox:
      label: Use checkers?
      value: false
      variable: checkers
  - text-entry:
      label: 'Checker phase shift: '
      type: R
      value: 0.333
      variable: checker_shift
  - text-entry:
      label: 'C = '
      type: C
      value: -0.11 + 0.78i
      variable: C
setup:
  size: 300x500
  title: Color test
  vertical-contents:
  - code:
      environment:
        index: ℤ
        t: ℝ
      value: |
        if index = 0:
          color <- rainbow(t)
        else if index = 1:
          color <- arenberg(t)
        else if index = 2:
          color <- romao(t)
        else if index = 3:
          color <- bamo(t)
        else if index = 4:
          color <- broco(t)
        else if index = 5:
          color <- corko(t)
        else if index = 6:
          color <- viko(t)
        else if index = 7:
          color <- ice(t)
        else if index = 8:
          color <- fire(t)
        else if index = 9:
          color <- rose(t)
        else if index = 10:
          color <- wheat(t)
        else if index = 11:
          color <- forest(t)
        else if index = 12:
          color <- ocean(t)
        else if index = 13:
          color <- winter(t)
        else if index = 14:
          color <- spring(t)
        else if index = 15:
          color <- summer(t)
        else if index = 16:
          color <- fall(t)
        else:
          color <- grey
      variable: colorize
viewers:
- code: |
    t : ℝ ⭠ re z / 4
    «colorize»
  position: 100x209
  size: 640x64
  title: Linear gradient
  z-coord: z
- code: |
    t : ℝ ⭠ arg z / (2 pi)
    «colorize»
  position: 762x123
  size: 174x151
  title: Cyclic gradient
  z-coord: z
- code: |
    z : C <- 0
    iterate z -> z^2 + C until z escapes

    color <- black
    if z escaped:
      t : ℝ ⭠ iterations
      if smooth:
        t ⭠ iterations + 1 - log (log (|z|²) / (2 log 10)) / log 2
      t ⭠ t / speed
      if checkers and im z > 0:
        t ⭠ t + checker_shift
      «colorize»
  initial-pixel-size: 1/150
  iteration-limit: max_iters
  position: 89x318
  size: 450x402
  title: Mandelbrot set
  z-coord: C
- code: |
    iterate z -> z^2 + C until z escapes

    color <- black
    if z escaped:
      t : ℝ ⭠ iterations
      if smooth:
        t ⭠ iterations + 1 - log (log (|z|²) / (2 log 10)) / log 2
      t ⭠ t / speed
      if checkers and im z > 0:
        t ⭠ t + checker_shift
      «colorize»
  initial-pixel-size: 1/150
  iteration-limit: max_iters
  position: 546x321
  size: 429x393
  title: Julia set
  z-coord: z
