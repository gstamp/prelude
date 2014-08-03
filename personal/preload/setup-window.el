(if window-system
    (progn
      (set-face-attribute 'default nil :font "Oxygen Mono" :height 140)
      (set-frame-size (selected-frame) 124 40)
      )
  )
