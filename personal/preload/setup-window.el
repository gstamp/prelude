(if window-system
    (progn
      (when (equal system-type 'darwin)
        (set-face-attribute 'default nil :font "Oxygen Mono" :height 140))
      (set-frame-size (selected-frame) 124 40)
      )
  )

(if (boundp 'idea-darkula)
    (setq prelude-theme 'idea-darkula))
