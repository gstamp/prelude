(if window-system
    (progn
      (when (equal system-type 'darwin)
        ;; Font fallback
        (cond
         ((find-font (font-spec :name "Operator Mono-14:weight=light"))
          (set-frame-font "Operator Mono-14:weight=light"))
         ((find-font (font-spec :name "Oxygen Mono-14"))
          (set-frame-font "Oxygen Mono-14"))
         ((find-font (font-spec :name "DejaVu Sans Mono"))
          (set-frame-font "DejaVu Sans Mono-12"))
         ((find-font (font-spec :name "inconsolata"))
          (set-frame-font "inconsolata-12"))
         ((find-font (font-spec :name "Lucida Console"))
          (set-frame-font "Lucida Console-12"))
         ((find-font (font-spec :name "courier"))
          (set-frame-font "courier-12"))))
      (set-frame-size (selected-frame) 124 40)
      )
  )

(if (featurep 'idea-darkula-theme)
    (setq prelude-theme 'idea-darkula))
