(defadvice popup-menu-show-quick-help
    (around pos-tip-popup-menu-show-quick-help () activate)
  "Show quick help using `pos-tip-show'."
  (if (eq window-system 'x)
      (let ((doc (popup-menu-document
                  menu (or item
                           (popup-selected-item menu)))))
        (when (stringp doc)
          (pos-tip-show doc nil
                        (if (popup-hidden-p menu)
                            (or (plist-get args :point)
                                (point))
                          (overlay-end (popup-line-overlay
                                        menu (+ (popup-offset menu)
                                                (popup-selected-line menu)))))
                        nil 0)
          nil))
    ad-do-it))
