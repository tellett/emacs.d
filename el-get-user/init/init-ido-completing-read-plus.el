(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(setq magit-completing-read-function 'magit-ido-completing-read)
