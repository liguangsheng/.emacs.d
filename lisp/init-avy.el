(use-package avy
  :init
  (avy-setup-default)

  :pretty-hydra
  ((:color pink :title "Avy: fast navigation")
   ("Char"
	(("c" avy-goto-char :color blue)
     ("C" avy-goto-char-2 :color blue)
     ("f" avy-goto-char-in-line :color blue))
	"Word"
	(("w" avy-goto-word-1 :color blue)
     ("S" avy-goto-subword-1 :color blue)
     ("J" avy-goto-word-1-above :color blue)
     ("W" avy-goto-word-1-below :color blue))
	"Line"
	(("l" avy-goto-line :color blue)
     ("j" avy-goto-char-timer :color blue))
	"Search"
	(("s" avy-isearch :color blue))
	"Cancel"
	(("q" nil :color blue)
	 ("x" nil :color blue)
	 )))

  :general
  (my-leader-def "a" #'avy-hydra/body)
  )

(provide 'init-avy)
