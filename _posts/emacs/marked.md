---
title: Marked in Emacs
---

    (defun marked ()
      "Open the current file in Marked."
      (interactive)
      (when (buffer-file-name)
        (shell-command (concat "open -a Marked " (shell-quote-argument buffer-file-name)))))



$5+3 \frac{20}{2} \sum_5$
