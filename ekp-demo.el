;;; ekp-demo.el --- Visual demo: ekp vs fill-paragraph  -*- lexical-binding: t; -*-
(require 'ekp)
(defvar ekp-demo--en
  "The Knuth-Plass line breaking algorithm, used in TeX, considers all possible break points globally to minimize the overall badness of a paragraph. Unlike the greedy first-fit algorithm, which fills each line as much as possible and moves on, Knuth-Plass can choose a slightly tighter or looser line to prevent a later line from being much worse. The result is a paragraph with evenly distributed spacing that looks clean and professional. This global optimization is what gives TeX its reputation for beautiful typesetting.")
(defvar ekp-demo--cjk
  "这是关于Knuth-Plass算法的一个示例。这个算法由高德纳（Donald Knuth）和他的学生共同开发，用于TeX排版系统。与贪心算法不同，Knuth-Plass会全局考虑所有可能的换行点。在中文排版中，中文字符之间不需要空格，但中英文混排时需要在合适的位置插入空格。这个算法可以自动处理这些情况，确保排版效果均匀美观。")
;;;###autoload
(defun ekp-demo-show (&optional width-px)
  "Show side-by-side comparison of greedy fill vs ekp Knuth-Plass.
WIDTH-PX defaults to the display window's text-area pixel width.
Run interactively:  M-x ekp-demo-show"
  (interactive)
  (unless (condition-case nil
              (progn (ekp-param-set-default ekp-demo--en) t)
            (error nil))
    (ekp-param-set 10 5 3 5 3 2 0 2 0))
  (let* ((buf (get-buffer-create "*ekp-demo*"))
         (txt (concat ekp-demo--en "\n\n" ekp-demo--cjk))
         wp fc win win-px ibw fringes header-info)
    (with-current-buffer buf
      (erase-buffer)
      (insert txt)
      (goto-char (point-min)))
    (setq win (display-buffer buf)
          win-px (window-pixel-width win)
          ibw (or (frame-parameter (window-frame win) 'internal-border-width) 0)
          fringes (window-fringes win)
          wp (or width-px (max 1 (- (window-text-width win t) (default-font-width))))
          fc (round (/ (float wp) (default-font-width)))
          header-info (format "win-px=%d ibw=%d fringes=(%d,%d)"
                              win-px ibw (nth 0 fringes) (nth 1 fringes)))
    (with-current-buffer buf
      (erase-buffer)
      (insert (propertize
               (format "■ fill-paragraph (greedy to ~%d chars, %s)\n" fc header-info)
               'face 'bold))
      (insert (make-string fc ?-) "\n")
      (insert txt)
      (let ((fill-column fc))
        (fill-region (point-min) (point-max)))
      (insert "\n\n")
      (let ((result (ekp-pixel-justify txt wp)))
        (insert (propertize
                 (format "■ ekp-pixel-justify (Knuth-Plass to %dpx, %s)\n" wp header-info)
                 'face 'bold))
        (insert (make-string fc ?-) "\n")
        (insert result))
      (goto-char (point-min))
      (visual-line-mode 1))
    (message "ekp-demo: see *ekp-demo* buffer")))
(provide 'ekp-demo)
;;; ekp-demo.el ends here
