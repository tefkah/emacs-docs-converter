<!-- This is the GNU Emacs Lisp Reference Manual
corresponding to Emacs version 27.2.

Copyright (C) 1990-1996, 1998-2021 Free Software Foundation,
Inc.

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3 or
any later version published by the Free Software Foundation; with the
Invariant Sections being "GNU General Public License," with the
Front-Cover Texts being "A GNU Manual," and with the Back-Cover
Texts as in (a) below.  A copy of the license is included in the
section entitled "GNU Free Documentation License."

(a) The FSF's Back-Cover Text is: "You have the freedom to copy and
modify this GNU manual.  Buying copies from the FSF supports it in
developing GNU and promoting software freedom." -->

<!-- Created by GNU Texinfo 6.7, http://www.gnu.org/software/texinfo/ -->

Previous: [Abstract Display Functions](Abstract-Display-Functions.html), Up: [Abstract Display](Abstract-Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]

#### 39.20.2 Abstract Display Example

Here is a simple example using functions of the ewoc package to implement a *color components* display, an area in a buffer that represents a vector of three integers (itself representing a 24-bit RGB value) in various ways.

    (setq colorcomp-ewoc nil
          colorcomp-data nil
          colorcomp-mode-map nil
          colorcomp-labels ["Red" "Green" "Blue"])

    (defun colorcomp-pp (data)
      (if data
          (let ((comp (aref colorcomp-data data)))
            (insert (aref colorcomp-labels data) "\t: #x"
                    (format "%02X" comp) " "
                    (make-string (ash comp -2) ?#) "\n"))
        (let ((cstr (format "#%02X%02X%02X"
                            (aref colorcomp-data 0)
                            (aref colorcomp-data 1)
                            (aref colorcomp-data 2)))
              (samp " (sample text) "))
          (insert "Color\t: "
                  (propertize samp 'face
                              `(foreground-color . ,cstr))
                  (propertize samp 'face
                              `(background-color . ,cstr))
                  "\n"))))

    (defun colorcomp (color)
      "Allow fiddling with COLOR in a new buffer.
    The buffer is in Color Components mode."
      (interactive "sColor (name or #RGB or #RRGGBB): ")
      (when (string= "" color)
        (setq color "green"))
      (unless (color-values color)
        (error "No such color: %S" color))
      (switch-to-buffer
       (generate-new-buffer (format "originally: %s" color)))
      (kill-all-local-variables)
      (setq major-mode 'colorcomp-mode
            mode-name "Color Components")
      (use-local-map colorcomp-mode-map)
      (erase-buffer)
      (buffer-disable-undo)
      (let ((data (apply 'vector (mapcar (lambda (n) (ash n -8))
                                         (color-values color))))
            (ewoc (ewoc-create 'colorcomp-pp
                               "\nColor Components\n\n"
                               (substitute-command-keys
                                "\n\\{colorcomp-mode-map}"))))
        (set (make-local-variable 'colorcomp-data) data)
        (set (make-local-variable 'colorcomp-ewoc) ewoc)
        (ewoc-enter-last ewoc 0)
        (ewoc-enter-last ewoc 1)
        (ewoc-enter-last ewoc 2)
        (ewoc-enter-last ewoc nil)))

This example can be extended to be a color selection widget (in other words, the “controller” part of the “model–view–controller” design paradigm) by defining commands to modify `colorcomp-data` and to finish the selection process, and a keymap to tie it all together conveniently.

    (defun colorcomp-mod (index limit delta)
      (let ((cur (aref colorcomp-data index)))
        (unless (= limit cur)
          (aset colorcomp-data index (+ cur delta)))
        (ewoc-invalidate
         colorcomp-ewoc
         (ewoc-nth colorcomp-ewoc index)
         (ewoc-nth colorcomp-ewoc -1))))

    (defun colorcomp-R-more () (interactive) (colorcomp-mod 0 255 1))
    (defun colorcomp-G-more () (interactive) (colorcomp-mod 1 255 1))
    (defun colorcomp-B-more () (interactive) (colorcomp-mod 2 255 1))
    (defun colorcomp-R-less () (interactive) (colorcomp-mod 0 0 -1))
    (defun colorcomp-G-less () (interactive) (colorcomp-mod 1 0 -1))
    (defun colorcomp-B-less () (interactive) (colorcomp-mod 2 0 -1))

    (defun colorcomp-copy-as-kill-and-exit ()
      "Copy the color components into the kill ring and kill the buffer.
    The string is formatted #RRGGBB (hash followed by six hex digits)."
      (interactive)
      (kill-new (format "#%02X%02X%02X"
                        (aref colorcomp-data 0)
                        (aref colorcomp-data 1)
                        (aref colorcomp-data 2)))
      (kill-buffer nil))

    (setq colorcomp-mode-map
          (let ((m (make-sparse-keymap)))
            (suppress-keymap m)
            (define-key m "i" 'colorcomp-R-less)
            (define-key m "o" 'colorcomp-R-more)
            (define-key m "k" 'colorcomp-G-less)
            (define-key m "l" 'colorcomp-G-more)
            (define-key m "," 'colorcomp-B-less)
            (define-key m "." 'colorcomp-B-more)
            (define-key m " " 'colorcomp-copy-as-kill-and-exit)
            m))

Note that we never modify the data in each node, which is fixed when the ewoc is created to be either `nil` or an index into the vector `colorcomp-data`, the actual color components.

Previous: [Abstract Display Functions](Abstract-Display-Functions.html), Up: [Abstract Display](Abstract-Display.html)   \[[Contents](index.html#SEC_Contents "Table of contents")]\[[Index](Index.html "Index")]