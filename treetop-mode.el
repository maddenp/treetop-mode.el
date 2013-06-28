;; Copyright 2012 Paul Madden (maddenp@colorado.edu)
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(defvar w 2) ;; spaces per indentation level

(require 'generic-x)

(define-generic-mode 'treetop-mode-base
  '("#")
  '("end" "include" "grammar" "module" "require" "rule")
  '(("\\(<[A-Za-z0-9_]+>\\)" 1 'font-lock-variable-name-face)
    ("rule\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*$" 1 'font-lock-function-name-face) 
    ("grammar\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*$" 1 'font-lock-type-face)
    ("include\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*$" 1 'font-lock-type-face)
    ("module\\s-+\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*$" 1 'font-lock-type-face)
    ("[^\\]'[^']*[^\\]'" . 'font-lock-string-face))
  nil
  nil
  "A base mode for Treetop grammar files")

(defun blank    () (looking-at "$"))
(defun end      () (looking-at "end[^A-Za-z0-9_]"))
(defun grammar  () (looking-at "grammar[^A-Za-z0-9_]"))
(defun module   () (looking-at "module[^A-Za-z0-9_]"))
(defun nonblank () (looking-at "[^:space:]"))
(defun rule     () (looking-at "rule[^A-Za-z0-9_]"))

(defun set-indent (p0 xend xmodule xrule xgrammar)
  (let ((x -1))
    (save-excursion
      (while (= x -1)
        (forward-line -1)
        (skip-chars-forward "[:space:]")
        (let ((c (current-indentation)) (p1 (point)))
          (setq x (if (= p0 p1)
                      0
                    (+ c (cond
                          ((end) xend)
                          ((rule) xrule)
                          ((grammar) xgrammar)
                          ((module) xmodule)
                          ((bobp) (- c))
                          (t (- (+ c 1))))))))))
    x))

(defun treetop-indent-line ()
  "Indent current line of Treetop grammar"
  (beginning-of-line)
  (skip-chars-forward "[:space:]")
  (let ((p0 (point)))
    (indent-line-to
     (cond
      ((blank) 0)
      ((rule)     (set-indent p0 (+ 0) (+ w) (+ 0) (+ w)))
      ((end)      (set-indent p0 (- w) (- w) (+ 0) (+ 0)))
      ((grammar)  (set-indent p0 (+ 0) (+ w) (+ 0) (+ 0)))
      ((module)   (set-indent p0 (+ 0) (+ w) (+ 0) (+ 0)))
      ((nonblank) (set-indent p0 (+ 0) (+ w) (+ w) (+ w)))))))

(define-derived-mode treetop-mode treetop-mode-base "Treetop"
  "A major mode for Treetop grammar files"
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
  (set (make-local-variable 'indent-line-function) 'treetop-indent-line)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'tab-always-indent) t)
  (set (make-local-variable 'tab-width) w))

(add-to-list 'auto-mode-alist '("\\.tt\\'" . treetop-mode))

(provide 'treetop-mode)
