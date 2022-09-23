;;; ob-html.el --- Babel Functions for HTML Code Blocks  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This code allows you to evaluate HTML in org-mode code blocks.

;;; Setup:

;;   (with-eval-after-load "org"
;;     (require 'ob-html)
;;     (org-babel-html-enable-open-src-block-result-temporary)) ;;Enable C-c C-o on html code block

;;; Examples:

;; - The result is html export block(#+begin_export:html ~ #+end_export)
;;   #+begin_src html :results replace html
;;   <p><strong>STRONG</strong> <ins>INS</ins> <del>DEL</del></p>
;;   #+end_src
;;
;; - The result is external html file
;;   #+begin_src html :results replace file :file example.html
;;   <!DOCTYPE html>
;;   <html><head><title>example</title></head><body><p>Example</p></body></html>
;;   #+end_src
;;   NOTE: Hit C-c C-o to open the file in your browser
;;
;; - The result is screenshot image file
;;   #+begin_src html :results replace file graphics :file example.png :width 640 :height 100
;;   <!DOCTYPE html>
;;   <html><head><title>example</title></head><body><p>Example</p></body></html>
;;   #+end_src
;;
;; - The result is silent but open HTML as temporary file
;;   #+begin_src html
;;   <html><head><title>example</title></head><body><p>Example</p></body></html>
;;   #+end_src
;;   NOTE: Hit C-c C-o to open the file in your browser

;;; Code:
(require 'ob)

;; Settings

(defcustom org-babel-html-screenshot-method
  'org-babel-html-take-screenshot-by-chrome
  "Method to take screenshot."
  :group 'org-babel
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Headless Chrome"
                        org-babel-html-take-screenshot-by-chrome)
                 function))

(defcustom org-babel-html-chrome-executable
  (cond
   ((eq system-type 'windows-nt)
    (seq-find
     #'file-exists-p
     '("C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
       "C:/Program Files/Google/Chrome/Application/chrome.exe")))
   (t "chrome"))
  "Absolute path to Google Chrome."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-html-execution-method-silent
  'org-babel-html-open-as-temporary-file
  "Specifies how to execute a code block that :results is silent."
  :group 'org-babel
  :type '(choice (const :tag "Do nothing" nil)
                 (const :tag "Open as temporary file"
                        org-babel-html-open-as-temporary-file)
                 function))

;; Define Babel Varibles & Functions

(defvar org-babel-default-header-args:html
  '((:results . "silent")
    (:exports . "code"))
  "Default arguments for evaluating an html source block.")

(defun org-babel-expand-body:html (body params)
  (dolist (var (org-babel--get-vars params))
    (setq body (replace-regexp-in-string
                (regexp-quote (format "$%s" (car var)))
                (format "%s" (cdr var))
                body nil 'literal)))
  body)

(defun org-babel-execute:html (body params)
  "Execute a block of HTML code with.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (cdr (assq :result-params params)))
         (graphics-file (and (member "graphics" result-params)
                             (org-babel-graphical-output-file params)))
         (body (org-babel-expand-body:html
                (replace-regexp-in-string "^," "" body) params)))

    (cond
     ;; Take a Screenshot
     (graphics-file
      (org-babel-html-take-screenshot-string body graphics-file params)
      nil)

     ;; Generate file
     ((member "file" result-params)
      body)

     ;; results is silent (and !graphics-file !file)
     ((member "silent" result-params)
      (if (functionp org-babel-html-execution-method-silent)
          (funcall org-babel-html-execution-method-silent body)
        body))

     ;; HTML As Value
     (t
      body))))

(defun org-babel-prep-session:html (_session _params)
  "Return an error because HTML does not support sessions."
  (error "HTML does not support sessions"))


;; Screenshot

(defvar org-babel-temporary-directory)

(defun org-babel-html-take-screenshot-string (body graphics-file params)
  (let ((html-file
         (let ((org-babel-temporary-directory default-directory))
           (org-babel-temp-file "ob-html-temp-" ".html"))))
    (unwind-protect
        (progn
          (with-temp-file html-file (insert body))
          (org-babel-html-take-screenshot html-file graphics-file params))
      (delete-file html-file)))
  )

(defun org-babel-html-take-screenshot (html-file graphics-file params)
  (cond
   ((functionp org-babel-html-screenshot-method)
    (funcall org-babel-html-screenshot-method html-file graphics-file params))

   (t
    (error "org-babel-html-screenshot-method is invalid"))))

(defun org-babel-html-take-screenshot-by-chrome (html-file graphics-file params)
  (let* ((width (cdr (assq :width params)))
         (height (cdr (assq :height params)))
         (window-size (if (and width height)
                          (format " --window-size=%s,%s" width height) ""))
         (cmd (format "%s --headless --disable-gpu --screenshot=%s %s file://%s"
                      (shell-quote-argument org-babel-html-chrome-executable)
                      (shell-quote-argument (expand-file-name graphics-file))
                      window-size
                      (shell-quote-argument html-file))))
    (org-babel-eval cmd "")))


;; Open HTML as temporary file

(defun org-babel-html-enable-open-src-block-result-temporary ()
  "Enable to open html code block as temporary file when
`org-babel-open-src-block-result' (C-c C-o) calling."
  (interactive)
  (advice-add 'org-babel-open-src-block-result :around
              'org-babel-html-open-src-block-result-advice))

(defun org-babel-html-open-src-block-result-advice (old-func &optional args)
  (pcase (org-babel-get-src-block-info 'light)
    ;; html and doesn't have a :file header argument
    (`("html" ,body
       ,(and arguments (guard (null (assq :file arguments)))) ,_ ,_ ,_ ,_)
     (org-babel-html-open-as-temporary-file body))
    ;; otherwise
    (_
     (apply old-func args))))

(defun org-babel-html-open-src-block-as-temporary-file ()
  "Open the pointed html code block as temporary file."
  (interactive)
  (pcase (org-babel-get-src-block-info 'light)
    (`(,_ ,body ,_ ,_ ,_ ,_ ,_)
     (org-babel-html-open-as-temporary-file body))
    (_ nil)))

(defvar org-babel-html-waiting-time-for-open-temporary-file 0.75)

(defun org-babel-html-open-as-temporary-file (body)
  "Open the HTML String BODY."
  (let ((html-file (let ((temporary-file-directory default-directory))
                     (make-temp-file "ob-html-temp-" nil ".html" body))))
    (unwind-protect
        (progn
          (browse-url-of-file html-file)
          (sleep-for org-babel-html-waiting-time-for-open-temporary-file))
      (delete-file html-file)))
  ;; return result value
  body)


(provide 'ob-html)
;;; ob-html.el ends here
