;;; org-link-completion.el ---  Org Link Completion At Point  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>
;; Keywords: outlines

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

;; 

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-element)
(require 'elisp-mode)


(defgroup org-link-completion nil
  "Org link completion at point."
  :tag "Org Link Completion At Point"
  :prefix "org-link-completion-"
  :group 'org)

(defgroup org-link-completion-functions nil
  "Completion at point functions for Org link."
  :tag "Completion Functions"
  :prefix "org-link-completion-"
  :group 'org-link-completion)

;;;; Setup

;;;###autoload
(defun org-link-completion-setup ()
  (interactive)
  (with-eval-after-load "org"
    (org-link-completion-setup-type-file)
    (org-link-completion-setup-type-id)
    (org-link-completion-setup-type-help)
    (org-link-completion-setup-type-elisp)
    (org-link-completion-setup-type-info)
    (add-hook 'org-mode-hook
              (lambda ()
                (add-hook 'completion-at-point-functions
                          'org-link-completion-at-point nil t)))))


;;;; Parse Link At Point

;; Without type:
;; [[#my-custom-id]
;; [[*heading]
;; [[(jump)   (See: https://orgmode.org/manual/Literal-Examples.html)
;; [[(jump)][Line (jump)
;; [[mytarget
;; [[My Target]
;; [[./dir/file]
;; [[../dir/file]
;; [[/]
;; [[\]
;; [[/dir/file]
;; [[\dir\file]
;; [[~/.emacs.d/]
;; [[~\.emacs.d/]
;; [[~USER/file]
;; [[~USER\file]
;; [[~USER]
;; [[~]
;; [[c:/
;; [[c:\
;;
;; With type:
;; [[unfinishedtype
;; [[type:<path>]
;; [[file:test.org
;; [[file:/dir/file]
;; [[file:README.org::library]
;; [[file:README.org::*Setup]
;; [[file:README.org::#custom-id]

;;
;; [[\[<type>:<path>\]][escape sequence
;; [[^#\\\\+TITLE][escape sequence
;; [[My Target][description[multiple[bracket]bracket]bracket]]
;; [[    ][
;; [[    ][  ]]
;; [[    ][  ][   ][   ]]

;; Not supported:
;; [[type:path][foofoofoo[[foofofofof[[fofof]] <= Unable to parse correctly
;; [[My
;; Target]] <= Not supported

;; Invalid Syntax:
;; [[My Target\][description]]
;; [[My [Target][description]]
;; [[My ]Target][description]]

(defcustom org-link-completion-accept-undefined-type nil
  "Non-nil means that the completed link type is treated as a link
type even if it is not registered in org-link-parameters.

For example, in the notation [[undefinedtype:foobar], this option
changes whether the part before the colon becomes the link type
or the target string of an internal link."
  :group 'org-link-completion
  :type 'boolean)

(defun org-link-completion-parse-at-point ()
  "Return a list in the following format:
  (WHERE TYPE-BEG TYPE-END [ PATH-BEG PATH-END [ DESC-BEG DESC-END ] ])"
  (save-excursion
    (let* ((origin (point))
           ;; Search back [[ and record location of ][
           (type-desc-beg (org-link-completion-search-back-beginning-of-link))
           (type-beg (car type-desc-beg))
           (desc-beg (cdr type-desc-beg)))
      (when type-beg ;; [[ was found
        (let ((type-end-maybe (org-link-completion-search-forward-end-of-type)))
          (if (<= origin type-end-maybe)
              ;; [[<type>
              (list 'type type-beg type-end-maybe)
            ;; After writing <type>
            (let* ((path-end (org-link-completion-search-forward-end-of-path))
                   (type-colon-end
                    (if org-link-completion-accept-undefined-type
                        ;; Followed by a colon?
                        (and (eq (char-after type-end-maybe) ?:)
                             (1+ (point)))
                      ;; Defined in `org-link-parameters'?
                      (save-match-data
                        (when (string-match org-link-types-re
                                            (buffer-substring-no-properties
                                             type-beg path-end))
                          (+ type-beg (match-end 0))))))
                   (type-end (if type-colon-end (1- type-colon-end) type-beg))
                   (path-beg (if type-colon-end type-colon-end type-beg)))

              (cond
               ;; [[<type>:<path>
               ;; [[#customid
               ;; [[*heading
               ;; [[/home/test.org
               ;; [[My Target
               ((<= origin path-end) (list 'path type-beg type-end path-beg path-end))
               ;; [[<type>:<path>]
               ((null desc-beg) nil)
               ;; Reject invalid syntax [[path\][desc , [[pa[th][desc, [[pa]th][desc
               ((/= (- desc-beg 2) path-end) nil)
               ;; [[<type>:<path>][
               (t
                ;; Find ]] or \n
                (goto-char desc-beg)
                (let ((desc-end
                       (org-link-completion-search-forward-end-of-desc)))
                  ;; [[<type>:<path>][<description>
                  (when (<= origin desc-end)
                    (list 'desc type-beg type-end path-beg path-end desc-beg desc-end)
                    )))))))))))

(defun org-link-completion-search-back-beginning-of-link ()
  "Search back `[[' and `]['.
Return a cons cell containing the positions of `[[' and `]]'.
The search stops when it reaches `[[', `]]', `\n' or BOB.
A nil is placed in the position that has not been found so far."
  (let (type-beg desc-beg)
    (while (progn
             (skip-chars-backward "^\n][")
             (pcase (char-before)
               (?\[ (pcase (char-before (1- (point)))
                      ;; ][ => record & skip
                      (?\] (setq desc-beg (point)) (backward-char 2) t)
                      ;; [[ => stop
                      (?\[ (setq type-beg (point)) nil)
                      ;; ?[ => skip
                      (_   (backward-char) t)))
               (?\] (if (or (eq (char-before (1- (point))) ?\])
                            (eq (char-after (point)) ?\]))
                        ;; ]] => stop
                        nil
                      ;; ?] => skip
                      (backward-char)
                      t))
               ;; \n or nil => stop
               (_   nil))))
    (cons type-beg desc-beg)))

(defconst org-link-completion-type-chars "-A-Za-z0-9_+")

(defun org-link-completion-search-forward-end-of-type ()
  (skip-chars-forward
   org-link-completion-type-chars)
  (point))

(defun org-link-completion-search-forward-end-of-path ()
  ;; Search forward ]
  ;; Skip escape sequence \\ \] \[ or not \ ] [ \n
  (while (progn
           (skip-chars-forward "^\n][\\\\")
           (when (eq (char-after) ?\\)
             (forward-char)
             (when (memq (char-after)
                         '(?\\ ?\[ ?\]))
               (forward-char))
             t)))
  (point))

(defun org-link-completion-search-forward-end-of-desc ()
  ;; Find ]] or \n
  (while (progn
           (skip-chars-forward "^]\n")
           (when (and (eq (char-after) ?\])
                      (not (eq (char-after (1+ (point))) ?\])))
             (forward-char)
             t)))
  (point))


(defvar org-link-completion-pos nil
  "Temporarily hold result of `org-link-completion-parse-at-point'
function.")

(defmacro org-link-completion-pos-ref (pos pname &optional where)
  "Return PNAME property of parsing result POS at WHERE.

POS is a return value of `org-link-completion-parse-at-point'.

PNAME is one of the following:

- where
- type-beg
- type-end
- path-beg
- path-end
- desc-beg
- desc-end
- type
- path
- desc

Example: (org-link-completion-pos-ref pos path-beg)"
  (let* ((path-p (memq where '(nil :desc :path)))
         (desc-p (memq where '(nil :desc)))
         (pos-sym (if (symbolp pos) pos (gensym "pos-")))
         (pos-refs nil)
         (expr
          (pcase pname
            ('where    (setq pos-refs 1) `(nth 0 ,pos-sym))
            ('type-beg (setq pos-refs 1) `(nth 1 ,pos-sym))
            ('type-end (setq pos-refs 1) `(nth 2 ,pos-sym))
            ('path-beg (setq pos-refs 1) (and path-p `(nth 3 ,pos-sym)))
            ('path-end (setq pos-refs 1) (and path-p `(nth 4 ,pos-sym)))
            ('desc-beg (setq pos-refs 1) (and desc-p `(nth 5 ,pos-sym)))
            ('desc-end (setq pos-refs 1) (and desc-p `(nth 6 ,pos-sym)))
            ('type `(buffer-substring-no-properties (nth 1 ,pos-sym)
                                                    (nth 2 ,pos-sym)))
            ('path (and
                    path-p
                    `(buffer-substring-no-properties (nth 3 ,pos-sym)
                                                     (nth 4 ,pos-sym))))
            ('desc (and
                    desc-p
                    `(buffer-substring-no-properties (nth 5 ,pos-sym)
                                                     (nth 6 ,pos-sym)))))))
    (unless expr
      (error "Unknown property `%s' for org-link-completion-parsed" pname))
    (if (or (eq pos-sym pos) (eq pos-refs 1))
        expr
      `(let ((,pos-sym ,pos)) ,expr))))

(defmacro org-link-completion-parse-let (where prop-names &rest body)
  "Parse link text and bind variables to results.

Assuming that point is at the WHERE position of the link, parse
the surrounding area, extract the property specified by
PROP-NAMES from the result, and evaluate BODY.

WHERE is the expected point location and can be specified as nil,
:type, :path, or :desc. If the point is not in the specified part
of the link, do nothing. nil means to accept any part."
  (declare (indent 2))
  (let* ((pos-sym (gensym "pos"))
         (pos-type (pcase where
                     (:type 'type)
                     (:path 'path)
                     (:desc 'desc)
                     ('nil nil)
                     (_ (error "Invalid where=%s" where))))
         (bindings (cl-loop for pname in prop-names
                            collect
                            `(,pname
                              (org-link-completion-pos-ref
                               ,pos-sym ,pname ,where)))))

    `(let ((,pos-sym (or org-link-completion-pos
                         (org-link-completion-parse-at-point))))
       ,(if pos-type
            `(when (eq (car-safe ,pos-sym) ',pos-type)
               (let ,bindings
                 ,@body))
          `(let ,bindings
             ,@body)))))


;;;; Completion At Point Function

;; See: `org-link-completion-at-point' function.

(defcustom org-link-completion-type-function
  #'org-link-completion-type
  "Function to complete link type part."
  :group 'org-link-completion-functions
  :type 'function)

(defcustom org-link-completion-path-untyped-function
  #'org-link-completion-path-untyped
  "Function to complete path of a link that does not have type specification."
  :group 'org-link-completion-functions
  :type 'function)

(defcustom org-link-completion-desc-untyped-function
  #'org-link-completion-desc-untyped
  "Function to complete description of a link that does not have
 type specification."
  :group 'org-link-completion-functions
  :type 'function)

(defcustom org-link-completion-path-unknown-type-function
  #'org-link-completion-path-unknown-type
  "Function to complete path of a link that is unknown type."
  :group 'org-link-completion-functions
  :type 'function)

(defcustom org-link-completion-desc-unknown-type-function
  #'org-link-completion-desc-unknown-type
  "Function to complete description of a link that is unknown type."
  :group 'org-link-completion-functions
  :type 'function)

;;;###autoload
(defun org-link-completion-at-point ()
  "Complete the path or description part of a link on point.

If point is on a link (including unfinished links), the
appropriate function is called depending on the part and link
type.

[[<type>:<path>][<desc>]]

- point is on <type> => call `org-link-completion-type-function' variable

- <type> is empty:

  - point is on <path> => call `org-link-completion-path-untyped-function'
  - point is on <desc> => call `org-link-completion-desc-untyped-function'

- <type> is a valid link type:

  call one of functions set to the following properties of
  `org-link-parameters'.

  - `:capf-path' : point is on <path>
  - `:capf-desc' : point is on <desc>
  - `:completion-at-point' : If the above properties are not present.

- No completion function found for <type>:

  - point is on <path> => call `org-link-completion-path-unknown-type-function'
  - point is on <desc> => call `org-link-completion-desc-unknown-type-function'

No arguments are passed to the function. However, before it is
called, the variable `org-link-completion-pos' is set with
information about the link to be completed.

The function must return the same format as functions added to
`completion-at-point-functions'.

To use this, do the following in org-mode buffer:
  (add-hook \\='completion-at-point-functions
            #\\='org-link-completion-at-point nil t)"
  (when-let ((pos (org-link-completion-parse-at-point)))
    (let* ((org-link-completion-pos pos)
           (where (org-link-completion-pos-ref pos where))
           (type (org-link-completion-pos-ref pos type)))
      (cond
       ((eq where 'type)
        (org-link-completion-call org-link-completion-type-function))
       ((string-empty-p type)
        (pcase where
          ('path (org-link-completion-call
                  org-link-completion-path-untyped-function))
          ('desc (org-link-completion-call
                  org-link-completion-desc-untyped-function))))
       (t
        (let* ((capf-prop (pcase where
                            ('path :capf-path)
                            ('desc :capf-desc)))
               (capf (or (org-link-get-parameter type capf-prop)
                         (org-link-get-parameter type :completion-at-point)
                         (pcase where
                           ('path org-link-completion-path-unknown-type-function)
                           ('desc org-link-completion-desc-unknown-type-function)
                           ))))
          ;;(message "capf=%s" capf)
          (org-link-completion-call capf)))))))


;;;; Complete Link Type Part

(defcustom org-link-completion-type-collectors
  '(org-link-completion-collect-type-part-internal-link-prefix-chars
    org-link-completion-collect-types
    ;; Consider the possibility that <type> is <target>.
    ;; For example: [[mytarget .
    org-link-completion-collect-search-target)
  "Functions that collect candidates for first part of link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

(defun org-link-completion-type ()
  "Complete [[#<type>: part of link at point."
  (org-link-completion-parse-let :type (type-beg type-end)
    (org-link-completion-capf-result
     type-beg
     (if (eq (char-after type-end) ?:) (1+ type-end) type-end)
     (org-link-completion-call-collectors
      org-link-completion-type-collectors))))

(defun org-link-completion-collect-type-part-internal-link-prefix-chars ()
  (list
   (org-link-completion-annotate "#" "CUSTOM_ID")
   (org-link-completion-annotate "*" "Heading")
   (org-link-completion-annotate "(" "Coderef")))

(defun org-link-completion-collect-types ()
  (mapcar (lambda (e) (concat (car e) ":"))
          (append org-link-abbrev-alist-local
                  org-link-abbrev-alist
                  org-link-parameters)))


;;;; Complete Untyped Links

;; Complete links without <type>: part.

;; `org-link-completion-path-untyped'
;;
;; Internal Links
;;  [[#custom-id       => `org-link-completion-path-custom-id'
;;   ][<description>]] => `org-link-completion-desc-custom-id'
;;  [[*heading         => `org-link-completion-path-heading'
;;   ][<description>]] => `org-link-completion-desc-heading'
;;  [[(coderef)        => `org-link-completion-path-coderef'
;;   ][<description>]] => `org-link-completion-desc-coderef'
;;  [[My Target"       => `org-link-completion-path-search'
;;   ][<description>]] => `org-link-completion-desc-search'
;;
;; External Links
;;  [[./file           => `org-link-completion-path-file' (same as file:)
;;   ][<description>]] => `org-link-completion-desc-file' (same as file:)

(defun org-link-completion-untyped-link-kind (beg end)
  (when (< beg end)
    (pcase (char-after beg)
      ;; #<custom id>
      (?# 'custom-id)
      ;; *<heading>
      (?* 'heading)
      ;; (<coderef>)
      (?\( 'coderef)
      ;; <file>
      ;; See: `org-element-link-parser'
      ;; Relative path: ./ ../  (Not a path: . .. .\ ..\)
      (?.
       (if (pcase (char-after (1+ beg))
             (?/ t)
             (?. (eq (char-after (+ 2 beg)) ?/)))
           'file
         'search))
      (_
       ;; Absolute path: /* ~/* ~<user>/* ~ ~<user>
       ;;  (MS-Windows): \* ~\* ~<user>\* <drive>:/* <drive>:\*
       ;; TODO: Add customization variable to select platform.
       (if (file-name-absolute-p (buffer-substring-no-properties beg end))
           'file
         ;; <target>
         'search)))))

;;;;; Untyped Path

(defcustom org-link-completion-path-untyped-kind-functions
  '((custom-id . org-link-completion-path-custom-id)
    (heading . org-link-completion-path-heading)
    (coderef . org-link-completion-path-coderef)
    (search . org-link-completion-path-search)
    (file . org-link-completion-path-file))
  "Alist of functions to complete path for each kind of untyped link."
  :group 'org-link-completion-functions
  :type 'alist)

(defun org-link-completion-path-untyped ()
  "Complete <path> of link that does not have <type>: at point.

For example:
  [[#custom-id
  [[*heading
  [[(coderef)
  [[My Target
  [[./file"
  (org-link-completion-parse-let :path (path-beg path-end)
    (org-link-completion-path-call-on-kind
     path-beg path-end
     org-link-completion-path-untyped-kind-functions)))

(defun org-link-completion-path-call-on-kind (path-beg path-end alist)
  (org-link-completion-call
   (alist-get (org-link-completion-untyped-link-kind path-beg path-end)
              alist)))

;;;;;; Custom-ID Path

(defcustom org-link-completion-path-custom-id-collectors
  '(org-link-completion-collect-custom-id)
  "Functions that collect completion candidates of path in CUSTOM-ID link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

(defun org-link-completion-path-custom-id ()
  "Complete [[#<custom-id> part of link at point."
  (org-link-completion-parse-let :path (path-beg path-end)
    (org-link-completion-capf-result
     (1+ path-beg) path-end
     (org-link-completion-call-collectors
      org-link-completion-path-custom-id-collectors)
     :kind 'keyword)))

(defun org-link-completion-collect-custom-id ()
  "Collect all :CUSTOM_ID: property values from the current buffer."
  (save-excursion
    (goto-char (point-min))
    ;; See: `org-find-property'
    (let ((case-fold-search t)
          (re (org-re-property "CUSTOM_ID" nil nil nil)))
      (cl-loop while (re-search-forward re nil t)
               for id = (org-entry-get (point) "CUSTOM_ID" nil t)
               when id
               collect id))))

;;;;;; Heading Path

(defcustom org-link-completion-path-heading-collectors
  '(org-link-completion-collect-heading)
  "Functions that collect completion candidates of path in heading link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

(defun org-link-completion-path-heading ()
  "Complete [[*<heading> part of link at point."
  ;; NOTE: There is already an implementation in
  ;; `pcomplete/org-mode/searchhead'
  (org-link-completion-parse-let :path (path-beg path-end)
    (org-link-completion-capf-result
     (1+ path-beg) path-end
     (org-link-completion-call-collectors
      org-link-completion-path-heading-collectors)
     :kind 'folder)))

(defun org-link-completion-collect-heading ()
  "Collect all heading text from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward (concat "^" org-outline-regexp) nil t)
             for heading = (ignore-errors
                              (org-link--normalize-string
                               (substring-no-properties
                                ;; or nil
                                (org-get-heading t t t t))))
             when heading
             collect heading)))

;;;;;; Coderef Path

(defcustom org-link-completion-path-coderef-collectors
  '(org-link-completion-collect-coderef)
  "Functions that collect completion candidates of path in coderef link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

(defun org-link-completion-path-coderef ()
  "Complete [[(<coderef>) part of link at point."
  ;; NOTE: There is already an implementation in
  ;; `pcomplete/org-mode/searchhead'
  (org-link-completion-parse-let :path (path-beg path-end)
    (org-link-completion-capf-result
     (1+ path-beg) path-end
     (org-link-completion-call-collectors
      org-link-completion-path-coderef-collectors)
     :kind 'reference)))

(defun org-link-completion-collect-coderef ()
  "Collect all coderef labels (ref:<label>) from the current buffer."
  ;; Ref: `org-link-search'
  (save-excursion
    (goto-char (point-min))
    (let (result
          (re-block (concat "\\(?:"
                            org-babel-src-block-regexp
                            "\\)\\|\\(?:"
                            ;; TODO: OK?
                            (replace-regexp-in-string
                             "_src" "_example" org-babel-src-block-regexp t t)
                            "\\)")))
      ;; For each code block
      (while (re-search-forward re-block nil t)
        (goto-char (match-beginning 0))
        (let ((block-beg (match-beginning 0))
              (block-end (match-end 0))
              (element (org-element-at-point))) ;;NOTE: Change match-data
          (when (and (memq (org-element-type element)
                           '(src-block example-block))
                     (<= block-beg
                         (org-element-property :post-affiliated element)))
            ;; For each coderef
            (let ((re-coderef (concat ".*?"
                                      (org-src-coderef-regexp
                                       (org-src-coderef-format element)))))
              (while (re-search-forward re-coderef block-end t)
                (push (match-string-no-properties 3) result))))
          (goto-char block-end)))
      (nreverse result))))


;;;;;; Search Target Path

(defcustom org-link-completion-path-search-collectors
  ;; Ignore headings, all words.
  '(org-link-completion-collect-dedicated-target
    org-link-completion-collect-element-names)
  "Functions that collect completion candidates of path in search link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

(defun org-link-completion-path-search ()
  "Complete `[[My Target' part of link at point.

NOTE: `[[mytarget' is treated as a link type named `mytarget:'."
  (org-link-completion-parse-let :path (path-beg path-end)
    (org-link-completion-capf-result
     path-beg path-end
     (org-link-completion-collect-search-target)
     :kind 'text)))

(defun org-link-completion-collect-search-target ()
  "Collect all search target strings from the current buffer."
  (org-link-completion-call-collectors
   org-link-completion-path-search-collectors))

(defun org-link-completion-collect-dedicated-target ()
  "Collect all dedicated target (<<target>>) from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward org-target-regexp nil t)
             for target = (org-link-completion-annotate
                           (match-string-no-properties 1)
                           "Dedicated")
             when target
             collect target)))

(defun org-link-completion-collect-element-names ()
  "Collect all element names (#+NAME:) from the current buffer."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (re-search-forward "^[ \t]*#\\+NAME:[ \t]*\\(.*?\\)[ \t]*$"
                                      nil t)
             collect (org-link-completion-annotate
                      (match-string-no-properties 1)
                      "Name"))))

;;;;; Untyped Description

(defcustom org-link-completion-desc-untyped-kind-functions
  '((custom-id . org-link-completion-desc-custom-id)
    (heading . org-link-completion-desc-heading)
    (coderef . org-link-completion-desc-coderef)
    (search . org-link-completion-desc-search)
    (file . org-link-completion-desc-file))
  "Alist of functions to complete description for each kind of untyped link."
  :group 'org-link-completion-functions
  :type 'alist)

(defun org-link-completion-desc-untyped ()
  (org-link-completion-parse-let :desc (path-beg path-end)
    (org-link-completion-path-call-on-kind
     path-beg path-end
     org-link-completion-desc-untyped-kind-functions)))

;;;;;; Custom-ID Description

(defcustom org-link-completion-desc-custom-id-collectors
  '(org-link-completion-collect-description-from-other-links
    org-link-completion-collect-description-from-favorite-links
    org-link-completion-collect-custom-id-desc-from-around-target
    org-link-completion-collect-stripped-internal-link-path)
  "Functions that collect candidates of description in CUSTOM-ID link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

(defun org-link-completion-desc-custom-id ()
  "Complete [[#custom-id][<description> at point."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-call-collectors
      org-link-completion-desc-custom-id-collectors)
     :kind 'text)))

(defun org-link-completion-collect-custom-id-desc-from-around-target ()
  (org-link-completion-parse-let :desc (path)
    ;; Extract from target location
    (save-excursion
      (when (org-link-completion-link-search path)
        (org-link-completion-string-list
         (org-link-completion-get-heading))))))

;;;;;; Heading Description

(defcustom org-link-completion-desc-heading-collectors
  '(org-link-completion-collect-description-from-other-links
    org-link-completion-collect-description-from-favorite-links
    org-link-completion-collect-stripped-internal-link-path)
  "Functions that collect candidates of description in heading link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

(defun org-link-completion-desc-heading ()
  "Complete [[*heading][<description> at point."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-call-collectors
      org-link-completion-desc-heading-collectors)
     :kind 'text)))

;;;;;; Search Target Description

(defcustom org-link-completion-desc-search-collectors
  '(org-link-completion-collect-description-from-other-links
    org-link-completion-collect-description-from-favorite-links
    org-link-completion-collect-search-desc-from-around-target
    org-link-completion-collect-stripped-internal-link-path)
  "Functions that collect candidates of description in search link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

(defun org-link-completion-desc-search ()
  "Complete [[My Target][<description> at point."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-call-collectors
      org-link-completion-desc-search-collectors)
     :kind 'text)))

(defun org-link-completion-collect-search-desc-from-around-target ()
  (org-link-completion-parse-let :desc (path)
    ;; Extract from target location
    (save-excursion
      (when (org-link-completion-link-search path)
        (nconc
         ;; Current line text
         (org-link-completion-call
          'org-link-completion-collect-search-desc-from-current-line)

         ;; Heading
         (org-link-completion-call
          'org-link-completion-collect-search-desc-from-heading))))))

(defun org-link-completion-collect-search-desc-from-heading ()
  (org-link-completion-string-list
   (org-link-completion-get-heading)))

(defun org-link-completion-collect-search-desc-from-current-line ()
  (org-link-completion-string-list
   (org-link-completion-annotate
    (org-link-completion-escape-description-string
     (string-trim
      ;; Remove <<...>> not <<<...>>>.
      (replace-regexp-in-string
       (concat "\\(?:[^<]\\|^\\)\\("
               org-target-regexp
               "\\)\\(?:[^>]\\|$\\)")
       ""
       (buffer-substring-no-properties (line-beginning-position)
                                       (line-end-position))
       nil nil 1)
      ;; Strip bullets and table separators
      "\\(?:[ \t\n\r]*[|-]\\)*[ \t]*"
      "\\(?:[ \t]*|\\)*[ \t]*"))
    "Line")))

;;;;;; Coderef Description

(defcustom org-link-completion-desc-coderef-collectors
  '(org-link-completion-collect-description-from-other-links
    org-link-completion-collect-description-from-favorite-links
    org-link-completion-collect-default-coderef-description
    org-link-completion-collect-path
    org-link-completion-collect-coderef-desc-from-around-target)
  "Functions that collect candidates of description in coderef link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

(defun org-link-completion-desc-coderef ()
  "Complete [[(coderef)][<description> at point."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-call-collectors
      org-link-completion-desc-coderef-collectors)
     :kind 'text)))

(defun org-link-completion-collect-coderef-desc-from-around-target ()
  (org-link-completion-parse-let :desc (path)
    ;; Extract from target location
    (save-excursion
      (when (org-link-completion-link-search path)
        (nconc
         ;; Current line text
         (org-link-completion-call
          'org-link-completion-collect-coderef-desc-from-current-line)

         ;; Heading
         (org-link-completion-call
          'org-link-completion-collect-coderef-desc-from-heading))))))


(defun org-link-completion-collect-coderef-desc-from-heading ()
  (org-link-completion-string-list
   (org-link-completion-get-heading)))

(defun org-link-completion-collect-coderef-desc-from-current-line ()
  (org-link-completion-string-list
   (org-link-completion-annotate
    (org-link-completion-escape-description-string
     (string-trim
      ;; Remove coderef target from the line.
      (replace-regexp-in-string
       (org-src-coderef-regexp (org-src-coderef-format
                                (org-element-at-point)))
       ""
       (buffer-substring-no-properties (line-beginning-position)
                                       (line-end-position)))))
    "Line")))

(defun org-link-completion-collect-default-coderef-description ()
  "Line (<coderef>)"
  (org-link-completion-parse-let :desc (path) ;; "(<coderef>)" format
    (org-link-completion-string-list
     (org-link-completion-annotate
      (org-link-completion-default-coderef-description path)
      "Format"))))

(defconst org-link-completion-default-coderef-description-format-dictionary
  '(("Japanese" . "%s行目")))

(defcustom org-link-completion-default-coderef-description-format
  (alist-get
   ;; Should I use `org-export-default-language'? (Is it loaded at this point?)
   current-language-environment
   org-link-completion-default-coderef-description-format-dictionary
   "Line %s"
   nil #'equal)
  "Default description format for coderef links."
  :group 'org-link-completion
  :type 'string)

(defun org-link-completion-default-coderef-description (label)
  (format org-link-completion-default-coderef-description-format label))


;;;; Complete Typed links

;; Complete links with <type>: part.

;;;;; Unknown Type

(defcustom org-link-completion-path-unknown-collectors
  '(org-link-completion-collect-path-from-other-links
    org-link-completion-collect-path-from-favorite-links)
  "Functions that collect candidates of path in unknown link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

(defun org-link-completion-path-unknown-type ()
  (org-link-completion-parse-let :path (path-beg path-end)
    (org-link-completion-capf-result
     path-beg path-end
     (org-link-completion-call-collectors
      org-link-completion-path-unknown-collectors)
     :kind 'text)))

(defcustom org-link-completion-desc-unknown-collectors
  '(org-link-completion-collect-description-from-other-links
    org-link-completion-collect-description-from-favorite-links)
  "Functions that collect candidates of description in unknown link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

(defun org-link-completion-desc-unknown-type ()
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-call-collectors
      org-link-completion-desc-unknown-collectors)
     :kind 'text)))


;;;;; File Type

;;;###autoload
(defun org-link-completion-setup-type-file ()
  (dolist (type '("file" "file+sys" "file+emacs"))
    (org-link-set-parameters
     type
     :capf-path 'org-link-completion-path-file
     :capf-desc 'org-link-completion-desc-file)))

(defcustom org-link-completion-path-file-functions
  '((custom-id . org-link-completion-path-file-custom-id)
    (heading . org-link-completion-path-file-heading)
    (coderef . org-link-completion-path-file-coderef)
    (search . org-link-completion-path-file-search)
    ;;(regexp . org-link-completion-path-file-regexp)
    (file . org-link-completion-path-file-file))
  "Alist of functions to complete path for each kind of file link."
  :group 'org-link-completion-functions
  :type 'alist)

(defun org-link-completion-path-file-option-beg (path-beg path-end)
  (save-excursion
    (goto-char path-beg)
    (when (search-forward "::" path-end t)
      (point))))

;;;###autoload
(defun org-link-completion-path-file ()
  "Complete <filename> of [[<type>:<filename> at point.

This function also works for `file+sys:' and `file+emacs:' link types."
  (org-link-completion-parse-let :path (path-beg path-end)
    (let* ((option-beg
            (org-link-completion-path-file-option-beg path-beg path-end))
           (has-option (and option-beg (<= option-beg (point))))
           (file (org-link-completion-file-expand-empty
                  (buffer-substring-no-properties
                   path-beg
                   (if option-beg (- option-beg 2) path-end))))
           (kind (if has-option
                     (pcase (char-after option-beg)
                       (?# 'custom-id)
                       (?* 'heading)
                       (?\( 'coderef)
                       (?/ 'regexp)
                       (_ 'search))
                   'file))
           (fun (alist-get kind org-link-completion-path-file-functions)))
      (org-link-completion-call fun path-beg path-end option-beg file))))

(defun org-link-completion-path-file-file (path-beg path-end option-beg _file)
  (list
   path-beg (if option-beg (- option-beg 2) path-end)
   #'read-file-name-internal
   :annotation-function
   (lambda (str) (if (string-suffix-p "/" str) " Dir" " File"))
   :company-kind
   (lambda (str) (if (string-suffix-p "/" str) 'folder 'file))
   :exclusive 'no))

(defun org-link-completion-path-file-custom-id (_path-beg
                                                path-end option-beg file)
  (org-link-completion-path-file-option-collect
   #'org-link-completion-collect-custom-id
   file (1+ option-beg) path-end)) ;; Skip #

(defun org-link-completion-path-file-heading (_path-beg
                                              path-end option-beg file)
  (org-link-completion-path-file-option-collect
   #'org-link-completion-collect-heading
   file (1+ option-beg) path-end)) ;; Skip *

(defun org-link-completion-path-file-coderef (_path-beg
                                              path-end option-beg file)
  (org-link-completion-path-file-option-collect
   #'org-link-completion-collect-coderef
   file (1+ option-beg) path-end)) ;; Skip ( ;; TODO: path-end? before close paren? (coderef)

(defun org-link-completion-path-file-search (_path-beg
                                             path-end option-beg file)
  (org-link-completion-path-file-option-collect
   #'org-link-completion-collect-search-target
   file option-beg path-end
   (list
    (org-link-completion-annotate "#" "CUSTOM_ID")
    (org-link-completion-annotate "*" "Heading")
    (org-link-completion-annotate "(" "Coderef")
    (org-link-completion-annotate "/" "Regexp"))))

(defun org-link-completion-path-file-option-collect (fun
                                                     file
                                                     completion-beg path-end
                                                     &optional additional)
  (org-link-completion-capf-result
   completion-beg path-end
   (nconc
    additional
    (org-link-completion-call-with-file-find file fun))
   :kind 'text))


;; Description

(defcustom org-link-completion-desc-file-collectors
  '(org-link-completion-collect-description-from-other-links
    org-link-completion-collect-description-from-favorite-links
    org-link-completion-collect-org-file-title
    ;; TODO: Get titles from more file formats
    org-link-completion-collect-file-name
    org-link-completion-collect-file-base
    org-link-completion-collect-path
    org-link-completion-collect-file-full-path)
  "Functions that collect candidates of description in file link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

;;;###autoload
(defun org-link-completion-desc-file ()
  "Complete <filename> of [[<type>:<filename>][<description> at point."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-call-collectors
      org-link-completion-desc-file-collectors)
     :kind 'text)))

(defun org-link-completion-file-path-part (path)
  (org-link-completion-file-expand-empty
   (org-link-completion-file-without-options path)))

(defun org-link-completion-file-without-options (path)
  ;; Find first `::' (See: `org-link-open-as-file')
  (if (string-match "\\`\\(.*?\\)::" path)
      (match-string 1 path)
    path))

(defun org-link-completion-file-expand-empty (path)
  "Expand empty filename to current buffer's filename.

An example of an empty filename is: [[file:::*Heading]]"
  (if (string-empty-p path)
      (org-link-completion-get-current-file)
    path))

(defun org-link-completion-collect-file-name ()
  (org-link-completion-parse-let :desc (path)
    (when-let ((filepath (org-link-completion-file-path-part path)))
      (org-link-completion-string-list
       (org-link-completion-annotate
        (file-name-nondirectory filepath)
        "File Name")))))

(defun org-link-completion-collect-file-base ()
  (org-link-completion-parse-let :desc (path)
    (when-let ((filepath (org-link-completion-file-path-part path)))
      (let ((filename (file-name-nondirectory filepath)))
        (when (string-match "\\`\\(.[^.]*\\)" filename)
          (org-link-completion-string-list
           (org-link-completion-annotate
            (match-string 1 filename)
            "Base Name")))))))

(defun org-link-completion-collect-file-full-path ()
  (org-link-completion-parse-let :desc (path)
    (when-let ((filepath (org-link-completion-file-path-part path)))
      (org-link-completion-string-list
       (org-link-completion-annotate
        (expand-file-name filepath)
        "Full Path")))))

(defun org-link-completion-collect-org-file-title ()
  (org-link-completion-parse-let :desc (path)
    (ignore-errors
      (org-link-completion-string-list
       (org-link-completion-annotate
        (if-let ((filepath (org-link-completion-file-path-part path)))
            (when (string-match-p "\\.org\\'" filepath)
              (org-link-completion-call-with-file
               filepath nil #'org-link-completion-get-org-title nil
               ;; It's probably near the top
               ;; TODO: Customize
               16384))
          ;; From current buffer
          (when (derived-mode-p 'org-mode)
            (org-link-completion-get-org-title)))
        "Title")))))

(defun org-link-completion-get-org-title ()
  "Read org-mode title from Current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward
             "^#\\+TITLE: *\\(.*\\)$" nil t)
        (match-string-no-properties 1)))))


;;;;; ID Type

;; Setup

;;;###autoload
(defun org-link-completion-setup-type-id ()
  (org-link-set-parameters
   "id"
   :capf-path 'org-link-completion-path-id
   :capf-desc 'org-link-completion-desc-id))

;; Path

;;;###autoload
(defun org-link-completion-path-id ()
  "Complete <id> of [[id:<id> at point."
  (org-link-completion-parse-let :path (path-beg path-end)
    ;;(message "Enter org-link-completion-path-id")
    (org-link-completion-capf-result
     path-beg path-end
     (org-link-completion-table-with-alist-search
      #'org-link-completion-get-id-cache)
     :kind 'text)))

;; Collect ID and Headings

(defcustom org-link-completion-collect-id-use-find-file-noselect t
  "Non-nil means that when collecting IDs and headings, open
unvisited files with find-file-noselect.

After processing, a buffer of the files used for collection
remains, but processing will be faster the next time."
  :group 'org-link-completion
  :type 'boolean)

(defun org-link-completion-collect-id-heading-alist ()
  (let ((files (org-link-completion-get-id-files))
        (current-file (org-link-completion-get-current-file))
        (alist)
        (case-fold-search t)
        (re (org-re-property "ID" nil nil nil)))
    ;; Include current file and put it at the beginning.
    (when current-file
      (setq files (cons current-file (delete current-file files))))
    (dolist (file files)
      (org-link-completion-call-with-file
       file 'org-mode
       (lambda ()
         (while (re-search-forward re nil t)
           (when-let ((id (org-entry-get (point) "ID" nil t)))
             (unless (assoc id alist #'string=) ;; TODO: Ignore case?
               (when-let ((heading
                           (org-link-completion-collect-id-heading-on-entry
                            file)))
                 (let* ((heading (substring-no-properties heading))
                        (id (org-link-completion-annotate id heading)))
                   (push (cons id heading) alist)))))))
       org-link-completion-collect-id-use-find-file-noselect))
    (nreverse alist)))

(defun org-link-completion-collect-id-heading-on-entry (file)
  (when-let ((heading (org-get-heading t t t t)))
    ;; TODO: Customize
    ;; <heading> - <filename>
    (concat (substring-no-properties heading)
            (and file (concat " - " (file-name-nondirectory file))))))

(defun org-link-completion-get-id-files ()
  (require 'org-id)
  (defvar org-id-locations)
  (unless org-id-locations (org-id-locations-load))
  (when (and org-id-locations
             (hash-table-p org-id-locations))
    (let (files)
      (maphash (lambda (_id file)
                 (let ((abs-file (expand-file-name file)))
                   (unless (member abs-file files)
                     (push abs-file files))))
               org-id-locations)
      files)))

;; Cache

(defvar org-link-completion-path-id--cached-p nil)
(defvar org-link-completion-path-id--cache nil)
(defvar org-link-completion-path-id--cache-time nil)
(defconst org-link-completion-path-id--cache-timeout 60)

(defun org-link-completion-clear-id-cache-hook ()
  ;;(message "clear-id-cache in-region-mode=%s" completion-in-region-mode)
  (when (null completion-in-region-mode)
    (org-link-completion-clear-id-cache)))

(defun org-link-completion-clear-id-cache ()
  (when org-link-completion-path-id--cached-p
    (setq org-link-completion-path-id--cached-p nil
          org-link-completion-path-id--cache nil
          org-link-completion-path-id--cache-time nil)
    (remove-hook 'completion-in-region-mode-hook
                 'org-link-completion-clear-id-cache-hook)))

(defun org-link-completion-get-id-cache ()
  ;; Check timeout
  (when org-link-completion-path-id--cached-p
    (if (>= (- (float-time) org-link-completion-path-id--cache-time)
            org-link-completion-path-id--cache-timeout)
        ;; Timeout
        (org-link-completion-clear-id-cache)
      ;; Update time
      (setq org-link-completion-path-id--cache-time (float-time))))

  ;; Create cache
  (unless org-link-completion-path-id--cached-p
    (setq org-link-completion-path-id--cache
          (org-link-completion-collect-id-heading-alist)
          org-link-completion-path-id--cache-time (float-time)
          org-link-completion-path-id--cached-p t)
    (add-hook 'completion-in-region-mode-hook
              'org-link-completion-clear-id-cache-hook))

  ;; Return cache
  org-link-completion-path-id--cache)

;; Description

(defcustom org-link-completion-desc-id-collectors
  '(org-link-completion-collect-description-from-other-links
    org-link-completion-collect-description-from-favorite-links
    org-link-completion-collect-heading-by-id)
  "Functions that collect candidates of description in id link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

;;;###autoload
(defun org-link-completion-desc-id ()
  "Complete <desc> of [[id:<id>][<desc> at point."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-call-collectors
      org-link-completion-desc-id-collectors)
     :kind 'text)))

(defun org-link-completion-collect-heading-by-id ()
  (org-link-completion-parse-let :desc (path)
    (org-link-completion-string-list
     (org-link-completion-get-heading-by-id path))))

(defun org-link-completion-get-heading-by-id (id)
  (when-let ((file (org-id-find-id-file id))) ;; or current file
    (org-link-completion-call-with-file
     file 'org-mode
     (lambda ()
       (when-let ((pos (org-find-property "ID" id)))
         (goto-char pos)
         (when-let ((heading (org-get-heading t t t t)))
           (substring-no-properties heading)))))))


;;;;; Help Type

;; Reference:
;; `org-link--open-help'
;; `org-link--store-help'

;; Setup

;;;###autoload
(defun org-link-completion-setup-type-help ()
  (org-link-set-parameters
   "help"
   :capf-path 'org-link-completion-path-help
   :capf-desc 'org-link-completion-desc-help))

;; Path

;;;###autoload
(defun org-link-completion-path-help ()
  "Complete <function-or-variable> of [[help:<function-or-variable> at point."
  (org-link-completion-parse-let :path (path-beg path-end)
    (list
     path-beg path-end
     (elisp--completion-local-symbols)
     :predicate (lambda (sym) (or (fboundp sym)
                                  (boundp sym)))
     :company-kind #'elisp--company-kind
     :company-doc-buffer #'elisp--company-doc-buffer
     :company-docsig #'elisp--company-doc-string
     :company-location #'elisp--company-location
     :company-deprecated #'elisp--company-deprecated)))

;; Description

(defcustom org-link-completion-desc-help-collectors
  '(org-link-completion-collect-description-from-other-links
    org-link-completion-collect-description-from-favorite-links
    org-link-completion-collect-path)
  "Functions that collect candidates of description in help link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

;;;###autoload
(defun org-link-completion-desc-help ()
  "Complete <desc> of [[help:<symbol>][<desc> at point."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-call-collectors
      org-link-completion-desc-help-collectors)
     :kind 'text)))


;;;;; Elisp Type

;; Reference:
;; `org-link--open-elisp'

;; Setup

;;;###autoload
(defun org-link-completion-setup-type-elisp ()
  (org-link-set-parameters
   "elisp"
   :capf-path 'org-link-completion-path-elisp
   :capf-desc 'org-link-completion-desc-elisp))

;; Path

;;;###autoload
(defun org-link-completion-path-elisp ()
  "Complete <expression> of [[elisp:<expression> at point."
  (org-link-completion-parse-let :path ()
    (elisp-completion-at-point)))

;; Description

(defcustom org-link-completion-desc-elisp-collectors
  '(org-link-completion-collect-description-from-other-links
    org-link-completion-collect-description-from-favorite-links
    org-link-completion-collect-path)
  "Functions that collect candidates of description in elisp link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

;;;###autoload
(defun org-link-completion-desc-elisp ()
  "Complete <desc> of [[elisp:<expression>][<desc> at point."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-call-collectors
      org-link-completion-desc-elisp-collectors)
     :kind 'text)))


;;;;; Info Type

;; Setup

;;;###autoload
(defun org-link-completion-setup-type-info ()
  (org-link-set-parameters
   "info"
   :capf-path 'org-link-completion-path-info
   :capf-desc 'org-link-completion-desc-info))

;; Path

;;;###autoload
(defun org-link-completion-path-info ()
  "Complete <info-file-node> of [[info:<info-file-node> at point."
  (org-link-completion-parse-let :path (path-beg path-end)
    (let* ((file-end (save-excursion
                       (goto-char path-beg)
                       (skip-chars-forward "^#" path-end)
                       (point)))
           (on-nodename (and (< file-end path-end) ;; Found #
                             (< file-end (point)))))

      (if on-nodename
          ;; [[info:<filename>#<nodename>
          (let ((filename (buffer-substring-no-properties path-beg file-end)))
            (org-link-completion-capf-result
             (1+ file-end) path-end
             (org-link-completion-collect-info-node-names filename)
             :kind 'text))
        ;; [[info:<filename>
        (org-link-completion-capf-result
         path-beg file-end ;; Include #
         (org-link-completion-table-with-alist-search
          #'org-link-completion-collect-info-file-title-alist)
         :kind 'file)))))

(autoload 'Info-speedbar-fetch-file-nodes "info")
(defun org-link-completion-collect-info-file-title-alist ()
  "Return an alist of info files and their titles."
  (cl-loop for (title . (filename . nodename))
           in (org-link-completion-collect-info-sub-nodes "dir" "Top")
           collect (cons (org-link-completion-annotate (concat filename "#")
                                                       title)
                         title)))

(defun org-link-completion-collect-info-sub-nodes (filename nodename)
  ;; Reference:
  ;; - `Info-speedbar-fetch-file-nodes'
  ;; - `Info-menu'
  (cl-loop for (title . node) in (Info-speedbar-fetch-file-nodes
                                  (format "(%s)%s" filename nodename))
           when (string-match "\\`(\\([^)]+\\))\\(.*\\)\\'" node)
           collect (cons (substring-no-properties title)
                         (cons (match-string-no-properties 1 node)
                               (match-string-no-properties 2 node)))))

(autoload 'Info-find-file "info")
(autoload 'info-insert-file-contents "info")
(autoload 'Info-following-node-name "info")
(defun org-link-completion-collect-info-node-names (file)
  ;; See: `Info-toc-build'
  (let (names
        (file-path (Info-find-file file t)))
    (when file-path
      (with-temp-buffer
        (info-insert-file-contents file-path)
        (goto-char (point-min))
        (while (and (search-forward "\n\^_\nFile:" nil 'move)
                    (search-forward "Node: " nil 'move))
          (push (substring-no-properties (Info-following-node-name)) names)))
      (nreverse names))))


;; Description

(defcustom org-link-completion-desc-info-collectors
  '(org-link-completion-collect-description-from-other-links
    org-link-completion-collect-description-from-favorite-links
    org-link-completion-collect-default-info-description
    org-link-completion-collect-info-node-name
    org-link-completion-collect-path)
  "Functions that collect candidates of description in info link."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

;;;###autoload
(defun org-link-completion-desc-info ()
  "Complete <desc> of [[info:<info-file-node>][<desc> at point."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-call-collectors
      org-link-completion-desc-info-collectors)
     :kind 'text)))

(defcustom org-link-completion-default-info-description "(%F)%N"
  "Default description format for info type links.

Used by the
`org-link-completion-collect-default-info-description' function."
  :group 'org-link-completion
  :type 'string)

(defun org-link-completion-default-info-description (file node)
  (replace-regexp-in-string "%[FN]"
                            (lambda (str)
                              (pcase str
                                ("%F" file)
                                ("%N" node)
                                (_ "")))
                            org-link-completion-default-info-description
                            t))

(defun org-link-completion-collect-default-info-description ()
  (org-link-completion-parse-let :desc (path)
    (when-let ((pos (seq-position path ?#)))
      (org-link-completion-string-list
       (org-link-completion-annotate
        (org-link-completion-default-info-description
         (substring path 0 pos)
         (substring path (1+ pos)))
        "Format")))))

(defun org-link-completion-collect-info-node-name ()
  (org-link-completion-parse-let :desc (path)
    (when-let ((pos (seq-position path ?#)))
      (org-link-completion-string-list
       (org-link-completion-annotate
        (substring path (1+ pos))
        "Node Name")))))


;;;; Complete From Other Links

;; Complete path and description from those used in other links.

;; Path

(defun org-link-completion-path-from-other-links ()
  "Complete the path at point from other links."
  (org-link-completion-parse-let :path (path-beg path-end)
    (org-link-completion-capf-result
     path-beg path-end
     (org-link-completion-collect-path-from-other-links)
     :kind 'text)))

(defun org-link-completion-collect-path-from-other-links ()
  (org-link-completion-parse-let nil (type-beg type-end)
    (when (< type-beg type-end)
      (save-excursion
        (goto-char (point-min))
        (let ((re (concat
                   "\\[\\["
                   (regexp-quote
                    (buffer-substring-no-properties type-beg type-end))
                   ":"
                   "\\(\\(?:[^][\\]\\|\\\\[][\\]\\|\\\\[^][\\]\\)+\\)\\][][]"))
              table)
          (while (re-search-forward re nil t)
            (unless (= (+ (match-beginning 0) 2) type-beg)
              (let ((path (match-string-no-properties 1)))
                (unless (member path table)
                  (push path table)))))
          table)))))


;; Description

(defun org-link-completion-desc-from-other-links ()
  "Complete the description at point from other links."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-collect-description-from-other-links)
     :kind 'text)))

(defun org-link-completion-collect-description-from-other-links (&optional
                                                                 link-beg
                                                                 link-end)
  (org-link-completion-parse-let :desc (type-beg path-end)
    (unless link-beg (setq link-beg type-beg))
    (unless link-end (setq link-end path-end))
    (when (< link-beg link-end)
      (save-excursion
        (goto-char (point-min))
        (let ((re (concat "\\[\\["
                          (regexp-quote
                           (buffer-substring-no-properties link-beg link-end))
                          "\\]\\["
                          "\\(.+\\)\\]\\]"))) ;; One or more chars
          (cl-loop while (re-search-forward re nil t)
                   unless (= (+ (match-beginning 0) 2) link-beg)
                   collect (org-link-completion-annotate
                            (match-string-no-properties 1)
                            "Others")))))))


;;;; Favorite Links

;; Favorite Links

(defcustom org-link-completion-favorite-links
  '(("https"
     ("//www.gnu.org/software/emacs/" "GNU Emacs")))
  "Path and description of the link you want to include as
completion candidates."
  :group 'org-link-completion
  :type '(alist :tag "Link Types"
                :key-type (string :tag "Type")
                :value-type (repeat
                             (list (string :tag "Path")
                                   (choice (const :tag "No Description" nil)
                                           (string :tag "Description"))))))

(defun org-link-completion-add-to-favorite-links ()
  "Add link at point to favorites."
  (interactive)
  (let ((element (org-element-context)))
    (unless (eq (org-element-type element) 'link)
      (error "No link at point"))
    (let* ((type (org-element-property :type element))
           (path (org-element-property :path element))
           (desc-beg (org-element-property :contents-begin element))
           (desc-end (org-element-property :contents-end element))
           (desc (and desc-beg desc-end
                      (buffer-substring-no-properties desc-beg desc-end)))
           (type (cond
                  ((member type '("fuzzy" "custom-id" "coderef")) "")
                  ((assoc type org-link-parameters) type)
                  (t (error "Unknown link type `%s'" type))))
           (favorite-links (copy-tree org-link-completion-favorite-links))
           (type-cell (assoc type favorite-links))
           (path-desc (list path desc)))
      (unless type-cell
        (push (setq type-cell (cons type nil))
              favorite-links))
      (when (member path-desc (cdr type-cell))
        (error "Already in favorite links"))
      (push path-desc (cdr type-cell))

      (setq org-link-completion-favorite-links favorite-links)
      (customize-save-variable 'org-link-completion-favorite-links
                               org-link-completion-favorite-links)
      (message "Added type:%s path:%s description:%s" type path desc))))

;; Path

(defun org-link-completion-path-from-favorite-links ()
  "Complete the path at point from favorite links."
  (org-link-completion-parse-let :path (path-beg path-end)
    (org-link-completion-capf-result
     path-beg path-end
     (org-link-completion-collect-path-from-favorite-links)
     :kind 'text)))

(defun org-link-completion-collect-path-from-favorite-links ()
  (org-link-completion-parse-let :path (type)
    (mapcar #'car (alist-get type org-link-completion-favorite-links
                             nil nil #'string=))))

;; Description

(defun org-link-completion-desc-from-favorite-links ()
  "Complete the description at point from favorite links."
  (org-link-completion-parse-let :desc (desc-beg desc-end)
    (org-link-completion-capf-result
     desc-beg desc-end
     (org-link-completion-collect-description-from-favorite-links)
     :kind 'text)))

(defun org-link-completion-collect-description-from-favorite-links ()
  (org-link-completion-parse-let :desc (type path)
    (cl-loop for (favorite-path favorite-desc)
             in (alist-get type org-link-completion-favorite-links
                           nil nil #'string=)
             when (equal path favorite-path)
             collect (org-link-completion-annotate favorite-desc "Favorites"))))


;;;; Utilities for completion

;;;;; Call Completion Function

(defcustom org-link-completion-disabled-completion-functions nil
  "List of completion functions to disable.

Affects calls with `org-link-completion-call'."
  :group 'org-link-completion-functions
  :type '(repeat (function)))

(defun org-link-completion-call (fun &rest args)
  "Call the completion function FUN with ARGS.

Do not use this function for functions that must be called.

Functions that are members of
`org-link-completion-disabled-completion-functions' are not called
and simply return nil."
  (when (and (functionp fun)
             (not (memq fun org-link-completion-disabled-completion-functions)))
    (apply fun args)))


;;;;; Completion Table

(defun org-link-completion-table-with-metadata (table metadata-alist)
  (when table
    (lambda (string predicate action)
      (cond
       ((eq action 'metadata) (cons 'metadata metadata-alist))
       (t (complete-with-action action table string predicate))))))

(defun org-link-completion-table-keep-order (table)
  (when table
    (org-link-completion-table-with-metadata
     table
     '((display-sort-function . identity)))))

(defun org-link-completion-table-with-alist-search (fun-collect-alist)
  ;; I used the idea from the following URL as a reference.
  ;; https://emacs.stackexchange.com/a/74550
  ;; completion - completing-read, search also in annotations -
  ;;  Emacs Stack Exchange
  (lambda (string predicate action)
    ;;(message "Complete alist action:%s string:%s" action string)
    (pcase action
      ('t (cl-loop with string-re = (regexp-quote string)
                   for cell in (funcall fun-collect-alist)
                   for key = (car cell)
                   for value = (cdr cell)
                   when (and
                         (or (null predicate)
                             (funcall predicate cell))
                         ;; TODO: respect completion style? Use all-completios?
                         (or (let ((case-fold-search t)) ;; Ignore case (?)
                               (string-match-p string-re key))
                             (let ((case-fold-search t)) ;; Ignore case (?)
                               (string-match-p string-re value))))
                   collect key)))))

;;;;; Return value of completion-at-point-functions

(defun org-link-completion-capf-result (beg end table &rest plist)
  (when table
    (nconc (list beg end
                 (org-link-completion-table-keep-order table))
           (org-link-completion-capf-result-convert-properties plist))))

(defun org-link-completion-capf-result-convert-properties (plist)
  (setq plist
        (cl-loop for (key value) on plist by #'cddr
                 nconc
                 (pcase key
                   ;; :kind <symbol>|<function>
                   (:kind
                    (list :company-kind
                          (if (functionp value)
                              value
                            (let ((kind value)) (lambda (_) kind)))))
                   (_
                    (list key value)))))
  ;; Add :annotate-function if not specified.
  (unless (plist-member plist :annotation-function)
    (setq plist (nconc (list :annotation-function
                             #'org-link-completion-annotation)
                       plist)))
  plist)

;;;;; Collectors

;; Collector returns only list of candidates

(defun org-link-completion-call-collectors (collectors)
  "Call functions in the list COLLECTORS and concatenate the
 returned lists with `nconc'."
  (mapcan (lambda (collector) (org-link-completion-call collector)) collectors))

(defun org-link-completion-collect-stripped-internal-link-path ()
  "Collect a string with internal link symbols removed from the
 path of the link at point."
  (org-link-completion-parse-let :desc (path)
    (org-link-completion-string-list
     (org-link-completion-annotate
      (org-link-completion-strip-internal-link path)
      "Path"))))

(defun org-link-completion-collect-path ()
  "Collect a path of the link at point."
  (org-link-completion-parse-let :desc (path)
    (org-link-completion-string-list
     (org-link-completion-annotate path "Path"))))

;;;;; Propertize

(defun org-link-completion-annotate (str annotation)
  (when (and str annotation)
    (propertize str :org-link-completion-annotation annotation)))

(defun org-link-completion-annotation (str)
  (get-text-property 0 :org-link-completion-annotation str))

;;;;; Retrieve From Org Document

(defun org-link-completion-get-heading ()
  "Return the heading of the current entry."
  (let ((heading (org-get-heading t t t t)))
    (when (and heading
               (not (string-empty-p heading)))
      (org-link-completion-annotate
       (substring-no-properties heading)
       "Heading"))))

(defun org-link-completion-link-search (path)
  "Move point to target of internal link PATH."
  (ignore-errors
    (if (eq org-link-search-must-match-exact-headline 'query-to-create)
        ;; Suppress questions to users.
        (let ((org-link-search-must-match-exact-headline t))
          (org-link-search path nil t))
      (org-link-search path nil t))
    t))

;;;;; Uncategorized

(defun org-link-completion-strip-internal-link (path)
  "Strip prefix and suffix (if any) at the beginning or end of internal links.

For example:
  \"#custom-id\" => \"custom-id\"
  \"*heading\" => \"heading\"
  \"(coderef)\" => \"coderef\""
  (let ((path-len (length path)))
    (or
     ;; #custom-id or *heading
     (and (>= path-len 1)
          (memq (elt path 0) '(?# ?*))
          (substring path 1))
     ;; (coderef)
     (and (>= path-len 2)
          (eq (elt path 0) ?\()
          (eq (elt path (1- path-len)) ?\))
          (substring path 1 -1))
     path)))


(defconst org-link-completion-escape-description-separator
  ;; export snippets hack or zero width space
  ;;(string ?\x200B)
  "@@-:@@")

(defun org-link-completion-escape-description-string (str)
  "Replace \"]]\" to \"]?]\"."
  ;; Ref: `org-link-make-string' (have a bug)
  (let ((last 0)
        curr
        (result ""))
    (while (setq curr (string-match "]\\(]\\|\\'\\)" str last))
      (setq result (concat result
                           (substring str last curr)
                           "]"
                           org-link-completion-escape-description-separator)
            last (1+ curr)))
    (setq result (concat result (substring str last)))
    result))

(defun org-link-completion-string-list (str)
  (when (and str (not (string-empty-p str)))
    (list str)))

(defun org-link-completion-call-with-file-find (file func)
  "Open FILE in a buffer and then call FUNC.
If a new buffer is created, it will not be killed. It is
inefficient to reopen the FILE every time it is completed."
  (when (file-regular-p file)
    (with-current-buffer (find-file-noselect file t)
      (save-mark-and-excursion
        (funcall func)))))

(defun org-link-completion-call-with-file (file mode func
                                                &optional
                                                use-find-file-noselect
                                                limit)
  "Open FILE in a buffer and then call FUNC.

If USER-FIND-FILE-NOSELECT is non-nil and a new buffer is
created, it will not be killed. It is inefficient to reopen the
FILE every time it is completed."
  (when (and (functionp func)
             (file-regular-p file))
    (let* ((file-buffer (or (find-buffer-visiting file)
                            (and
                             use-find-file-noselect
                             (find-file-noselect file))))
           (temp-buffer (unless file-buffer
                          (generate-new-buffer " *temp*" t))))
      (unwind-protect
          (with-current-buffer (or file-buffer temp-buffer)
            (when temp-buffer
              (insert-file-contents file nil nil limit)
              (when mode
                (funcall mode)))
            (save-mark-and-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (funcall func))))
        (when (buffer-live-p temp-buffer)
          (kill-buffer temp-buffer))))))

(defun org-link-completion-get-current-file ()
  (expand-file-name
   (buffer-file-name
    (or (buffer-base-buffer (current-buffer))
        (current-buffer)))))

(provide 'org-link-completion)
;;; org-link-completion.el ends here
