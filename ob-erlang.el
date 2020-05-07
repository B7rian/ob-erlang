;;; ob-erlang.el --- org-babel functions for erlang evaluation
;; Copyright (C) 2020 Brian Hughes (brian@kuayo.xyz)
;; URL: http://github.com/b7rian/ob-elixir
;; Keywords: org babel elixir
;; Version: 0.0.1
;; Created: 4th May 2020
;; Package-Requires: ((org "8"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Commentary:
;; org-babel functions for erlang evaluation

;; ----------------------- Old copyright below -----------------------------
;; Copyright (C) 2015 ZHOU Feng

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-elixir
;; Package-Version: 20170725.1419
;; Keywords: org babel elixir
;; Version: 0.0.1
;; Created: 28th Sep 2015

;;; Code:
(require 'ob)

(defvar ob-erlang-process-output "")

(defconst org-babel-header-args:erlang
  '((setcookie . :any)
    (name . :any)
    (remsh . :any)
    (module . :any)
    (start . :any)
    (sname . :any))
  "erlang header arguments")

(defvar ob-erlang-eoe "\u2029")

(add-to-list 'org-babel-tangle-lang-exts '("erlang" . "erl"))

(defun org-babel-execute:erlang (body params)
  ;; Issues: where to run from, and how to get the right modules imported?
  ;; 1. If we cd to /tmp/something and work there we lose the ability to access
  ;;    files in cwd (can't change module search path from within erl session)
  ;; 2. We could start the session with erl -pa . to put cwd in the path, but
  ;;    different blocks to be evaled may be in different folders so we don't
  ;;    really know where cwd is anyway.
  ;; 3. Could dump buffer in cwd but we might overwrite something, especially
  ;;    if we use the user-given module name
  ;; 4. User might specifiy :dir and we have to look like we're using it.
  ;;
  ;; Solution: Dump user code in tmp file, rename module to match random filename
  ;; given by org-babel, compile it (.beam will land in cwd) and run in user-given
  ;; :dir or cwd it's random name
  (let* ((session (cdr (assoc :session params)))
         (dir (assoc-default :dir params))
         (tmp-file (org-babel-temp-file "erlang" ".erl"))
         (module (file-name-base tmp-file)))
    (ob-erlang-ensure-session session params)
    (with-temp-file tmp-file (insert (ob-erlang-force-module-name body module)))
    (if dir
      (string-join (list
        (ob-erlang-eval session "pwd().")
        (ob-erlang-eval-in-dir session dir (format "c(\"%s\")." tmp-file))
        (ob-erlang-eval-in-dir session dir
          (format "%s:%s()." module (or (cdr (assoc :start params)) "start")))))
      (string-join (list
        (ob-erlang-eval session "pwd().")
        (ob-erlang-eval session (format "c(\"%s\")." tmp-file))
        (ob-erlang-eval session
          (format "%s:%s()." module (or (cdr (assoc :start params)) "start"))))))))

(defun ob-erlang-eval-in-dir (session dir body)
  (string-join (list
    (ob-erlang-eval session (format "cd(\"%s\")." dir))
    (ob-erlang-eval session body))))

(defun ob-erlang-force-module-name (body name)
  (string-join
   (list (format "-module(%s)." name)
         (replace-regexp-in-string "-module([^)]+)\." "" body))))

(defun ob-erlang-eval (session body)
  (let ((result (ob-erlang-eval-in-repl session body)))
    (replace-regexp-in-string (format "%s[\r\n]*" body) ""
      (replace-regexp-in-string "^.+>.*[\r\n]*" ""
        (replace-regexp-in-string "\r" "" result)))))

(defun ob-erlang-ensure-session (session params)
  (let ((name (format "*erlang-%s*" session)))
    (unless (and (get-process name)
                 (process-live-p (get-process name)))
      (with-current-buffer (get-buffer-create name)
        (make-local-variable 'process-environment)
        ;; things hang without line below
        (setq process-environment (cons "TERM=vt100" process-environment))
        (apply 'start-process name name "erl"
               (append (when (assoc :sname params)
                         (list "-sname" (assoc-default :sname params)))
                       (when (assoc :name params)
                         (list "-name" (assoc-default :name params)))
                       (when (assoc :setcookie params)
                         (list "-setcookie" (assoc-default :cookie params)))
                       (when (assoc :remsh params)
                         (list "-remsh" (assoc-default :remsh params)))
                       (when (assoc :pa params)
                         (list "-pa" (assoc-default :pa params))))))
      (sit-for 0.5)
      (set-process-filter (get-process name) 'ob-erlang-process-filter)
      (sit-for 0.2))))

(defun ob-erlang-process-filter (process output)
  (setq ob-erlang-process-output (concat ob-erlang-process-output output)))

(defun ob-erlang-eval-in-repl (session body)
  (let ((name (format "*erlang-%s*" session)))
    (setq ob-erlang-process-output "")
    (process-send-string name (format "%s\n" body))
    (process-send-string name (format "%% %s\n" ob-erlang-eoe))
    (while (not (string-match-p ob-erlang-eoe ob-erlang-process-output))
      (accept-process-output (get-process name) nil 200 1))
    (replace-regexp-in-string (format "^.*%% %s.*[\r\n]*" ob-erlang-eoe) ""
                              ob-erlang-process-output)))

(provide 'ob-erlang)
;;; ob-erlang.el ends here
