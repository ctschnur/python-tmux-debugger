;;; cs-python-tmux-debugger.el --- focuses a terminal window with a tmux session running in it and sends text into it (like compile or debug)  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  chris

;; Author: chris <chris@chris-lenovo>
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

;;

;;; Code:

;; at creation, create a cs-tmux-session with buffer-name, tmux-session-name and window-id
;; and push it to the list cs-connected-tmux-sessions

;; then, at call, get the connected-session by the current buffer-name and connect to it


;; or, just do everything buffer-local

;; (cl-defstruct cs-tmux-session tmux-session-name buffer-name window-id)

;; (defvar cs-connected-tmux-sessions nil
;;   "List of connected tmux sessions to send keys to.")

;; (defconst cs-cur-tmux-session-window-id nil
;;   "Window id for the debugging session.")


(require 'pyvenv)

(setq cs-cur-tmux-session-window-id nil)
(make-local-variable 'cs-cur-tmux-session-window-id)

(defun cs-tmux-get-tmux-session-name-for-buffer (;; &optional buffer
                                                           )
  ""
  (let* ((replacements (list (list "\\." "-dot-")
                             (list "<" "-langle-")
                             (list ">" "-rangle-")))
         (str (buffer-name)))
    (mapc (lambda (repl)
              (setq str (replace-regexp-in-string (car repl)
                                                  (cadr repl)
                                                  str)))
      replacements)
    (concat "cs-tmux--"
            ;; (file-name-base (buffer-name))
            str)))

(defun cs-tmux-session-exists-p (&optional session-name)
  ""
  (interactive)
  (unless session-name
    (setq session-name "Test01"))

  (let* ((return-str (shell-command-to-string "tmux list-sessions")))
    (string-match (regexp-quote (concat session-name)) return-str)))

(defun cs-tmux-test-open ()
  ""
  (interactive)
  (shell-command "gnome-terminal -e 'tmux attach -t 0' >/dev/null")
  ;; (shell-command (concat "gnome-terminal -e 'tmux attach -t "
  ;;                        "hey" "' >/dev/null"))
  (shell-command "gnome-terminal -e 'tmux attach -t hey' >/dev/null"))

(defun run-gnome-terminal-here ()
  (interactive "@")
  (shell-command (concat "gnome-terminal "
                         (file-name-directory (or load-file-name buffer-file-name))
                         ;; " -e 'tmux attach -t hey' "
                         " > /dev/null 2>&1 & disown")
                 nil
                 nil))

(defun cs-tmux-set-focus-to-window (window-id)
  ""
  (shell-command-to-string (concat "wmctrl -i -a " window-id))
  window-id)

(defun cs-tmux-open-new-terminal-and-attach (tmux-session-name)
  ""
  ;; log all terminal windows id's
  (let* (terminal-windows-ids-before
         terminal-windows-ids-after
         new-terminal-window-id
         term-window-id)
    (setq terminal-windows-ids-before
          (remove ""
                  (split-string (shell-command-to-string "xdotool search --onlyvisible -class gnome-terminal")
                                "\n")))

    (shell-command (concat "gnome-terminal -e 'tmux attach -t "
                           tmux-session-name "' >/dev/null"))

    (setq terminal-windows-ids-after
          (remove ""
                  (split-string (shell-command-to-string "xdotool search --onlyvisible -class gnome-terminal")
                                "\n")))

    (setq term-window-id
          (let* ((tmp (-difference terminal-windows-ids-after terminal-windows-ids-before)))
            ;; check that a exactly one window was created
            ;; (assert (equal (length tmp) 1))
            (unless (equal (length tmp) 1)
              (warn (concat "term-window-id will be set to " (prin1-to-string (car tmp)))))
            (car tmp)))

    (cs-tmux-set-focus-to-window term-window-id)))

(defun cs-tmux-terminal-window-exists-p (window-id)
  ""
  (let* ((terminal-windows-ids (remove ""
                                       (split-string (shell-command-to-string "xdotool search --onlyvisible -class gnome-terminal")
                                                     "\n"))))
    (member window-id terminal-windows-ids)))


(defun cs-tmux-send-keys (tmux-session-name keys-to-send)
  (shell-command-to-string (concat "tmux send-keys -t " tmux-session-name
                                   " " keys-to-send ;; " ENTER"
                                   )))

(defun cs-tmux-create-new-session (tmux-session-name-target)
  (shell-command-to-string (concat "tmux new -s " tmux-session-name-target
                                   " -d")))

(defun python-call (&optional keys-to-send)
  "From within a python buffer, make a call to run that file in a tmux session."
  (interactive)

  (unless (equal (file-name-extension (buffer-file-name)) "py")
    (user-error "Not a python file"))

  (unless keys-to-send
    (setq keys-to-send (concat (prin1-to-string (concat "python3 " (prin1-to-string (buffer-file-name))))
                               " ENTER")))

  (let* ((tmux-session-for-cur-buffer (cs-tmux-get-tmux-session-name-for-buffer)))
    (if (cs-tmux-session-exists-p tmux-session-for-cur-buffer)
        ;; send to existing tmux session
        (cs-tmux-send-keys tmux-session-for-cur-buffer
                           keys-to-send)
      ;; create it
      (cs-tmux-create-new-session tmux-session-for-cur-buffer)
      (if (cs-tmux-session-exists-p tmux-session-for-cur-buffer)
          (let* ((virtualenv-path (when (and (boundp 'pyvenv-virtual-env)
                                             pyvenv-virtual-env)
                                    pyvenv-virtual-env))
                 virtualenv-activate-script-path)
            (when virtualenv-path
              (setq virtualenv-activate-script-path
                    (concat (file-name-directory pyvenv-virtual-env)
                            "./bin/activate"))
              ;; activate possible virtual environment
              ;; check if it is a conventional virtual environment
              ;; with a bin/actiate script
              (if (file-exists-p virtualenv-activate-script-path)
                  (cs-tmux-send-keys tmux-session-for-cur-buffer
                                     (concat (prin1-to-string (concat "source "
                                                                      (prin1-to-string virtualenv-activate-script-path)))
                                             " ENTER"))
                ;; else try conda
                (cs-tmux-send-keys tmux-session-for-cur-buffer
                                   (concat (prin1-to-string (concat "conda activate "
                                                                    pyvenv-virtual-env-name))
                                           " ENTER"))))
            (cs-tmux-send-keys tmux-session-for-cur-buffer
                               keys-to-send))
        (user-error (concat tmux-session-for-cur-buffer " was not created"))))

    ;; check if the window associated to this session has already been opened from inside
    ;; this program, i.e. if there is a window id
    (unless (boundp 'cs-cur-tmux-session-window-id)
      (make-local-variable 'cs-cur-tmux-session-window-id)
      (setq cs-cur-tmux-session-window-id nil))
    (if (and cs-cur-tmux-session-window-id
             (cs-tmux-terminal-window-exists-p cs-cur-tmux-session-window-id))
        ;; only set focus to it
        (cs-tmux-set-focus-to-window cs-cur-tmux-session-window-id)
      ;; connect to it and open it in external gnome-terminal
      ;; then, set the window id and the name
      (setq cs-cur-tmux-session-window-id (cs-tmux-open-new-terminal-and-attach tmux-session-for-cur-buffer)))))

(defun focus-python-window ()
  ""
  (interactive)
  (python-call ""))

(defun cs-tmux-python-call-send-region ()
  "This only makes sense once ipdb is already active in tmux.
I don't know exactly how I would check that though."
  (interactive)
  (let* ((region-text (if (region-active-p)
                          (buffer-substring-no-properties (mark)
                                                          (point))
                        "")))
    (python-call (concat (prin1-to-string region-text)
                         " ENTER"))))

(defun python-call-general ()
  ""
  (interactive)
  (if (region-active-p)
      (cs-tmux-python-call-send-region)
    (python-call)))

(define-key python-mode-map (kbd "C-, e") 'python-call-general)
(define-key python-mode-map (kbd "C-, o") 'focus-python-window)


(provide 'cs-python-tmux-debugger)
;;; cs-python-tmux-debugger.el ends here
