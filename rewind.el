;;; rewind.el --- toggle window configuration save and restore -*- lexical-binding: t; -*-

;; Copyright © 2026, by ed9w2in6

;; Author: ed9w2in6
;; Maintainer: ed9w2in6
;; Version: 0.1
;; Package-Requires: ((emacs "30.2"))
;; Created: [2026-04-08 Wed]
;; Keywords: convenience
;; Homepage: https://github.com/ed9w2in6/rewind

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 3.

;;; Commentary:

;; Default behaviour:
;; toggle to save and restore window configuation and point, with state
;; tracked per frame and tab in `tab-bar-mode'

;; Window configuration is the layout of your windows in a tab or frame.
;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/Window-Configurations.html

;; Customise behaviour using "rewind-after-*" hooks and
;; "rewind-before-*" functions.  Some hints:
;; + "after-*-hooks" can be used to add IO behaviours
;;   - e.g. show an indicator at mode-line
;; + "before-*-functions" can be used to add conditions before save and restore
;;   - e.g. only save when more than 1 window (splitted layout)

;; By adjusting these, you can create alternative behaviours.  For example:
;; + Add    `delete-other-windows'    to   `rewind-after-save-hook' and
;;          `rewind-not-one-window-p' to   `rewind-before-save-functions'
;; + Remove `rewind--restore-point'   from `rewind-during-restore-hook'
;; This makes toggle behaves like tmux zoom pane: maximise current window, NOT
;; saving when already maximised (single window), and NOT restoring point.

;; TODO:
;; - tests

;;; Code:

(defgroup rewind nil
  "Toggle window configuration save and restore.

Window configuration is the layout of your windows in a tab or frame."
  :group 'convenience)

;;; Branch predicates

(defcustom rewind-before-save-functions nil
  "Abnormal hook run before saving the configuration.

Each function should take a single parameter for `current-prefix-arg'.

Each function should return non-nil otherwise the save branch is aborted.
Note that when aborting, `rewind-after-save-hook' will be skipped.

See `rewind-toggle'."
  :type 'hook
  :group 'rewind)

(defcustom rewind-before-restore-functions nil
  "Abnormal hook run before restoring the configuration.

Each function should take a single parameter for `current-prefix-arg'.

Each function should return non-nil otherwise the restore branch is aborted.
Note that when aborting, both `rewind-during-restore-hook'
and `rewind-after-restore-hook' will be skipped.

See `rewind-toggle'."
  :type 'hook
  :group 'rewind)

;;; After action hooks

(defvaralias 'rewind-during-save-hook 'rewind-after-save-hook
  "Same as `rewind-after-save-hook'."
  )
(defcustom rewind-after-save-hook nil
  "Hook run after saving `current-window-configuration' and point.

See `rewind--saved-configs' and `rewind--saved-point-markers'."
  :type 'hook
  :group 'rewind)
 
(defcustom rewind-during-restore-hook '(rewind--restore-point)
  "Hook run immediately after restoring window configuration, before clean-up.

Ran before clean-up: resetting the managed saved states to nil.
See `rewind-after-restore-hook' for after clean-up.

See `rewind--saved-configs' and `rewind--saved-point-markers'.

The function `rewind--restore-point' is added to this hook by default."
  :type 'hook
  :group 'rewind)

(defcustom rewind-after-restore-hook nil
  "Hook run after restoring window configuration, after clean-up.

Ran after clean-up: resetting the managed saved states to nil.
See `rewind-during-restore-hook' for after clean-up.

See `rewind--saved-configs' and `rewind--saved-point-markers'."
  :type 'hook
  :group 'rewind)

;;; Internal states
(defvar rewind--saved-configs (make-hash-table :test 'equal :weakness 'key)
  "Currently saved window configurations.
No guarantee to what object type this is.
Do NOT modify this or rely on this or risk undefined behaviour.
Managed by `rewind-toggle'.")

(defvar rewind--saved-point-markers (make-hash-table :test 'equal :weakness 'key)
  "Currently saved points as marker objects.
No guarantee to what object type this is.
Do NOT modify this or rely on this or risk undefined behaviour.
Managed by `rewind-toggle'.")

;;; Functions
(defun rewind--get-saved-config ()
  "Get the corresponding saved window configuration."
  (let ((inner-hash-table (gethash (selected-frame) rewind--saved-configs)))
    (when (hash-table-p inner-hash-table)
        (gethash (tab-bar--current-tab-index) inner-hash-table))))

(defun rewind--set-saved-config (window-configuration)
  "Set the corresponding window configuration to WINDOW-CONFIGURATION."
  (let ((inner-hash-table (gethash (selected-frame) rewind--saved-configs)))
    (unless (hash-table-p inner-hash-table)
      (setq inner-hash-table (make-hash-table :test 'eq))
      (puthash (selected-frame) inner-hash-table rewind--saved-configs))
    (puthash (tab-bar--current-tab-index) window-configuration inner-hash-table)))

(defun rewind--get-saved-point-marker ()
  "Get the corresponding saved point marker."
  (let ((inner-hash-table (gethash (selected-frame) rewind--saved-point-markers)))
    (when (hash-table-p inner-hash-table)
        (gethash (tab-bar--current-tab-index) inner-hash-table))))

(defun rewind--set-saved-point-marker (marker)
  "Set the corresponding marker to MARKER."
  (let ((inner-hash-table (gethash (selected-frame) rewind--saved-point-markers)))
    (unless (hash-table-p inner-hash-table)
      (setq inner-hash-table (make-hash-table :test 'eq))
      (puthash (selected-frame) inner-hash-table rewind--saved-point-markers))
    (puthash (tab-bar--current-tab-index) marker inner-hash-table)))

(defun rewind--restore-point ()
  "Restore point using `rewind--saved-point-markers'.

Restore cursor position of the saved state.

Do NOT call this manually as there might be no point marker saved.
It is valid when calling via `rewind-during-restore-hook'."
  (goto-char (rewind--get-saved-point-marker)))

(defun rewind-not-one-window-p (_has-prefix-p)
  "Negation of `one-window-p'.

Can be used to make toggle behave like tmux zoom pane."
  (not (one-window-p)))

(defun rewind-is-saved-p ()
  "Return non-nil if states managed by rewind is saved.

That is, retuning non-nil when the corresponding values for
 `rewind--saved-configs' and `rewind--saved-point-markers'
contains valid objects."
  (and (window-configuration-p (rewind--get-saved-config))
       (markerp (rewind--get-saved-point-marker))))

;;;###autoload
(defun rewind-toggle ()
  "Toggle window configuration save and restore.

Window configuration is the layout of your windows in a tab or frame.

Attempts to restore if `rewind-is-saved-p' is non-nil, else tries to save.

Before saving or restoring, will run hook `rewind-before-save-functions' and
`rewind-before-restore-functions' respectively until failure.
If any of the predicates in the hooks returns non-nil, will abort any action.

See `run-hook-with-args-until-failure'."
  (interactive)
  (if (rewind-is-saved-p)
      ;; restore branch
      (when (run-hook-with-args-until-failure 'rewind-before-restore-functions current-prefix-arg)
        (set-window-configuration (rewind--get-saved-config))
        (run-hooks 'rewind-during-restore-hook)
        (rewind--set-saved-config nil)
        (rewind--set-saved-point-marker nil)
        (run-hooks 'rewind-after-restore-hook))
    ;; save branch
    (when (run-hook-with-args-until-failure 'rewind-before-save-functions current-prefix-arg)
      (rewind--set-saved-config (current-window-configuration))
      (rewind--set-saved-point-marker (point-marker))
      (run-hooks 'rewind-after-save-hook))))

(provide 'rewind)
;;; rewind.el ends here
