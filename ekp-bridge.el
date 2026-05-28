;;; ekp-bridge.el --- WebSocket bridge to Deno/TypeScript  -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026

;; Author: iris
;; URL: https://github.com/iris/ekp
;; Version: 0.1
;; Package-Requires: ((websocket "1.15"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This is a fork of deno-bridge with fixed byte-compile warnings:
;; - Changed unused lambda arguments from _websocket to _ (Emacs convention)

;;; Commentary:

;; Provides a WebSocket-based communication layer between Emacs Lisp
;; and Deno/TypeScript subprocesses.  Starts a Deno process and
;; manages bidirectional messaging via local WebSocket connections.
;; Used by ekp.el for accelerated Knuth-Plass line breaking.

;;; Code:
(require 'cl-lib)
(require 'websocket nil t)
(require 'ansi-color nil t)

(defvar ekp-bridge-app-list (list))

(defun ekp-bridge-get-free-port ()
  (save-excursion
    (let* ((process-buffer " *temp*")
           (process (make-network-process
                     :name process-buffer
                     :buffer process-buffer
                     :family 'ipv4
                     :server t
                     :host "127.0.0.1"
                     :service t))
           port)
      (setq port (process-contact process))
      (delete-process process)
      (kill-buffer process-buffer)
      (format "%s" (cadr port)))))

(cl-defmacro ekp-bridge-start (app-name ts-path)
  (let ((server (intern (format "ekp-bridge-server-%s" app-name)))
        (process (intern (format "ekp-bridge-process-%s" app-name)))
        (process-buffer (format " *ekp-bridge-app-%s*" app-name))
        (client (intern (format "ekp-bridge-client-%s" app-name))))
    `(if (member ,app-name ekp-bridge-app-list)
         (message "[EkpBridge] Application %s already started." ,app-name)
       (let* ((deno-port (ekp-bridge-get-free-port))
              (process-environment (cons "NO_COLOR=true" process-environment)))
         (defvar ,server nil)
         (defvar ,process nil)
         (defvar ,client nil)

         (setq ,server
               (websocket-server
                t
                :host 'local
                :on-message
                (lambda (&ignore frame)
                  (let ((text (websocket-frame-text frame))
                        (opcode (websocket-frame-opcode frame)))
                    (when (eq opcode 'text)
                      (condition-case nil
                          (let* ((info (json-parse-string text))
                                 (info-type (gethash "type" info nil)))
                            (pcase info-type
                              ("show-message" (message (gethash "content" info nil)))
                              ("eval-code" (eval (read (gethash "content" info nil))))
                              ("fetch-var"
                               (websocket-send-text
                                frame
                                (json-encode (eval (read (gethash "content" info nil))))))))
                        (json-parse-error nil)))))
                :on-open
                (lambda (&ignore)
                  (setq ,client (websocket-open (format "ws://127.0.0.1:%s" deno-port))))
                :on-close
                (lambda (&ignore)
                  (when (and (boundp ',client) (symbol-value ',client))
                    (websocket-close (symbol-value ',client))
                    (makunbound ',client))
                  (when (and (boundp ',process)
                             (process-live-p (symbol-value ',process)))
                    (delete-process (symbol-value ',process))
                    (makunbound ',process))
                  (when (and (boundp ',server) (symbol-value ',server))
                    (makunbound ',server))
                  (let ((buf (get-buffer ,process-buffer)))
                    (when buf (kill-buffer buf)))
                  (setq ekp-bridge-app-list
                        (delete ,app-name ekp-bridge-app-list)))
                :on-error
                (lambda (&ignore err)
                  (message "[EkpBridge] %s error: %s" ,app-name
                           (error-message-string err))))

         (let ((emacs-port (process-contact
                            (websocket-server-process ,server) :service)))
           (setq ,process
                 (start-process ,app-name ,process-buffer
                                "deno" "run" "--allow-all"
                                ,ts-path ,app-name
                                deno-port (format "%s" emacs-port))))

         (set-process-sentinel
          ,process
          (lambda (p &ignore)
            (when (eq 0 (process-exit-status p))
              (with-current-buffer (process-buffer p)
                (ansi-color-apply-on-region (point-min) (point-max))))))

          (add-to-list 'ekp-bridge-app-list ,app-name t))))))

(defun ekp-bridge-exit ()
  (interactive)
  (let* ((app-name (completing-read "[EkpBridge] Exit application: " ekp-bridge-app-list)))
    (if (member app-name ekp-bridge-app-list)
        (let* ((server (intern-soft (format "ekp-bridge-server-%s" app-name)))
               (process (intern-soft (format "ekp-bridge-process-%s" app-name)))
               (process-buffer (format " *ekp-bridge-app-%s*" app-name))
               (client (intern-soft (format "ekp-bridge-client-%s" app-name))))
          (when server
            (when (symbol-value server)
              (websocket-server-close (symbol-value server)))
            (makunbound server))

          (when client
            (when (symbol-value client)
              (websocket-close (symbol-value client)))
            (makunbound client))

          (let ((old-kill-buffer-query-functions kill-buffer-query-functions))
            (when process
              (let ((kill-buffer-query-functions nil))
                (kill-buffer process-buffer)
                (makunbound process))
              (setq kill-buffer-query-functions old-kill-buffer-query-functions)))

          (setq ekp-bridge-app-list (delete app-name ekp-bridge-app-list)))
      (message "[EkpBridge] Application %s not found." app-name))))

(defun ekp-bridge-call (app-name &rest func-args)
  "Call Deno TypeScript function from Emacs."
  (if (member app-name ekp-bridge-app-list)
      (websocket-send-text (symbol-value (intern-soft (format "ekp-bridge-client-%s" app-name)))
                           (json-encode (list "data" func-args)))
    (message "[EkpBridge] Application %s not started." app-name)))

(provide 'ekp-bridge)

;;; ekp-bridge.el ends here
