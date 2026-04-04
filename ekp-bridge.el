;;; ekp-bridge.el --- Bridge between Emacs and Deno -*- lexical-binding: t; -*-

;; This is a fork of deno-bridge with fixed warnings and improved websocket handling.

;;; Code:
(require 'cl-lib)
(require 'websocket nil t)
(require 'ansi-color nil t)

(declare-function websocket-server "websocket")
(declare-function websocket-server-close "websocket")
(declare-function websocket-open "websocket")
(declare-function websocket-close "websocket")
(declare-function websocket-send-text "websocket")
(declare-function json-encode "json")

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
  (if (member app-name ekp-bridge-app-list)
      (message "[EkpBridge] Application %s has started." app-name)
    (let* ((deno-port (ekp-bridge-get-free-port))
           (emacs-port (ekp-bridge-get-free-port))
           (server-sym (intern (format "ekp-bridge-server-%s" app-name)))
           (process-sym (intern (format "ekp-bridge-process-%s" app-name)))
           (process-buffer (format " *ekp-bridge-app-%s*" app-name))
           (client-sym (intern (format "ekp-bridge-client-%s" app-name))))
      `(let ((process-environment (cons "NO_COLOR=true" process-environment)))
         (defvar ,server-sym nil)
         (defvar ,process-sym nil)
         (defvar ,client-sym nil)

         (setq ,server-sym
               (websocket-server
                ,emacs-port
                :host 'local
                :on-message (lambda (_websocket frame)
                              (let ((text (websocket-frame-text frame))
                                    (opcode (websocket-frame-opcode frame)))
                                (when (eq opcode 'text)
                                  (condition-case err
                                      (let* ((info (json-parse-string text))
                                             (info-type (gethash "type" info nil)))
                                        (pcase info-type
                                          ("show-message" (message "%s" (gethash "content" info nil)))
                                          ("eval-code" (eval (read (gethash "content" info nil))))
                                          ("fetch-var"
                                           (let* ((content (gethash "content" info nil))
                                                  (var-name (when content (format "%s" content))))
                                             (when var-name
                                               (condition-case ev-err
                                                   (let ((value (eval (read var-name) t)))
                                                     (websocket-send-text _websocket (json-encode (format "%s" value))))
                                                 (void-variable
                                                  (websocket-send-text _websocket (json-encode "nil")))
                                                 (error
                                                  (websocket-send-text _websocket (json-encode "nil"))))))))
                                    (json-parse-error
                                     (message "[EkpBridge] JSON parse error: %S" err))))))
                :on-open (lambda (_websocket)
                           (setq ,client-sym (websocket-open (format "ws://127.0.0.1:%s" ,deno-port))))
                :on-close (lambda (_websocket))))
         (setq ,process-sym
               (start-process ,app-name ,process-buffer "deno" "run" "--allow-all" ,ts-path ,app-name ,deno-port ,emacs-port))
         (set-process-sentinel
          ,process-sym
          (lambda (p _m)
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
            (when (and (boundp server) (symbol-value server))
              (websocket-server-close (symbol-value server)))
            (makunbound server))

          (when client
            (when (and (boundp client) (symbol-value client))
              (websocket-close (symbol-value client)))
            (makunbound client))

          (let ((old-kill-buffer-query-functions kill-buffer-query-functions))
            (when process
              (let ((kill-buffer-query-functions nil))
                (kill-buffer process-buffer)
                (makunbound process))
              (setq kill-buffer-query-functions old-kill-buffer-query-functions)))

          (setq ekp-bridge-app-list (delete app-name ekp-bridge-app-list)))
      (message "[EkpBridge] Application %s has exited." app-name))))

(defun ekp-bridge-call (app-name &rest func-args)
  "Call Deno TypeScript function from Emacs."
  (if (member app-name ekp-bridge-app-list)
      (let ((client-sym (intern-soft (format "ekp-bridge-client-%s" app-name))))
        (when (and (boundp client-sym) (symbol-value client-sym))
          (websocket-send-text (symbol-value client-sym)
                               (json-encode (list "data" func-args)))))
    (message "[EkpBridge] Application %s has exited." app-name)))

(provide 'ekp-bridge)

;;; ekp-bridge.el ends here
