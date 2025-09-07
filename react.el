;;; react.el --- react to light -*- lexical-binding: t -*-

;; Author: kpm <kpm@krzysckh.org>
;; Created: 07 Sep 2025
;; Keywords: network
;; URL: https://github.com/krzysckh/lightp
;;
;; Copyright (C) 2025 kpm <kpm@krzysckh.org>
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;     * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;; This file is not part of GNU Emacs.

;;; Code:

(defconst react/ip 7762)
(defconst react/buffer-name " *React buffer*")
(defconst react/process-name " *React process*")

(defvar react/last-theme-type nil)
(defvar react/last-ip nil)

(defun react/maybe-load (type)
  (when (not (eq react/last-theme-type type))
    (setf react/last-theme-type type)
    (rc/load-random-theme type)))

(defun react/connect (ip)
  (message "[react] connecting to %s" ip)
  (setf react/last-ip ip)
  (open-network-stream
   react/process-name
   react/buffer-name
   ip
   react/ip
   :type 'plain
   ; :filter #'(lambda (&rest _) t)
  (with-current-buffer (get-buffer-create react/buffer-name)
    (add-hook 'after-change-functions 'react/buffer-change-hook nil t))))

(defun react/buffer-change-hook (start end length)
  (with-current-buffer react/buffer-name
    (let* ((data (buffer-substring-no-properties start end)))
       (case (string-to-char data)
         (?a (react/maybe-load 'dark))
         (?b (react/maybe-load 'light))
         (?R (message "[react] asked to reconnect")
             (when (process-live-p react/process-name)
               (kill-process react/process-name))
             (react/connect react/last-ip))
         (else
          (message "[react] unknown command: " data))))))

(provide 'react)
