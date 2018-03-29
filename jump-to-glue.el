(require 'json)
(require 's)

(defvar glue-roots
  ""
  '())

(defun trowel-request (json-plist)
  (let ((url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")))
        (url-request-data (json-encode json-plist)))
    (with-current-buffer (url-retrieve-synchronously "http://127.0.0.1:5555")
      (unless (null url-http-end-of-headers)
        (let ((json-object-type 'plist))
          (json-read-from-string (buffer-substring (+ url-http-end-of-headers 1) (point-max))))))))

(defun select-appropriate-glue-root (file-path)
  (cdr
   (seq-find (lambda (pair)
               ;; Case sensitivity :(
               (s-prefix? (car pair) file-path))
             glue-roots)))

(defun request-step-location (file-path step-text)
  (let* ((root (select-appropriate-glue-root file-path))
         (response (trowel-request `(:rootDir ,root :action "lookup" :stepText ,step-text))))
    (plist-get response :matches)))

(defun do-jump-to-step-definition (file-path step-text)
  (let ((r (request-step-location file-path step-text)))
    (cond ((null r) (message "Couldn't contact Trowel :(") nil)
          ((= 0 (length r)) (message "Step not found: %s" step-text) nil)
          ((> 0 (length r)) (message "More than one match found.") nil)
          (t (let* ((m (elt r 0))
                    (file (plist-get m :file))
                    (lineNumber (plist-get m :lineNumber)))
               (start-process "eclipse" nil "eclipse" (format "%s+%s" file lineNumber)))))))

(defun jump-to-step-definition-current-line ()
  (interactive)
  (let* ((whole-line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
         (trim-line (s-trim whole-line))
         (first-space (seq-position trim-line ?\s))
         (step-text (substring trim-line (+ 1 first-space))))
    (do-jump-to-step-definition (buffer-file-name) step-text)))
