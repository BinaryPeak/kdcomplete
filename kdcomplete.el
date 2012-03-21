(require 'json)

(setq kdc-loading-results nil)
(setq kdc-result-string "")
(setq kdc-result-completions nil)
(setq kdc-result-help nil)
(setq kdc-completion-point nil)
(setq kdc-temp-file "/tmp/completion.txt")
(setq kdc-server-proc nil)

(if load-file-name
    (setq kdc-dir (file-name-directory load-file-name))
    )

(defun kdc-current-line ()
  "Return the current line number (in the buffer) of point."
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))

(defun kdc-create-completion-request  ()
  "Create the string for a completion request. Basically just dumps the current"
  "buffer along with information where the cursor is located. Future improvement:"
  "dump partial buffer, only up to completion point."
  (interactive)
  (save-excursion
    (goto-char ac-point)
    (let ((msg nil))
      (setq msg 
	    (concat 
	     buffer-file-name "\n"
	     (number-to-string (kdc-current-line)) "\n"
	     (number-to-string (+ 1 (current-column))) "\n"
	     (buffer-substring-no-properties (point-min) (point-max))
	     "\n"
	     )
	    )      
      msg
      )))

(defun kdc-create-help (items)
  "Creates the help alist from a list of completions with return types and overloads."
  (setq kdc-result-help
  	(mapcar
  	 (lambda (completion) 
  	   ;; For each completion, create alist pair	   
  	   (cons
	    (cdr (assoc 'word completion))
	    (concat
	     ;; Create a multiline string with <return_value> <word>(<param>..) syntax
	     (mapconcat
	      (lambda (overload)
	    	 (concat (cdr (assoc 'return_value overload)) " "
	    		 (cdr (assoc 'word completion))
	    		 "("
	    		 (mapconcat 'identity (cdr (assoc 'arguments overload)) ", ")
	    		 ")")
	    	 )
	      (cdr (assoc 'overloads completion))
	      "\n"
	      ))))
  	 items
  	 )))

(defun kdc-extract-completions (str)
  "Given a JSON reply from the server, create help alist and completion list"
  (setq kdc-result-completions (json-read-from-string str))

  (kdc-create-help kdc-result-completions)

  ;; Strip everything but the completion word for the completion list
  (setq kdc-result-completions 
	(mapcar (lambda (arg) (cdr (assoc 'word arg))) kdc-result-completions))

  kdc-result-completions
  )

(defun kdc-write-completion-file (req)
  "Writes the completion file to a temporary location"
  (with-temp-file kdc-temp-file
    (insert-string req)
    ))

(defun kdc-completion-stash-filter (proc string)
  "Append partial JSON string from the asynchronous client process"
  (setq kdc-result-string (concat kdc-result-string string)))

(defun kdc-completion-sentinel (proc event)
  "Called when asynchronous client process finished. Triggers completion list."
  (setq kdc-loading-results nil)
  (setq kdc-result-completions (kdc-extract-completions kdc-result-string))
  (ac-complete-kdc-static)
  )

(defun kdc-init-completion-request()
  "Sets up the completion state so that the completion client can be invoked."
  (setq kdc-loading-results t)
  (setq kdc-completion-point ac-point)
  (setq kdc-result-string "")
  (kdc-write-completion-file (kdc-create-completion-request))
  )

(defun kdc-trigger-client-process ()
  "Starts the client process for a completion."
  (let (
	(cc-proc 
	 (start-process "completion-client" "*Messages*" 
			(concat kdc-dir "client.py") kdc-temp-file
			)))
    
    (set-process-filter cc-proc 'kdc-completion-stash-filter)
    (set-process-sentinel cc-proc 'kdc-completion-sentinel)
    (set-process-query-on-exit-flag cc-proc nil)
    )
  )

(defun kdc-server-sentinel (proc event)
  "Called whenever the server proc exits"
  (setq kdc-server-proc nil)
  )

(defun kdc-start-server-if-necessary ()
  "If the server is not started, start it."

  (when (not kdc-server-proc)
    (setq kdc-server-proc
	  (start-process "completion-server" "*Messages*" 
			 (concat kdc-dir "server.py") 
			 ))

    (set-process-sentinel kdc-server-proc 'kdc-server-sentinel)
    (set-process-query-on-exit-flag kdc-server-proc nil)
    )
  )

(defun kdc-trigger-completion (prefix)
  "Attempts to initiate a completion. If data for given point already available, "
  "return it."
  (interactive)  

  (kdc-start-server-if-necessary)

  (if (and (not (eq ac-point (point)))
	   (eq kdc-completion-point ac-point))
      ;; Completions already available for point, no need to re-invoke client
      kdc-result-completions
    (if (eq kdc-loading-results nil)
	(progn 
	  (kdc-init-completion-request)
	  (kdc-trigger-client-process)
	  ))
    nil    
    ))

(defun ac-kdc-static-candidates(p)
  (if kdc-loading-results
      '("Loading.." "")
    kdc-result-completions
    ))

(defun ac-prefix-c-namespace ()
  "C-like languages ::."
  (if (re-search-backward "\\(?:\\.\\|::\\)\\(\\(?:[a-zA-Z0-9][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
      (match-beginning 1)))

(defun kdc-candidate-document(s)
  "Simply returns the associated help for a completion."
  (cdr (assoc s kdc-result-help)))

(defun kdc-prefix-member ()
  (or (ac-prefix-c-dot) 
      (ac-prefix-c-dot-ref) 
      (ac-prefix-c-namespace)
      ;; (ac-prefix-symbol)
      ))

;; The completion source that if necessary invokes a completion client request.
(ac-define-source kdc-member
  '((candidates . (kdc-trigger-completion ac-prefix))
    (prefix . kdc-prefix-member)
    (requires . 0)
    (document . kdc-candidate-document)
    (symbol . "m")))
 

;; The static completion source that's just used for showing a completion list
;; without invoking a completion client request. Used by client request process
;; sentinel.

(ac-define-source kdc-static
  '((candidates . (ac-kdc-static-candidates ac-prefix))
    (prefix . kdc-prefix-member)
    (requires . 0)
    (document . kdc-candidate-document)
    (symbol . "m")))

(defun ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-kdc-member) ac-sources 
			   )))
