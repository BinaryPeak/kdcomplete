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

(defun kdc-create-arg-string (arg-list)  
  (if (eq major-mode 'objc-mode)
      (mapconcat 'identity arg-list " ")
    (concat "(" (mapconcat 'identity arg-list ", ") ")")
    )
  )

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
	    		 (kdc-create-arg-string (cdr (assoc 'arguments overload)))
			 )
	    	 )
	      (cdr (assoc 'overloads completion))
	      "\n"
	      ))))
  	 items
  	 )))

(defun kdc-create-completion-types (items)
  (setq kdc-result-types
	(mapcar
	 (lambda (completion) 
	   (cons (cdr (assoc 'word completion)) (cdr (assoc 'type completion)))
	   )
	 items
	 )
	)
  )

(defun kdc-create-overloads (items)
  (setq kdc-result-overloads
  	(mapcar
  	 (lambda (completion) 
  	   ;; For each completion, create alist pair	   
  	  (cons
	    (cdr (assoc 'word completion))
	     ;; Create a list of strings with (<param>..) syntax
	     (mapcar
	      (lambda (overload)
		(if (string-equal (cdr (assoc 'type completion)) "function-call")
		    (kdc-create-arg-string (cdr (assoc 'arguments overload)))
		  ""
		  )		
		)
	      (cdr (assoc 'overloads completion))
	      )))
  	 items
  	 ))
  kdc-result-overloads
  )

(defun kdc-extract-completions (str)
  "Given a JSON reply from the server, create help alist and completion list"
  ;; (setq str kdc-result-string)

  (setq kdc-result-completions (json-read-from-string str))

  (kdc-create-completion-types kdc-result-completions)
  (kdc-create-overloads kdc-result-completions)
  (kdc-create-help kdc-result-completions)
  
  ;; Strip everything but the completion word for the completion list
  (setq kdc-result-completions 
	(mapcar (lambda (arg) (cdr (assoc 'word arg))) kdc-result-completions))

  ;; Append empty element to force showing of menu (should be a better way)
  (if (eq (length kdc-result-completions) 1 )
      (setq kdc-result-completions (nreverse (cons "" kdc-result-completions)))
      )

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

  (if kdc-force-loose
      (ac-complete-kdc-static-loose)
    (ac-complete-kdc-static)
    ) 
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
	 (start-process "completion-client" "*kdcomplete*" 
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
	  (start-process "completion-server" "*kdcomplete*" 
			 (concat kdc-dir "server.py") 
			 ;; (concat kdc-dir "server.py")  "--run-once"
			 ))

    (set-process-sentinel kdc-server-proc 'kdc-server-sentinel)
    (set-process-query-on-exit-flag kdc-server-proc nil)
    )
  )

(defun kdc-trigger-completion (prefix loose)
  "Attempts to initiate a completion. If data for given point already available, "
  "return it."
  (interactive)    

  (setq kdc-force-loose loose)

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
    )
)

(defun kdc-static-candidates(p)
  (if kdc-loading-results
      '("Loading.." "")
    kdc-result-completions
    )
)

(defun ac-prefix-c-namespace ()
  "C-like languages ::."
  (if (re-search-backward "\\(?:\\.\\|::\\)\\(\\(?:[a-zA-Z0-9][_a-zA-Z0-9]*\\)?\\)\\=" nil t)
      (match-beginning 1)))

(defun kdc-candidate-document(s)
  "Simply returns the associated help for a completion."
  (cdr (assoc s kdc-result-help)))

(defun kdc-objc-p ()
  (eq major-mode 'objc-mode)
  )

(defun ac-prefix-objc ()
  "Objective C, [var ."
  (let ((start-point (point)))
    (when (re-search-backward "\\(\\[\[a-zA-Z0-9][._a-zA-Z0-9]*\\)\\( +[_a-zA-Z0-9]*\\)\\=" nil t)
      (+ 1 (match-beginning 2)))
    )
  )

(defun ac-prefix-objc-selector-2 ()
  "Objective C selector, -(] ] ) ."
  (when (re-search-backward "\\(\\]\\) *[<>_a-zA-Z0-9]*\\=" nil t)
    (+ 2 (match-beginning 1))    
    )
  )

(defun ac-prefix-objc-selector ()
  "Objective C selector, -([var] ) ."
  (let ((start-point (point)))
    (if (re-search-backward "- *([a-zA-Z0-9][<>_a-zA-Z0-9]*\\*?)\\( *[_a-zA-Z0-9]*\\)\\=\\=" nil t)
	(progn
	  (+ 1 (match-beginning 1)))
      )
    )
  )

(defun kdc-prefix-member ()
  (or (ac-prefix-c-dot) 
      (ac-prefix-c-dot-ref) 
      (ac-prefix-c-namespace)

      (if (kdc-objc-p)
      	(or      
      	  (ac-prefix-objc)
      	  (ac-prefix-objc-selector)
	  (ac-prefix-objc-selector-2)
      	  )
      	)
      )
)

(defun kdc-test-prefix ()
  (interactive)
  ;; (ac-complete-kdc-member)
  (ac-complete-kdc-member-loose)

  ;; (save-excursion
  ;;   (print (ac-prefix-objc-selector-2))
  ;;   )
  )

(global-set-key (quote [67109092]) (quote kdc-test-prefix))

(defun kdc-function-overload-candidates ()
  (let (
	(complete-str (substring-no-properties (cdr ac-last-completion)))
	)

    (cdr (assoc complete-str kdc-result-overloads))
    )
)

(defun kdc-member-action ()
  (let (
	(complete-str (substring-no-properties (cdr ac-last-completion)))
	)

    (setq ac-parent-completion complete-str)
    (setq kdc-ac-template-point (point))
    (ac-complete-kdc-function-overload)    
    )
)

(defun kdc-template-post-action ()
  (remove-hook 'yas/after-exit-snippet-hook 'kdc-template-post-action)
)

(defun kdc-template-action ()
  (let (
  	(end-p (point))
  	(complete-str (substring-no-properties (cdr ac-last-completion)))
	(complete-type (cdr (assoc ac-parent-completion kdc-result-types)))
  	)

    (if (and  (not (string-equal "()" complete-str))
	      (not (string-equal "" complete-str))
	      )
	(progn 
	  
	  (if (kdc-objc-p)
	      (progn
		(setq complete-str (replace-regexp-in-string "(" "${(" complete-str))
		(setq complete-str (replace-regexp-in-string ")" ")}" complete-str))
		)
	    (progn
	      (setq complete-str (replace-regexp-in-string "(" "(${" complete-str))
	      (setq complete-str (replace-regexp-in-string ")" "})" complete-str))
	      (setq complete-str (replace-regexp-in-string ", " "}, ${" complete-str))
	      )
	    )
	  

	  (goto-char kdc-ac-template-point)
	  (when (string-equal complete-type "function-definition")
	    (progn
	      (add-hook 'yas/after-exit-snippet-hook 'kdc-template-post-action)
	      )
	    )
	  (yas/expand-snippet complete-str (point) end-p)

	  )
      )
    )
  )

;; The completion source that if necessary invokes a completion client request.
(ac-define-source kdc-function-overload
  '((candidates . (kdc-function-overload-candidates))
    (prefix . point)
    (requires . 0)
    (action . kdc-template-action)
    (symbol . "f")))
 
;; The completion source that if necessary invokes a completion client request.
(ac-define-source kdc-member
  '((candidates . (kdc-trigger-completion ac-prefix nil))
    (prefix . kdc-prefix-member)
    (requires . 0)
    (action . kdc-member-action)
    (document . kdc-candidate-document)
    (symbol . "m")))

;; The static completion source that's just used for showing a completion list
;; without invoking a completion client request. Used by client request process
;; sentinel.
(ac-define-source kdc-static
  '((candidates . (kdc-static-candidates ac-prefix))
    (prefix . kdc-prefix-member)
    (requires . 0)
    (action . kdc-member-action)
    (document . kdc-candidate-document)
    (symbol . "m")))

(ac-define-source kdc-static-loose
  '((candidates . (kdc-static-candidates ac-prefix))
    (requires . 0)
    (action . kdc-member-action)
    (document . kdc-candidate-document)
    (symbol . "m")))

(ac-define-source kdc-member-loose
  '((candidates . (kdc-trigger-completion ac-prefix t))
    (requires . 0)
    (action . kdc-member-action)
    (document . kdc-candidate-document)
    (symbol . "m")))


   
(defun ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-kdc-member) ac-sources 
			   )))

(setq ac-expand-on-auto-complete nil)
