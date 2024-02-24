;; -*- coding: utf-8; lexical-binding: t; -*-

;; 
;; JORP - v0.1 - simple project configuration 
;; by mursel alper gokcan (github.com/alpergkcan)
;;
;; [`About']
;; create and utilize simple configs,
;; and manage per project shortcuts
;;
;; [`Features']
;; + set compile commands and their shortcuts
;; + set file patterns for indexing (just opens buffers at the moment, but still usefull for lsp-servers)
;; + quickly reach your project directories and files 
;;
;; [`Sample']
;; @@@@@@@@@@@@ config.jorp @@@@@@@@@@@@@
;; @ Project Name                       @
;; @                                    @
;; @ [commands]                         @
;; @   <f5> ` "run.bat"                 @
;; @ S-<f5> ` "build.bat"               @
;; @   <f6> ` "debug.bat"               @
;; @                                    @
;; @ [files]                            @
;; @ *.bat                              @
;; @ source/*.cpp                       @
;; @ source/*.h                         @
;; @                                    @
;; @ [vars]                             @
;; @ debugger-target ` "proj.exe"       @
;; @                                    @
;; @ # comment sample,                  @
;; @ # vars is for elisp variables      @
;; @                                    @
;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;;
;; [`Note']
;; !! Dependency packages: `s' and `bind'
;;

(require 's)
(require 'bind)

;; UTILITY
(defun get-lines-as-list (file)
  "returns list of lines in FILE."
  (with-temp-buffer
	(insert-file-contents file)
	(split-string (buffer-string) "\n" t)))

(defun get-config-lines (config-file comment-char)
  "returns list of lines in CONFIG-FILE."
  (setq-local all-config-lines (get-lines-as-list config-file))
  (setq-local config-lines (list))
  (let ((config-line-count (length all-config-lines)))
	   (setq-local config-line-index 0)
	   (while (< config-line-index config-line-count)
		      (setq-local current-config-line (nth config-line-index all-config-lines))
			  (setq-local config-line-index (1+ config-line-index))
			  (if (not (s-starts-with? "#" current-config-line))
				  (add-to-list 'config-lines current-config-line))))
  (nreverse config-lines))

(defun get-index-of (element list);;
  "returns index of 'ELEMENT' in 'LIST'"
  (let ((element-index (cl-position element list :test 'equal)) )
	   (if  (equal element-index nil)
			-1
		    element-index)))

(defun get-single-arg-from-list (list prompt-str)
  "pass this to the interactive for reading argument from a list collection"
  (list (let ((completion-ignore-case t)
			  (prompt prompt-str))
		     (completing-read prompt
							  list
							  nil
							  t))))

(defun eval-string (string)
  "eval elisp code stored in a string."
  (eval (car (read-from-string string))))

(defun format-numbered (string &rest args-list)
  "accepts $x where x is index of argument"
  (s-format string 'elt args-list))

(defun jorp-apply-to-each (list-var func)
  (setq-local item-count (length list-var))
  (setq-local new-list (list))
  (setq-local item-index (- item-count 1))
  (while (>= item-index 0)
	(add-to-list 'new-list (funcall func (nth item-index list-var)))
	(setq-local item-index (- item-index 1)))
  (append new-list (list)))

;; following region is for loading list of files with idle timer 
(setq jorp-file-idle-loading t)
(setq jorp-idle-file-batch-size 4) ; set to your liking
(setq jorp-idle-file-sec 0.5)	   ; set to your liking

(setq jorp-idle-file-index 0)
(setq jorp-idle-file-list nil)
(setq jorp-idle-file-timer nil)

(defun find-files-batched (filelist batch-size start-index)
  "find-file-noselect on 'batch-size' many files from 'filelist' starting from 'start-index'"
  (setq-local num-to-load batch-size)
  (setq-local num-left (- (length filelist) start-index))
  (if (< num-left batch-size) (setq-local num-to-load num-left))
  (setq-local index start-index)
  (while (< index (+ start-index num-to-load)) 
	(let ()
	  (setq-local file (nth index filelist))
	  (find-file-noselect file)
	  (message (format "[%s] indexed '%s'" jorp-name file))
	  (setq-local index (+ index 1)))))
(defun find-files-idle-inner ()
  "find-file from 'jorp-idle-file-list' in 'jorp-idle-file-index'"
  (if (equal jorp-idle-file-list nil)
	  (let ()
		  (cancel-timer jorp-idle-file-timer)
		  (setq jorp-idle-file-index 0)
		  (setq jorp-idle-file-timer nil)
		  (message (format "[%s] no files to index!" jorp-name)))
	  (if (>= jorp-idle-file-index (length jorp-idle-file-list))
		  (let ()						; no more files
			   (cancel-timer jorp-idle-file-timer)
			   (setq jorp-idle-file-index 0)
			   (setq jorp-idle-file-list nil)
			   (setq jorp-idle-file-timer nil)
			   (message (format "[%s] indexing complete!" jorp-name)))
		  (let ()						; find-file
			   (find-files-batched jorp-idle-file-list jorp-idle-file-batch-size jorp-idle-file-index)
			   (setq jorp-idle-file-index (+ jorp-idle-file-index jorp-idle-file-batch-size))))))
(defun find-files-idle  (filelist)
  "find-file files one by one each 'jorp-idle-file-sec' idle seconds from 'FILELIST'"
  (if (equal jorp-idle-file-list nil) ; are we already loading some files?
	  (let ()
		(setq jorp-idle-file-list filelist)
		(setq jorp-idle-file-index 0)
		(message (format "[%s] indexing started for %s files!" jorp-name (number-to-string (length filelist))))
		(find-files-idle-inner)
		(setq jorp-idle-file-timer 
			  (run-with-idle-timer jorp-idle-file-sec t 'find-files-idle-inner)))
	  (let ()
		(setq jorp-idle-file-list (append jorp-idle-file-list filelist))
		(message (format "[%s] more %s files will be indexed!" jorp-name (number-to-string (length filelist)))))))

;;;; JORP
;; Constants
(setq jorp-projects-file     "~/.emacs.d/projects.jorp")
(setq jorp-config-filename   "config.jorp")
(setq jorp-config-command-seperator "=")
(setq jorp-config-commands-string   "commands")
(setq jorp-config-binds-string      "binds")
(setq jorp-config-vars-string       "vars")
(setq jorp-config-files-string      "files")
(setq jorp-config-comment-char      "#")
(setq jorp-config-default-content (format-numbered "Project Name \n\
\n\
[$0]\n\
  <f5>$1\"make -k\"\n\
S-<f5>$1\"make configure\"\n\
\n\
$5 files can contain wildcards,  'some_dir/*.cpp' etc.\n\
[$2]\n\
$3\n\
Makefile\n\
[$4]\n\
rdbg-executable ` remedybg.exe\n\
\n"
												   jorp-config-commands-string jorp-config-command-seperator
												   jorp-config-files-string jorp-config-filename
												   jorp-config-vars-string
												   jorp-config-comment-char))

;; runtime vars
(setq jorp-dir      "")
(setq jorp-file     "")
(setq jorp-name     "")
(setq jorp-lines    nil)
(setq jorp-commands nil)
(setq jorp-binds nil)
(setq jorp-vars     nil)
(setq jorp-files    nil)

;; Private Functions
(defun jorp-get-dirs ()
  "returns list of jorp dirs"
  (if (file-exists-p jorp-projects-file)
	  (jorp-apply-to-each (get-lines-as-list jorp-projects-file) 's-trim)
	  (list "")))

(defun jorp-command (string)
  "compiles with command 'STRING' using 'compile' function."
  (setq-local temp-dir default-directory)
  (setq default-directory (concat jorp-dir "/"))
  (compile string)
  (setq default-directory temp-dir))

(defun clear-old-commands ()
  "unbind shortcuts for previously set commands from `jorp-commands' configured by 'config.jorp'"
  (setq-local old-command-index 0)
  (setq-local command-count (length jorp-commands))
  (while (< old-command-index command-count)
		 (setq-local key   (s-trim (nth 0 (split-string (nth old-command-index jorp-commands) jorp-config-command-seperator t))))
	     (setq-local old-command-index (+ old-command-index 1))
		 (setq-local jorp-name-compact (s-replace " " "" jorp-name))
		 ;; (eval-string (format "(unbind-key (kbd \"%s\"))"
		 ;; 					  key))
		 (eval-string (format "(fmakunbound '%s-%s)"
							  jorp-name-compact key))
		 (eval-string (format "(makunbound  'j-o-r-p-%s-%s-command-string)"
							  jorp-name-compact key)))
  (setq jorp-commands nil))

(defun clear-old-binds ()
  "unbind shortcuts for previously set commands from `jorp-commands' configured by 'config.jorp'"
  (setq-local old-bind-index 0)
  (setq-local bind-count (length jorp-binds))
  (while (< old-bind-index bind-count)
		 (setq-local key   (s-trim (nth 0 (split-string (nth old-bind-index jorp-binds) jorp-config-command-seperator t))))
	     (setq-local old-bind-index (+ old-bind-index 1))
		 (setq-local jorp-name-compact (s-replace " " "" jorp-name))
		 (eval-string (format "(unbind-key (kbd \"%s\"))"
							  key))
		 ;; (eval-string (format "(fmakunbound '%s-%s)"
		 ;; 					  jorp-name-compact key))
		 ;; (eval-string (format "(makunbound  'j-o-r-p-%s-%s-command-string)"
		 ;; 					  jorp-name-compact key))
		 )
  (setq jorp-binds nil))

(defun jorp-fetch-config ()
  "fetches config fields from 'jorp-file'"
  (setq jorp-lines (jorp-apply-to-each (get-config-lines jorp-file jorp-config-comment-char) 's-trim))
  (setq jorp-name  (s-trim (nth 0 jorp-lines)))
  (setq jorp-headers (list))
  (set-frame-name jorp-name)
  (let ((line-count (length jorp-lines)))
	   (setq-local config-line-index 1)
	   (while (< config-line-index line-count)
		      (setq-local config-line (nth config-line-index jorp-lines))
			  (setq-local config-line-index (+ config-line-index 1))
			  (if (s-starts-with? "[" config-line)
				  (add-to-list 'jorp-headers (s-chop-right 1 (s-chop-left 1 config-line)))))))

(setq result-lines nil)
(defun jorp-get-lines-of-type (type)
  "sets 'result-lines' to the lines inside config region marked as '[type]'"
  (setq result-lines (list))
  (let ((line-count (length jorp-lines))
		(jorp-header-index (get-index-of (format "[%s]" type) jorp-lines)))
	   (setq-local line-index (+ jorp-header-index 1))
	   (while (< line-index line-count)
		      (setq-local jorp-line (nth line-index jorp-lines))
			  (setq-local line-index (+ line-index 1))
			  (if (s-starts-with? "[" jorp-line)
				  (setq-local line-index line-count)
				  (add-to-list 'result-lines jorp-line)))))

(defun jorp-load-commands ()
  "loads and sets 'jorp-commands' from 'jorp-file'"
    (if (not (equal jorp-commands nil))
		(clear-old-commands))
	(jorp-get-lines-of-type jorp-config-commands-string)
	;; (message jorp-headers)
	;; (message result-lines)
	(let ((line-count (length result-lines))
		  (jorp-name-compact (s-replace " " "" jorp-name)))
		 (setq jorp-commands (list))
		 (setq-local command-index 0)
		 (while (< command-index line-count)
				(setq-local command-line (nth command-index result-lines))
				(add-to-list 'jorp-commands command-line)
				(setq-local command-index (1+ command-index))
				(setq-local current-command (split-string command-line jorp-config-command-seperator t))
				(setq-local key     (s-trim (nth 0 current-command)))
				(setq-local command (s-trim (nth 1 current-command)))
				(eval-string (format "(setq  j-o-r-p-%s-%s-command-string %s)"
									 jorp-name-compact key command))
				(eval-string (format "(defun %s-%s () (interactive) (jorp-command j-o-r-p-%s-%s-command-string))"
									 jorp-name-compact key jorp-name-compact key))
				;; (eval-string (format "(global-set-key (kbd \"%s\") '%s-%s)"
				;; 					 key jorp-name-compact key))
				)))

(defun jorp-load-binds ()
    "loads and sets 'jorp-binds' from 'jorp-file'"
	(if (not (equal jorp-binds nil))
		(clear-old-binds))
	(jorp-get-lines-of-type jorp-config-binds-string)
	(let ((line-count (length result-lines))
		  (jorp-name-compact (s-replace " " "" jorp-name)))
	     (setq jorp-binds (list))
		 (setq-local bind-index 0)
		 (while (< bind-index line-count)
		     (setq-local bind-line (nth bind-index result-lines))
			 (add-to-list 'jorp-binds bind-line)
			 (setq-local bind-index (1+ bind-index))
			 (setq-local current-bind (split-string bind-line jorp-config-command-seperator t))
			 (setq-local key     (s-trim (nth 0 current-bind)))
			 (setq-local command (s-trim (nth 1 current-bind)))
			 ;; (eval-string (format "(setq  j-o-r-p-%s-%s-command-string %s)"
			 ;; 					  jorp-name-compact key command))
			 ;; (eval-string (format "(defun %s-%s () (interactive) (jorp-command j-o-r-p-%s-%s-command-string))"
			 ;; 					  jorp-name-compact key jorp-name-compact key))
			 (eval-string (format "(global-set-key (kbd \"%s\") '%s-%s)"
								 key jorp-name-compact command))
			 )))


(defun jorp-load-vars ()
  "loads and sets 'jorp-vars' from 'jorp-file'"
	(jorp-get-lines-of-type jorp-config-vars-string)
	(let ((line-count (length result-lines))
		  (jorp-name-compact (s-replace " " "" jorp-name)))
		  (setq jorp-vars (list))
		  (setq-local var-index 0)
		  (while (< var-index line-count)
				 (setq-local jorp-var (nth var-index result-lines))
				 (setq-local var-index (+ var-index 1))
				 (setq-local current-var (split-string jorp-var jorp-config-command-seperator t))
				 (setq-local var-name  (s-trim (nth 0 current-var)))
				 (setq-local var-value (s-trim (nth 1 current-var)))				  
				 (add-to-list 'jorp-vars jorp-var)
				 (setq-local set-var-string (format "(setq %s %s)" var-name var-value))
				 (eval-string set-var-string))))

(defun jorp-load-files ()
  "loads and sets 'jorp-files' from 'jorp-file'"
	(jorp-get-lines-of-type jorp-config-files-string)
	(let ((line-count (length result-lines))
		  (jorp-name-compact (s-replace " " "" jorp-name)))
		  (setq jorp-files (list))
		  (setq-local file-index 0)
		  (while (< file-index line-count)
				 (setq-local jorp-file-pattern (nth file-index result-lines))
				 (setq-local file-index (+ file-index 1))
				 (setq jorp-files (append jorp-files (file-expand-wildcards (concat (concat jorp-dir "/") jorp-file-pattern))))))
		  (if jorp-idle-file-loading
			  (find-files-idle jorp-files)
			  (find-files-batched jorp-files (length jorp-files) 0)))

(defun jorp-load-with-confirmation (should-load)
  "loads 'jorp-load-target' with interactive confirmation"
  (interactive (list (y-or-n-p (format "[%s] Load %s?" jorp-name jorp-load-target))))
  (if (not (equal jorp-load-target ""))
	  (if should-load (eval-string (format "(jorp-load-%s)" jorp-load-target)))))

(setq jorp-load-target "")

(defun jorp-load (dir)
  "loads jorp 'DIR' commands and files."
  (setq jorp-dir (s-chop-right 1 dir))
  (setq jorp-file (concat (concat jorp-dir "/") jorp-config-filename))
  (setq default-directory (concat jorp-dir "/"))
  (jorp-fetch-config)
  (let ((header-count (length jorp-headers)))
	   (setq-local header-index 0)
	   (while (< header-index header-count)
	          (setq jorp-load-target (nth (- (- header-count header-index) 1) jorp-headers))
			  (setq-local header-index (1+ header-index))
			  (jorp-load-with-confirmation t))))

;; Public Functions
(defun jorp-add (dir)
  "adds existing jorp 'DIR' to managed projects"
  (interactive "D")						 ; get dir
  (if (member dir (jorp-get-dirs))
	  (message (format "[JORP] %s is already being managed!" dir))
	  (if (file-exists-p (concat dir jorp-config-filename))
		  (write-region (concat dir "\n")	 ; start
						nil				 ; end
						jorp-projects-file ; filename
						t)
	      (message "[JORP] given directory does not contain config.jorp"))))

(defun jorp-remove (dir-l should-delete-config)
  "removes jorp 'DIR' from managed projects."
  (interactive (list
				(get-single-arg-from-list (jorp-get-dirs) "[JORP] Which project?")
				(y-or-n-p (format "[JORP] Delete config file '%s'?" jorp-config-filename))))
  (let ((config-file (concat dir jorp-config-filename)))
 	   (if (and should-delete-config
				(file-exists-p config-file))
		   (delete-file config-file))
	   (if (equal config-file jorp-file)
		   (let ()
			    (clear-old-commands)
			    (setq jorp-file  (setq jorp-name     (setq jorp-dir    "")))
				(setq jorp-lines (setq jorp-commands (setq jorp-files nil))))))
  (setq dir (nth 0 dir-l))
  (if (equal dir "")
	  (message "[JORP] no project is configured. Try `jorp-create'")
	  (let ((dirs (jorp-get-dirs)))
		   (delete-file jorp-projects-file)
		   (setq-local dir-count (length dirs))
		   (setq-local dir-index 0)
	  	   (while (< dir-index dir-count)
		          (setq-local cur-dir (nth dir-index dirs))
				  (setq-local dir-index (1+ dir-index))
				  (if (or (equal cur-dir "")
						  (equal cur-dir dir))
					  (list nil)
					(jorp-add cur-dir))))))

(defun jorp-open (dir)
  "Opens jorp 'DIR' loads."
  (interactive (get-single-arg-from-list (jorp-get-dirs) "Which project? "))
  (if (equal dir "")
	  (message "[JORP] no project is configured. Try `jorp-create'")
	  (let ()
		   (find-file dir)
		   (jorp-load dir))))

(defun jorp-reload ()
  "reloads current jorp project."
  (interactive)
  (if (equal jorp-name "")
	  (message "[JORP] No project is currently loaded!")
	  (let ()
		   (jorp-fetch-config)
		   (let ((line-count (length jorp-headers)))
			    (setq-local header-index 0)
				(while (< header-index line-count)
					   (setq jorp-load-target (nth (1- (- line-count header-index)) jorp-headers))
				       (setq-local header-index (1+ header-index))
					   (call-interactively 'jorp-load-with-confirmation))))))
			   
(defun jorp-create (dir)
  "Create jorp project on DIR"
  (interactive "D")						; get dir
  (setq-local filename (concat dir jorp-config-filename))
  (if (file-exists-p filename)
	  (message (format "[JORP] %s already exists. Delete before re-creating." filename))
	  (let ()
		   (write-region jorp-config-default-content ; start
						 nil						 ; end
						 filename)
		   (jorp-add  dir)
		   (jorp-load dir)
		   (find-file jorp-file))))

(defun jorp-config ()
  "Open current config.jorp that is 'jorp-file'"
  (interactive)
  (if (equal jorp-file "")
	  (message "[JORP] no project is loaded!")
	  (find-file jorp-file)))

(bind (setq jorp-map (make-sparse-keymap))
	  "A" #'jorp-add
	  "C" #'jorp-create
	  "R" #'jorp-remove
	  "o" #'jorp-open
	  "r" #'jorp-reload
	  "c" #'jorp-config)

(provide 'jorp)
