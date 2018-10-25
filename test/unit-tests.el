;;; unit-tests.el --- Unit tests for counsel-gtags   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Felipe Lema

;; Author: Felipe Lema <felipel@devuan-pega>
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
;;; See http://sachachua.com/blog/2015/02/continuous-integration-code-coverage-emacs-packages-travis-coveralls/
;;; See https://www.atlassian.com/continuous-delivery/continuous-integration-tutorial
;;; Because of an extra `find-file' and me running this interactively, you'll
;;; see a lot of (save-window-excursion …)


;;

;;; Code:

;; See https://emacs.stackexchange.com/a/19458/10785
(eval-when-compile (require 'cl))
(require 'f)
(require 'dash)
(require 's)

(require 'counsel-gtags)

;;;;;;;;;;;;;;;;;
;; Infrastructure
;;;;;;;;;;;;;;;;;
(defmacro counsel-gtags--with-mock-project (&rest body)
  "Create mock project with source file and execute BODY.

You can access the project at `default-directory', non-empty source file and
header at `main-file-path' and `main-header-path'."
  `(let* ((project-path (make-temp-file
			 "counsel-gtags-unit-test-mock-project"
			 t))
	  (default-directory project-path)
	  (main-file-path (concat
			   (file-name-as-directory
			    default-directory)
			   "main.c"))
	  (main-header-path (concat
			     (file-name-as-directory
			      default-directory)
			     "main.h"))
	  (main-header-text "char *header_string;
int header_int;
float header_float;
void global_fun();
void another_global_fun();
")
	  (main-file-text  "
#include \"main.h\"

char *global_string;
int global_int;
float global_float;
void global_fun(){
    write(header_string);
    write(header_header_int);
}
void another_global_fun(){
    write(header_header_int);
}
int main{
    int local_int;
}")
	  )
     (f-write-text main-file-text 'utf-8 main-file-path)
     (f-write-text main-header-text 'utf-8 main-header-path)
     (call-process "gtags")
     (save-window-excursion
       (let ((__result ,@body))
	 ;; clean up buffers that may have been opened in the process before
	 ;; returning
	 (let* ((buffers-&-paths (mapcar
				  (lambda (buffer)
				    (list buffer
					  (with-current-buffer buffer
					    (and buffer-file-name
						 (file-name-directory buffer-file-name)))))
				  (buffer-list)))
		(buffers-from-project (remove-if-not
				       (lambda (buffer-&-path)
					 (and (stringp (cadr buffer-&-path))
					      (string-equal (cadr buffer-&-path)
							    project-path)))
				       buffers-&-paths)))
	   (dolist (buffer-&-path buffers-from-project)
	     (kill-buffer (cadr buffer-&-path))))
	 __result))))

(ert-deftest 00-can-create-project ()
  (counsel-gtags--with-mock-project
   (should (stringp (f-read main-file-path 'utf-8)))))

(ert-deftest 00-can-create-db-and-read-symbols ()
  "For now, we can only create tags interactively"
  (counsel-gtags--with-mock-project
   (should
    (string-equal (counsel-gtags--default-directory)
		  (file-name-as-directory default-directory)))))
(ert-deftest 00-gtags-command-works ()
  (counsel-gtags--with-mock-project
   (let* ((from-cmd (shell-command-to-string "global --result=grep another_global_fun"))
	  (clean (s-trim from-cmd)))
     (should (string-equal
	      "main.c:11:void another_global_fun(){"
	      clean)))))

(ert-deftest 00-can-query ()
  "For now, we can only create tags interactively"
  (counsel-gtags--with-mock-project
   (should
    (string-equal (counsel-gtags--default-directory)
		  (file-name-as-directory default-directory)))))
;;;;;;;;;;;;;;;;;
;; Actual testing
;;;;;;;;;;;;;;;;;
(ert-deftest correct-collection-of-candidates ()
  (counsel-gtags--with-mock-project
   (let* ((root (counsel-gtags--default-directory))
          (default-directory root)
	  (type 'definition)
	  (tagname "another_global_fun")
          (encoding buffer-file-coding-system)
	  (extra-options)
	  (expected '("main.c:11:void another_global_fun(){"))
	  (collected (counsel-gtags--collect-candidates type tagname encoding extra-options)))
     (should
      (-intersection collected expected)))))

(ert-deftest default-case-sensitive ()
  (let ((ivy-auto-select-single-candidate t))
    (counsel-gtags--with-mock-project
     (should
      (string-prefix-p "main.c"
		       (counsel-gtags-find-definition
			"another_global_fun"))))))

(ert-deftest ignore-case ()
  (let ((ivy-auto-select-single-candidate t)
	(counsel-gtags-ignore-case t))
    (counsel-gtags--with-mock-project
     (should
      (string-prefix-p "main.c"
		       (counsel-gtags-find-definition
			"ANOTHER_GLOBAL_FUN"))))))

(ert-deftest match_anywhere_in_symbol ()
  "Test that we can search for a string anywhere in each candidate, not just the start.
Should match both functions defined."
  (counsel-gtags--with-mock-project
   (should
    (s-equals?
     "another_global_fun
global_fun
"
     (shell-command-to-string
      (counsel-gtags--build-command-for-candidates 'definition "_fun"))))))

(provide 'unit-tests)
;;; unit-tests.el ends here
