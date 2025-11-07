(defsystem "hlbcalc"
  :description "HLBCalc: Hydrophilic-Lipophilic Balance Calculator"
  :author "Your Name"
  :license "Apache-2.0"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ("cl-ppcre"
               "cl-gtk4"
               "cl-gtk4.adw")
  :components 
  ((:module "source"
    :components
    ((:file "hlbcalc")
     (:file "main"  :depends-on ("hlbcalc")))))
  :in-order-to ((test-op (test-op "hlbcalc/tests"))))

;;; ==============================================================================
;;; Register Systems
;;; ==============================================================================
;; The function `register-system-packages' must be called to register packages
;; used or provided by your system when the name of the system/file that 
;; provides the package is not the same as the package name
;; (converted to lower case).
(register-system-packages "fiveam" '(:5am))

;;; ==============================================================================
;;; Secondary Systems
;;; ==============================================================================
(defsystem "hlbcalc/tests"
  :depends-on ("hlbcalc" "fiveam")
  :components ((:module "tests"
                :components
                ((:file "suite"))))
  :perform (test-op (o c) 
                    (symbol-call :fiveam :run! :suite)))
