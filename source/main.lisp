(defpackage :hlbcalc.adw
  (:nicknames :main)
  (:use :cl :gtk4 :hlbcalc)
  (:export #:main)
  (:documentation "Frontend"))
(in-package :hlbcalc.adw)


;; ==============================================================================
;; ADW/GTK4 Frontend
;; ==============================================================================

(define-application (:name hlbcalc
                     :id "org.hlbcalc.app")
    (define-main-window (window (adw:make-application-window :app *application*))
        (let ((expression nil))
          (widget-add-css-class window "devel")
          (setf (widget-size-request window) '(400 600))
          (let ((box (make-box :orientation +orientation-vertical+
                               :spacing 0)))
            (setf (adw:window-content window) box)
            (let ((header-bar (adw:make-header-bar)))
              (setf (adw:header-bar-title-widget header-bar)
                    (adw:make-window-title :title "HLBCALC"
                                           :subtitle "Hydrophilic-Lipophilic Balance Calculator"))
              (box-append box header-bar))
            (let ((carousel (adw:make-carousel)))
              (setf (widget-hexpand-p carousel) t
                    (widget-vexpand-p carousel) t
                    (adw:carousel-interactive-p carousel) t)
              (let ((page (adw:make-status-page)))
                (setf (widget-hexpand-p page) t
                      (widget-vexpand-p page) t
                      (adw:status-page-icon-name page) "utilities-terminal-symbolic"
                      (adw:status-page-title page) "HLBCALC - HLB Calculator"
                      (adw:status-page-description page) " ")
                (flet ((calculate-hlb (widget)
                         (declare (ignore widget))
                         (when expression
                           (setf (adw:status-page-description page)
                                 (princ-to-string
                                  (handler-case (eval expression)
                                    (error (err) err)))))))
                  (let ((box (make-box :orientation +orientation-vertical+
                                       :spacing 0)))
                    (let ((group (adw:make-preferences-group)))
                      (setf (widget-margin-all group) 10)
                      (let ((hydro-row (adw:make-action-row)))
                        (setf (adw:preferences-row-title hydro-row) "Hydrophilic mass")
                        (let ((hydro-entry (make-entry)))
                          (setf (widget-valign hydro-entry) +align-center+
                                (widget-hexpand-p hydro-entry) t)
                          (connect
                           hydro-entry "changed"
                           (lambda (entry)
                             (let ((hydro-text (entry-buffer-text (entry-buffer hydro-entry))))
                               (setf expression
                                     (ignore-errors
                                      `(hlbcalc:calculate-hlb
                                        ,(read-from-string hydro-text)
                                        ,(read-from-string (entry-buffer-text (entry-buffer hydro-entry))))))
                               (funcall (if expression #'widget-remove-css-class #'widget-add-css-class) entry "error"))))
                          (connect hydro-entry "activate" #'calculate-hlb)
                          (adw:action-row-add-suffix hydro-row hydro-entry))
                        (let ((lipo-row (adw:make-action-row)))
                          (setf (adw:preferences-row-title lipo-row) "Lipophilic mass")
                          (let ((lipo-entry (make-entry)))
                            (setf (widget-valign lipo-entry) +align-center+
                                  (widget-hexpand-p lipo-entry) t)
                            (connect
                             lipo-entry "changed"
                             (lambda (entry)
                               (let ((lipo-text (entry-buffer-text (entry-buffer lipo-entry))))
                                 (setf expression
                                       (ignore-errors
                                        `(hlbcalc:calculate-hlb
                                          ,(read-from-string (entry-buffer-text (entry-buffer lipo-entry)))
                                          ,(read-from-string lipo-text))))
                                 (funcall (if expression #'widget-remove-css-class #'widget-add-css-class) entry "error"))))
                            (connect lipo-entry "activate" #'calculate-hlb)
                            (adw:action-row-add-suffix lipo-row lipo-entry))
                          (adw:preferences-group-add group lipo-row))
                        (adw:preferences-group-add group hydro-row))
                      (box-append box group))
                    (let ((carousel-box box)
                          (box (make-box :orientation +orientation-horizontal+
                                         :spacing 0)))
                      (setf (widget-hexpand-p box) t
                            (widget-halign box) +align-fill+)
                      (let ((button (make-button :label "Exit")))
                        (setf (widget-css-classes button) '("pill")
                              (widget-margin-all button) 10
                              (widget-hexpand-p button) t)
                        (connect button "clicked"
                                 (lambda (button)
                                   (declare (ignore button))
                                   (window-destroy window)))
                        (box-append box button))
                      (let ((button (make-button :label "Calc")))
                        (setf (widget-css-classes button) '("suggested-action" "pill")
                              (widget-margin-all button) 10
                              (widget-hexpand-p button) t)
                        (connect button "clicked" #'calculate-hlb)
                        (box-append box button))
                      (box-append carousel-box box))
                    (setf (adw:status-page-child page) box)))
                (adw:carousel-append carousel page))
              (box-append box carousel)))
          (unless (widget-visible-p window)
            (window-present window)))))

;; ==============================================================================
;; Application
;; ==============================================================================

(defun main ()
  (unless (adw:initialized-p)
    (adw:init))
  (hlbcalc))
