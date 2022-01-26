;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; parse.el
;;
;; Takes a work order as a string, and extracts the pertinent
;; information. Inserts a :PROPERTIES: drawer in your kill-ring.
;;
;; 2017-03-11 Justin St-Amant
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse ()
  "Takes a work order as a string, and extracts the pertinent
information. Inserts a :PROPERTIES: drawer in your kill-ring for
use with org-mode."
  ;;(interactive) ; Necessary?
  (let ((work-order (read-string "WO: "))
        (wo-number nil)
        (wo-description nil)
        (wo-start-date nil))
    ;; Extract information from work-order
    (string-match "^WORK ORDER...: \\(.*$\\)" work-order)
    (setq wo-number (match-string 1 work-order))
    (string-match "^DESCRIPTION..: \\(.*$\\)" work-order)
    (setq wo-description (match-string 1 work-order))
    (string-match "^START DATE...: \\(.*$\\)" work-order)
    (setq wo-start-date (match-string 1 work-order))

    ;; Generate :PROPERTIES: drawer
    (concat
     ":PROPERTIES:\n"
     ":added: \n"
     ":wo: " wo-number "\n"
     ":END:\n")))
  ;;   ":PROPERTIES:
  ;; :added: [2017-03-11 Sat]
  ;; :wo: NUM
  ;; :END:"

  ;; Insert :PROPERTIES drawere in kill-ring
  ;; (command)
  
