;;; plantuml-archimate.el --- Archimate helpers for PlantUML  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  S. Shyam Sundar

;; Author: S. Shyam Sundar
;; Keywords: convenience


;;; Commentary:

;;

;;; Code:

(defvar plantuml--archimate-element-shortnames
	'(("collaboration" . "Coll")
		("component" . "Comp")
		("data" . "Data")
		("event" . "Evt")
		("function" . "Func")
		("interaction" . "Intr")
		("interface" . "Int")
		("process" . "Proc")
		("service" . "Svc")
		("filled" . "Fld")
		("unidirect" . "Unid")
		("activity" . "Acti")
		("actor" . "Act")
		("contract" . "Ctrt")
		("location" . "Loc")
		("meaning" . "Mean")
		("object" . "Obj")
		("product" . "Prod")
		("representation" . "Rep")
		("role" . "Role")
		("value" . "Value")
		("path" . "Path")
		("deliverable" . "Dlv")
		("gap" . "Gap")
		("plateau" . "Plat")
		("workpackage" . "WP")
		("required" . "Req")
		("symmetric" . "Sym")
		("assessment" . "Assmt")
		("constraint" . "Con")
		("driver" . "Drv")
		("goal" . "Gaol")
		("outcome" . "Outc")
		("principle" . "Princ")
		("requirement" . "Rqmt")
		("stakeholder" . "Stkh")
		("distribution" . "Dist")
		("equipment" . "Eqmt")
		("facility" . "Fac")
		("material" . "Mat")
		("capability" . "Cbp")
		("course" . "Cour")
		("resource" . "Rsrc")
		("software" . "SW")
		("artifact" . "Arti")
		("Communication" . "Comm")
		("device" . "Dev")
		("infra" . "Infra")
		("network" . "NW")
		("node" . "Node")
		("system" . "Sys"))
	"Shortnames for Archimate elements")

(defvar plantuml--archimate-category-shortnames
	'(("application" . "app")
		("assessment" . "ass")
		("association" . "asso")
		("business" . "biz")
		("communication" . "comm")
		("constraint" . "con")
		("deliverable" . "dlv")
		("driver" . "drv")
		("gap" . "gap")
		("goal" . "goal")
		("implementation" . "impl")
		("interface" . "int")
		("motivation" . "mot")
		("physical" . "phy")
		("principle" . "pri")
		("requirement" . "req")
		("stakeholder" . "sth")
		("strategy" . "sty")
		("system" . "sys")
		("technology" . "tech")
		("workpackage" . "wp"))
	"Shortnames for the Archimate categories")

(defvar plantuml--archimate-sprites
	'("application-collaboration"
		"application-component"
		"application-data-object"
		"application-event"
		"application-function"
		"application-interaction"
		"application-interface"
		"application-process"
		"application-service"
		"assessment-filled"
		"business-activity"
		"business-actor"
		"business-collaboration"
		"business-contract"
		"business-event"
		"business-function"
		"business-interaction"
		"business-interface"
		"business-location"
		"business-meaning"
		"business-object"
		"business-process"
		"business-product"
		"business-representation"
		"business-role"
		"business-service"
		"business-value"
		"communication-path"
		"constraint-filled"
		"deliverable-filled"
		"driver-filled"
		"gap-filled"
		"goal-filled"
		"implementation-deliverable"
		"implementation-event"
		"implementation-gap"
		"implementation-plateau"
		"implementation-workpackage"
		"interface-required"
		"interface-symmetric"
		"motivation-assessment"
		"motivation-constraint"
		"motivation-driver"
		"motivation-goal"
		"motivation-meaning"
		"motivation-outcome"
		"motivation-principle"
		"motivation-requirement"
		"motivation-stakeholder"
		"motivation-value"
		"physical-distribution-network"
		"physical-equipment"
		"physical-facility"
		"physical-material"
		"principle-filled"
		"requirement-filled"
		"stakeholder-filled"
		"strategy-capability"
		"strategy-course-of-action"
		"strategy-resource"
		"strategy-value-stream"
		"system-software"
		"technology-artifact"
		"technology-collaboration"
		"technology-communication-network"
		"technology-communication-path"
		"technology-device"
		"technology-event"
		"technology-function"
		"technology-infra-interface"
		"technology-infra-service"
		"technology-interaction"
		"technology-interface"
		"technology-network"
		"technology-node"
		"technology-path"
		"technology-process"
		"technology-service"
		"technology-system-software"
		"workpackage-filled")
	"List of Archimate sprites")

(defun plantuml--archimate-sprite (selection)
	(let ((sprite-name (cl-destructuring-bind
													(category element) (split-string selection "-")
												(concat (cdr (assoc category  plantuml--archimate-category-shortnames))
																(cdr (assoc element  plantuml--archimate-element-shortnames))))))
		(format "sprite $%s jar:archimate/%s" sprite-name selection)))

(defun plantuml-insert-archimate-sprite ()
	(interactive)
	(when-let ((selection (completing-read
												 "Select archimate artifact: "
												 plantuml--archimate-sprites)))
		(insert (plantuml--archimate-sprite selection))))

(keymap-set plantuml-mode-map "C-c a" #'plantuml-insert-archimate-sprite)

(provide 'plantuml-archimate)
;;; plantuml-archimate.el ends here
