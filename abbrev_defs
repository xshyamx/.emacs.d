;;-*-coding: utf-8;-*-
(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("acg" "atomic-change-group" nil :count 3)
    ("ahk" "add-hook" nil :count 10)
    ("atl" "add-to-list" nil :count 10)
    ("bbf" "bury-buffer" nil :count 0)
    ("bfn" "buffer-file-name" nil :count 29)
    ("blp" "buffer-live-p" nil :count 2)
    ("blv" "buffer-local-value" nil :count 4)
    ("bn" "buffer-name" nil :count 11)
    ("bnp" "boundp '" nil :count 2)
    ("bsn" "buffer-substring-no-properties" nil :count 18)
    ("bss" "buffer-substring" nil :count 0)
    ("cbf" "current-buffer" nil :count 12)
    ("cldb" "cl-destructuring-bind" nil :count 1)
    ("df" "defun" nil :count 156)
    ("dfa" "defalias" nil :count 13)
    ("dfc" "defconst" nil :count 8)
    ("dfk" "define-key" nil :count 9)
    ("dfm" "defmacro" nil :count 0)
    ("dfs" "define-skeleton" nil :count 3)
    ("dfu" "defcustom" nil :count 7)
    ("dfv" "defvar" nil :count 27)
    ("ef" "executable-find" nil :count 9)
    ("efn" "expand-file-name" nil :count 70)
    ("exf" "executable-find" nil :count 1)
    ("fep" "file-exists-p" nil :count 19)
    ("ff" "find-file" nil :count 0)
    ("ffn" "find-file-noselect" nil :count 2)
    ("fmt" "format \"\"" nil :count 5)
    ("fnb" "file-name-base" nil :count 3)
    ("fnd" "file-name-directory" nil :count 11)
    ("fne" "file-name-extension" nil :count 4)
    ("fnnd" "file-name-nondirectory" nil :count 8)
    ("fns" "file-name-split" nil :count 1)
    ("fnse" "file-name-sans-extension" nil :count 7)
    ("frn" "file-relative-name" nil :count 8)
    ("frp" "file-readable-p " nil :count 0)
    ("fts" "format-time-string" nil :count 10)
    ("fu" "funcall" nil :count 0)
    ("gbc" "get-buffer-create" nil :count 18)
    ("gc" "goto-char" nil :count 77)
    ("genv" "getenv" nil :count 5)
    ("gsk" "global-set-key " nil :count 0)
    ("ifc" "insert-file-cotnents" nil :count 7)
    ("ii" "interactive" nil :count 58)
    ("kyg" "keymap-global-set" nil :count 7)
    ("kyl" "keymap-local-set" nil :count 3)
    ("kys" "keymap-set" nil :count 19)
    ("kyu" "keymap-unset" nil :count 5)
    ("lbp" "line-beginning-position" nil :count 12)
    ("len" "length" nil :count 8)
    ("lep" "line-end-position" nil :count 7)
    ("luef" "locate-user-emacs-file" nil :count 5)
    ("mbg" "match-beginning" nil :count 12)
    ("mcr" "mapcar" nil :count 17)
    ("mct" "mapconcat" nil :count 1)
    ("med" "match-end" nil :count 1)
    ("msg" "message \"%s\"" nil :count 13)
    ("msn" "match-string-no-properties" nil :count 18)
    ("msnp" "match-string-no-properties" nil :count 17)
    ("mst" "match-string 0" nil :count 8)
    ("nlvf" "no-littering-expand-var-file-name" nil :count 4)
    ("nums" "number-sequence" nil :count 4)
    ("ocp" "org-element-context (org-element-at-point)" nil :count 3)
    ("oeap" "org-element-at-point" nil :count 6)
    ("oep" "org-element-property" nil :count 16)
    ("oet" "org-element-type" nil :count 3)
    ("plg" "plist-get" nil :count 35)
    ("plp" "plist-put" nil :count 14)
    ("pm" "point-min" nil :count 29)
    ("prg" "process-get" nil :count 4)
    ("prp" "process-put" nil :count 4)
    ("px" "point-max" nil :count 23)
    ("pz" "propertize " nil :count 0)
    ("req" "require '" nil :count 3)
    ("ro" "regexp-opt " nil :count 0)
    ("rq" "regexp-quote " nil :count 0)
    ("rris" "replace-regexp-in-string" nil :count 4)
    ("rrs" "replace-regexp-in-string" nil :count 2)
    ("rsb" "re-search-backward" nil :count 2)
    ("rsf" "re-search-forward" nil :count 42)
    ("rxs" "rx-to-string" nil :count 1)
    ("se" "save-excursion" nil :count 31)
    ("sgb" "seq-group-by" nil :count 3)
    ("smd" "save-match-data" nil :count 2)
    ("spp" "string-prefix-p" nil :count 16)
    ("spst" "split-string" nil :count 2)
    ("sq" "setq" nil :count 151)
    ("sqa" "shell-quote-argument" nil :count 4)
    ("sqf" "seq-filter" nil :count 13)
    ("sr" "save-restriction" nil :count 9)
    ("ssn" "substring-no-properties" nil :count 0)
    ("ssnp" "substring-no-properties" nil :count 5)
    ("ssp" "string-suffix-p" nil :count 2)
    ("stb" "switch-to-buffer" nil :count 1)
    ("stj" "string-join" nil :count 2)
    ("stm" "string-match" nil :count 2)
    ("stn" "string-to-number" nil :count 9)
    ("stp" "stringp" nil :count 0)
    ("syn" "symbol-name" nil :count 2)
    ("syp" "symbolp" nil :count 0)
    ("syv" "symbol-value" nil :count 1)
    ("tap" "thing-at-point" nil :count 4)
    ("ttb" "(with-current-buffer \"*temp*\"
	)" nil :count 1)
    ("ue" "user-error" nil :count 0)
    ("wcb" "with-current-buffer" nil :count 55)
    ("weal" "with-eval-after-load" nil :count 4)
    ("wtb" "with-temp-buffer" nil :count 33)
    ("wtf" "with-temp-file" nil :count 0)
    ("wtt" "with-current-buffer (get-buffer-create \"*temp*\")" nil :count 2)
   ))

(define-abbrev-table 'go-mode-abbrev-table
  '(
    ("erf" "Errorf" nil :count 3)
    ("fut" "func(t *testing.T) {}" nil :count 1)
    ("htf" "func(w http.ResponseWriter, r *http.Request) {}" nil :count 1)
    ("iferr" "if err != nil {
  log.Fatal(err)
}" nil :count 7)
   ))

(define-abbrev-table 'graphviz-dot-mode-abbrev-table
  '(
    ("sg" "subgraph cluster_" nil :count 2)
   ))

(define-abbrev-table 'org-mode-abbrev-table
  '(
    ("cap" "#+caption:" nil :count 21)
    ("ds" "#+html: <details><summary></summary>
#+html: </details>" nil :count 10)
    ("inc" "#+include:" nil :count 3)
    ("st" "#+startup:" nil :count 1)
    ("tit" "#+title:" nil :count 2)
    ("tle" "#+title:" nil :count 2)
   ))

(define-abbrev-table 'plantuml-mode-abbrev-table
  '(
    ("archi" "!include <archimate/Archimate>" nil :count 1)
    ("dtc" "skinparam defaultTextAlignment center" nil :count 11)
    ("lbt" "skinparam legendBorderThickness 0" nil :count 4)
    ("lgt" "skinparam legendBackgroundColor transparent" nil :count 4)
    ("lto" "skinparam lineType ortho" nil :count 6)
    ("ltr" "left to right direction" nil :count 4)
    ("mms" "skinparam maxMessageSize 120" nil :count 1)
    ("skp" "skinparam" nil :count 1)
   ))

(define-abbrev-table 'texinfo-mode-abbrev-table
  '(
    ("bul" "@bullet" nil :count 12)
    ("inc" "@include" nil :count 4)
    ("sec" "@section" nil :count 50)
    ("sh" "@subheading" nil :count 28)
   ))

(define-abbrev-table 'visual-basic-mode-abbrev-table
  '(
    ("add" "Add" nil :count 0)
    ("aggregate" "Aggregate" nil :count 0)
    ("and" "And" nil :count 0)
    ("app" "App" nil :count 0)
    ("appactivate" "AppActivate" nil :count 0)
    ("application" "Application" nil :count 0)
    ("array" "Array" nil :count 0)
    ("as" "As" nil :count 0)
    ("asc" "Asc" nil :count 0)
    ("ascb" "AscB" nil :count 0)
    ("atn" "Atn" nil :count 0)
    ("attribute" "Attribute" nil :count 0)
    ("beep" "Beep" nil :count 0)
    ("begin" "Begin" nil :count 0)
    ("begintrans" "BeginTrans" nil :count 0)
    ("boolean" "Boolean" nil :count 0)
    ("byref" "ByRef" nil :count 0)
    ("byval" "ByVal" nil :count 0)
    ("call" "Call" nil :count 0)
    ("case" "Case" nil :count 0)
    ("cbool" "CBool" nil :count 0)
    ("cbyte" "CByte" nil :count 0)
    ("ccur" "CCur" nil :count 0)
    ("cdate" "CDate" nil :count 0)
    ("cdbl" "CDbl" nil :count 0)
    ("character" "Character" nil :count 0)
    ("chdir" "ChDir" nil :count 0)
    ("chdrive" "ChDrive" nil :count 0)
    ("choose" "Choose" nil :count 0)
    ("chr" "Chr" nil :count 0)
    ("chrb" "ChrB" nil :count 0)
    ("cint" "CInt" nil :count 0)
    ("class" "Class" nil :count 0)
    ("classmodule" "ClassModule" nil :count 0)
    ("clipboard" "Clipboard" nil :count 0)
    ("clng" "CLng" nil :count 0)
    ("close" "Close" nil :count 0)
    ("collection" "Collection" nil :count 0)
    ("column" "Column" nil :count 0)
    ("columns" "Columns" nil :count 0)
    ("command" "Command" nil :count 0)
    ("committrans" "CommitTrans" nil :count 0)
    ("compactdatabase" "CompactDatabase" nil :count 0)
    ("component" "Component" nil :count 0)
    ("components" "Components" nil :count 0)
    ("const" "Const" nil :count 0)
    ("container" "Container" nil :count 0)
    ("containers" "Containers" nil :count 0)
    ("cos" "Cos" nil :count 0)
    ("createdatabase" "CreateDatabase" nil :count 0)
    ("createobject" "CreateObject" nil :count 0)
    ("csng" "CSng" nil :count 0)
    ("cstr" "CStr" nil :count 0)
    ("curdir" "CurDir" nil :count 0)
    ("currency" "Currency" nil :count 0)
    ("cvar" "CVar" nil :count 0)
    ("cverr" "CVErr" nil :count 0)
    ("data" "Data" nil :count 0)
    ("database" "Database" nil :count 0)
    ("databases" "Databases" nil :count 0)
    ("date" "Date" nil :count 0)
    ("dateadd" "DateAdd" nil :count 0)
    ("datediff" "DateDiff" nil :count 0)
    ("datepart" "DatePart" nil :count 0)
    ("dateserial" "DateSerial" nil :count 0)
    ("datevalue" "DateValue" nil :count 0)
    ("day" "Day" nil :count 0)
    ("dbengine" "DBEngine" nil :count 0)
    ("ddb" "DDB" nil :count 0)
    ("debug" "Debug" nil :count 0)
    ("declare" "Declare" nil :count 0)
    ("deftype" "Deftype" nil :count 0)
    ("deletesetting" "DeleteSetting" nil :count 0)
    ("dim" "Dim" nil :count 0)
    ("dir" "Dir" nil :count 0)
    ("do" "Do" nil :count 0)
    ("doevents" "DoEvents" nil :count 0)
    ("domain" "Domain" nil :count 0)
    ("double" "Double" nil :count 0)
    ("dynaset" "Dynaset" nil :count 0)
    ("each" "Each" nil :count 0)
    ("else" "Else" nil :count 0)
    ("empty" "Empty" nil :count 0)
    ("end" "End" nil :count 0)
    ("endproperty" "EndProperty" nil :count 0)
    ("enum" "Enum" nil :count 0)
    ("environ" "Environ" nil :count 0)
    ("eof" "EOF" nil :count 0)
    ("erase" "Erase" nil :count 0)
    ("err" "Err" nil :count 0)
    ("error" "Error" nil :count 0)
    ("exit" "Exit" nil :count 0)
    ("exp" "Exp" nil :count 0)
    ("explicit" "Explicit" nil :count 0)
    ("false" "False" nil :count 0)
    ("field" "Field" nil :count 0)
    ("fields" "Fields" nil :count 0)
    ("fileattr" "FileAttr" nil :count 0)
    ("filecopy" "FileCopy" nil :count 0)
    ("filedatetime" "FileDateTime" nil :count 0)
    ("filelen" "FileLen" nil :count 0)
    ("fix" "Fix" nil :count 0)
    ("font" "Font" nil :count 0)
    ("for" "For" nil :count 0)
    ("form" "Form" nil :count 0)
    ("format" "Format" nil :count 0)
    ("formatcurrency" "FormatCurrency" nil :count 0)
    ("formatdatetime" "FormatDateTime" nil :count 0)
    ("formatnumber" "FormatNumber" nil :count 0)
    ("formatpercent" "FormatPercent" nil :count 0)
    ("forms" "Forms" nil :count 0)
    ("formtemplate" "FormTemplate" nil :count 0)
    ("freefile" "FreeFile" nil :count 0)
    ("freelocks" "FreeLocks" nil :count 0)
    ("friend" "Friend" nil :count 0)
    ("function" "Function" nil :count 0)
    ("fv" "FV" nil :count 0)
    ("get" "Get" nil :count 0)
    ("getallsettings" "GetAllSettings" nil :count 0)
    ("getattr" "GetAttr" nil :count 0)
    ("getobject" "GetObject" nil :count 0)
    ("getsetting" "GetSetting" nil :count 0)
    ("global" "Global" nil :count 0)
    ("gosub" "GoSub" nil :count 0)
    ("goto" "GoTo" nil :count 0)
    ("group" "Group" nil :count 0)
    ("groups" "Groups" nil :count 0)
    ("hex" "Hex" nil :count 0)
    ("hour" "Hour" nil :count 0)
    ("if" "If" nil :count 0)
    ("iif" "IIf" nil :count 0)
    ("imestatus" "IMEStatus" nil :count 0)
    ("implements" "Implements" nil :count 0)
    ("input" "Input" nil :count 0)
    ("instr" "InStr" nil :count 0)
    ("int" "Int" nil :count 0)
    ("integer" "Integer" nil :count 0)
    ("ipmt" "IPmt" nil :count 0)
    ("irr" "IRR" nil :count 0)
    ("is" "Is" nil :count 0)
    ("isarray" "IsArray" nil :count 0)
    ("isdate" "IsDate" nil :count 0)
    ("isempty" "IsEmpty" nil :count 0)
    ("iserror" "IsError" nil :count 0)
    ("ismissing" "IsMissing" nil :count 0)
    ("isnull" "IsNull" nil :count 0)
    ("isnumeric" "IsNumeric" nil :count 0)
    ("isobject" "IsObject" nil :count 0)
    ("kill" "Kill" nil :count 0)
    ("lbound" "LBound" nil :count 0)
    ("lcase" "LCase" nil :count 0)
    ("left" "Left" nil :count 0)
    ("len" "Len" nil :count 0)
    ("let" "Let" nil :count 0)
    ("like" "Like" nil :count 0)
    ("line" "Line" nil :count 0)
    ("load" "Load" nil :count 0)
    ("loadpicture" "LoadPicture" nil :count 0)
    ("loadresdata" "LoadResData" nil :count 0)
    ("loadrespicture" "LoadResPicture" nil :count 0)
    ("loadresstring" "LoadResString" nil :count 0)
    ("loc" "Loc" nil :count 0)
    ("lock" "Lock" nil :count 0)
    ("lof" "LOF" nil :count 0)
    ("log" "Log" nil :count 0)
    ("long" "Long" nil :count 0)
    ("loop" "Loop" nil :count 0)
    ("lset" "LSet" nil :count 0)
    ("ltrim" "LTrim" nil :count 0)
    ("mdiform" "MDIForm" nil :count 0)
    ("me" "Me" nil :count 0)
    ("menuitems" "MenuItems" nil :count 0)
    ("menuline" "MenuLine" nil :count 0)
    ("mid" "Mid" nil :count 0)
    ("minute" "Minute" nil :count 0)
    ("mirr" "MIRR" nil :count 0)
    ("mkdir" "MkDir" nil :count 0)
    ("month" "Month" nil :count 0)
    ("msgbox" "MsgBox" nil :count 0)
    ("name" "Name" nil :count 0)
    ("new" "New" nil :count 0)
    ("next" "Next" nil :count 0)
    ("not" "Not" nil :count 0)
    ("nothing" "Nothing" nil :count 0)
    ("now" "Now" nil :count 0)
    ("nper" "NPer" nil :count 0)
    ("npv" "NPV" nil :count 0)
    ("null" "Null" nil :count 0)
    ("object" "Object" nil :count 0)
    ("oct" "Oct" nil :count 0)
    ("on" "On" nil :count 0)
    ("open" "Open" nil :count 0)
    ("opendatabase" "OpenDatabase" nil :count 0)
    ("operator" "Operator" nil :count 0)
    ("option" "Option" nil :count 0)
    ("optional" "Optional" nil :count 0)
    ("or" "Or" nil :count 0)
    ("parameter" "Parameter" nil :count 0)
    ("parameters" "Parameters" nil :count 0)
    ("partition" "Partition" nil :count 0)
    ("picture" "Picture" nil :count 0)
    ("pmt" "Pmt" nil :count 0)
    ("ppmt" "PPmt" nil :count 0)
    ("preserve" "Preserve" nil :count 0)
    ("print" "Print" nil :count 0)
    ("printer" "Printer" nil :count 0)
    ("printers" "Printers" nil :count 0)
    ("private" "Private" nil :count 0)
    ("projecttemplate" "ProjectTemplate" nil :count 0)
    ("properties" "Properties" nil :count 0)
    ("property" "Property" nil :count 0)
    ("public" "Public" nil :count 0)
    ("put" "Put" nil :count 0)
    ("pv" "PV" nil :count 0)
    ("qbcolor" "QBColor" nil :count 0)
    ("querydef" "QueryDef" nil :count 0)
    ("querydefs" "QueryDefs" nil :count 0)
    ("randomize" "Randomize" nil :count 0)
    ("rate" "Rate" nil :count 0)
    ("recordset" "Recordset" nil :count 0)
    ("recordsets" "Recordsets" nil :count 0)
    ("redim" "ReDim" nil :count 0)
    ("registerdatabase" "RegisterDatabase" nil :count 0)
    ("relation" "Relation" nil :count 0)
    ("relations" "Relations" nil :count 0)
    ("rem" "Rem" nil :count 0)
    ("repairdatabase" "RepairDatabase" nil :count 0)
    ("reset" "Reset" nil :count 0)
    ("resume" "Resume" nil :count 0)
    ("return" "Return" nil :count 0)
    ("right" "Right" nil :count 0)
    ("rmdir" "RmDir" nil :count 0)
    ("rnd" "Rnd" nil :count 0)
    ("rollback" "Rollback" nil :count 0)
    ("rowbuffer" "RowBuffer" nil :count 0)
    ("rset" "RSet" nil :count 0)
    ("rtrim" "RTrim" nil :count 0)
    ("savepicture" "SavePicture" nil :count 0)
    ("savesetting" "SaveSetting" nil :count 0)
    ("screen" "Screen" nil :count 0)
    ("second" "Second" nil :count 0)
    ("seek" "Seek" nil :count 0)
    ("selbookmarks" "SelBookmarks" nil :count 0)
    ("select" "Select" nil :count 0)
    ("selectedcomponents" "SelectedComponents" nil :count 0)
    ("sendkeys" "SendKeys" nil :count 0)
    ("set" "Set" nil :count 0)
    ("setattr" "SetAttr" nil :count 0)
    ("setdataaccessoption" "SetDataAccessOption" nil :count 0)
    ("setdefaultworkspace" "SetDefaultWorkspace" nil :count 0)
    ("sgn" "Sgn" nil :count 0)
    ("shell" "Shell" nil :count 0)
    ("sin" "Sin" nil :count 0)
    ("single" "Single" nil :count 0)
    ("sln" "SLN" nil :count 0)
    ("snapshot" "Snapshot" nil :count 0)
    ("space" "Space" nil :count 0)
    ("spc" "Spc" nil :count 0)
    ("sqr" "Sqr" nil :count 0)
    ("static" "Static" nil :count 0)
    ("step" "Step" nil :count 0)
    ("stop" "Stop" nil :count 0)
    ("str" "Str" nil :count 0)
    ("strcomp" "StrComp" nil :count 0)
    ("strconv" "StrConv" nil :count 0)
    ("string" "String" nil :count 0)
    ("sub" "Sub" nil :count 0)
    ("submenu" "SubMenu" nil :count 0)
    ("switch" "Switch" nil :count 0)
    ("syd" "SYD" nil :count 0)
    ("tab" "Tab" nil :count 0)
    ("table" "Table" nil :count 0)
    ("tabledef" "TableDef" nil :count 0)
    ("tabledefs" "TableDefs" nil :count 0)
    ("tan" "Tan" nil :count 0)
    ("then" "Then" nil :count 0)
    ("time" "Time" nil :count 0)
    ("timer" "Timer" nil :count 0)
    ("timeserial" "TimeSerial" nil :count 0)
    ("timevalue" "TimeValue" nil :count 0)
    ("to" "To" nil :count 0)
    ("trim" "Trim" nil :count 0)
    ("true" "True" nil :count 0)
    ("type" "Type" nil :count 0)
    ("typename" "TypeName" nil :count 0)
    ("ubound" "UBound" nil :count 0)
    ("ucase" "UCase" nil :count 0)
    ("unload" "Unload" nil :count 0)
    ("unlock" "Unlock" nil :count 0)
    ("val" "Val" nil :count 0)
    ("variant" "Variant" nil :count 0)
    ("vartype" "VarType" nil :count 0)
    ("verb" "Verb" nil :count 0)
    ("weekday" "Weekday" nil :count 0)
    ("wend" "Wend" nil :count 0)
    ("while" "While" nil :count 0)
    ("width" "Width" nil :count 0)
    ("with" "With" nil :count 0)
    ("workspace" "Workspace" nil :count 0)
    ("workspaces" "Workspaces" nil :count 0)
    ("write" "Write" nil :count 0)
    ("year" "Year" nil :count 0)
   ))

