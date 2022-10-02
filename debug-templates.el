;;template django test
;;
(dap-register-debug-template "debug app"
 (list :type "python"
       :args "runserver --noreload"
       :cwd "/Users/dmitrijmartys/PYTHON/test/core"
       :module nil
       :console "integratedTerminal"
       :program "/Users/dmitrijmartys/PYTHON/test/core/manage.py"
       :request "launch"
       :name "Python: Django"
       :django t))

;;=======================================================
;;;;template django zakupki
;;;;
(dap-register-debug-template "debug zakupki"
 (list :type "python"
       :args "runserver --noreload"
       :cwd "/Users/dmitrijmartys/SRC/zakupki/zakupki/src/zakupki"
       :module nil
       :console "integratedTerminal"
       :program "/Users/dmitrijmartys/SRC/zakupki/zakupki/src/zakupki/manage.py"
       :request "launch"
       :name "Python: Django"
       :django t))
