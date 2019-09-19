(import  :drewc/ftw/endpoint/struct 
         :drewc/ftw/endpoint/mux
         :std/format :std/test 
         :std/net/httpd)
(export define-endpoint add-endpoint! 
        current-http-mux current-http-request current-http-response)

(def current-http-mux
  (make-parameter (make-endpoint-http-mux)))

(def (add-endpoint! endpoint to: (to (current-http-mux)))
  (add-endpoint-to-http-mux! to endpoint))

(defrules define-endpoint ()
  ((_ name match)
   (add-endpoint! (make-endpoint 'name match)))
  ((_ name match args ... )
   (add-endpoint! (make-endpoint 'name match args ...)))
  ((_ name match mux: mux args ... )
   (add-endpoint! (make-endpoint 'name match args ...)
                  to: mux)))
