(import ;; :std/net/httpd/mux 
        :std/net/httpd ;; :std/misc/sync
        :std/pregexp :gerbil/expander
        :std/sugar :std/format :std/srfi/95 :std/iter :std/error
        :gerbil/gambit/exceptions)
(export #t)

(defstruct endpoint (name match priority
                          context parameters predicate function)
  constructor: :init!)

(defmethod {:init! endpoint}
  (lambda (self name match
           priority: (priority 10)
           predicate: (test endpoint-request-function-parameters)
           parameters: (parameters #t)
           function: (function endpoint-request-function) 
           context: (context (gx#current-expander-context)))
    (struct-instance-init! self name match priority
                           context parameters test function)))

(defstruct (endpoint-error <error>) (endpoint))

(def (raise-endpoint-error endpoint what . thingies)
  (raise (make-endpoint-error what thingies (endpoint-name endpoint) endpoint)))

(defstruct (endpoint-request-function-not-found endpoint-error) ())

(def (raise-endpoint-request-function-not-found endpoint what . thingies)
  (raise (make-endpoint-request-function-not-found what thingies (endpoint-name endpoint) endpoint)))

(def (endpoint-request-function endpoint request)
    (let* ((context (endpoint-context endpoint))
           (fname (string->symbol
                   (string-append
                    (symbol->string (endpoint-name endpoint))
                    "/"
                    (symbol->string (http-request-method request)))))
           (exception #f)
           (bound (try (##eval fname context) (catch (e) (set! exception e) #f)))
           (proc (if (and bound (procedure? bound))
                   bound
                   #f)))

      (cond (proc proc)
            (bound
             (raise-endpoint-request-function-not-found
              endpoint
              (format "~A is not a procedure. Context: ~A"
                      fname (expander-context-id context))
              fname))
            ((not bound)
             (raise-endpoint-request-function-not-found
              endpoint
              (format "~A is not bound in context: ~A. ~A"
                      fname (expander-context-id context)
                      (if exception (with-output-to-string (cut display-exception
                                                             exception))
                          ""))
              fname)))))

(def (endpoint-request-function-parameters endpoint request)
  (let ((groups (pregexp-match (endpoint-match endpoint)
                              (http-request-path request))))
    (if groups
      (cdr groups)
      #f)))

(def (make-endpoint-dispatch-function endpoint)
  (let* ((test (endpoint-predicate endpoint))
         (test (cut test endpoint <>))
         (params (endpoint-parameters endpoint))
         (param-fn (case params
                     ((#t) test)
                     ((#f) (lambda _ '()))
                     (else
                      (if (procedure? params)
                        (cut params endpoint <>)
                        (lambda _ params)))))
         (dispatch-to-function (cut (endpoint-function endpoint) endpoint <>)))
    (lambda (request)
      (let (results (test request))
        (if (not results)
          #f
          (begin0 #t
            (let (args (if (eq? #t params)
                         results
                         (param-fn request)))
              (apply (dispatch-to-function request) args))))))))
