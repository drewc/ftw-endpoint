(import :drewc/ftw/endpoint/struct 
        :std/net/httpd/mux :std/net/httpd :std/misc/sync
        :std/sugar :std/srfi/95 :std/iter :std/format
        :gerbil/gambit/exceptions :gerbil/expander)
(export #t)

(defstruct endpoint-http-mux (endpoints queue)
  constructor: :init!)

(defmethod {:init! endpoint-http-mux}
  (lambda (self)
    (struct-instance-init! self (make-sync-hash (make-hash-table)) '())))

(def (endpoint-hash endpoint)
  (string->symbol (string-append
                   (symbol->string
                    (expander-context-id (endpoint-context endpoint)))
                   "::"
                   (with-output-to-string
                     (cut display (endpoint-name endpoint))))))

(def (make-endpoint-http-mux-queue mux)
  (let (q (sync-hash-do (endpoint-http-mux-endpoints mux)
                        (lambda (t) (for/collect ((values k v) (in-hash t)) v))))
    (map make-endpoint-dispatch-function (sort q < endpoint-priority))))

(def (add-endpoint-to-http-mux! mux endpoint)
  (let (hash (endpoint-hash endpoint))
    (begin0 hash (sync-hash-put! (endpoint-http-mux-endpoints mux)
                  hash 
                  endpoint)
            (set! (endpoint-http-mux-queue mux) (make-endpoint-http-mux-queue mux)))))

(def current-http-request
  (make-parameter #f))

(def current-http-response
  (make-parameter #f))

(def (endpoint-http-mux-request-handler mux)
  (lambda (req res)
    (parameterize ((current-http-request req)
                   (current-http-response res))
      (try 
       (let handle-request ((q (endpoint-http-mux-queue mux)))
         (if (null? q)
           (error "Cannot find handler for " (http-request-path req))
           (let (dispatched? ((car q) req))
             (or dispatched? (handle-request (cdr q))))))
      (catch (e)
        (http-response-write
         res 500 '() (format "Endpoint Error: ~A"
                         (with-output-to-string (cut display-exception e)))))))))

(defmethod {get-handler endpoint-http-mux}
  (lambda (mux . _) (endpoint-http-mux-request-handler mux)))

(defmethod {put-handler! endpoint-http-mux}
  (lambda (mux host path handler)
    (if (procedure? handler)
      (add-endpoint-to-http-mux!
       mux (make-endpoint (string->symbol path) path
                          function: (lambda ()
                                      (handler (current-http-request)
                                               (current-http-response)))
                          parameters: '()
                          predicate: (lambda (req) (eqv? path
                                                    (http-request-path (current-http-request))))))
      (add-endpoint-to-http-mux! mux handler))))
