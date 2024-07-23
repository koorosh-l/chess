(define-module (utils)
  #:export (lnr isit?))

(define-syntax-rule (lnr expr ...)
  (let ([e (begin expr ...)]
	[mod-name (module-name (current-module))]
	[loc (cdar (current-source-location))])
    (format (current-output-port) "----- ~a : ~a --> ~a\n" mod-name loc e)
    e))
(define-syntax isit?
  (syntax-rules ()
    [(_ msg pred obj)
     (if (not (pred obj))
	 (error obj msg 'pred))]
    [(_ msg pred obj obj* ...)
     (begin
       (isit? msg pred obj)
       (isit? msg pred obj* ...))]))
