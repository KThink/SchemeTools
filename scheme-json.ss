(library (scheme-json)
  (export json->scheme)
  (import (chezscheme))

  (define get-exp-digits
    (lambda (p)
      (let ([c (lookahead-char p)]
	    [msg-n "The exp part of the number is missing number"]
	    [msg-e "The exp part of the number have an invalid number"])
	(cond [(eof-object? c) (error "get-exp-digits" msg-n)]
	      [(char-numeric? c)
	       (let f ()
		 (let ([c (lookahead-char p)])
		   (cond [(eof-object? c) '()]
			 [(char=? c #\,) '()]
			 [(char=? c #\]) '()]
			 [(char=? c #\}) '()]
			 [(char=? c #\x0009) (get-char p) (f)]
			 [(char=? c #\x000A) (get-char p) (f)]
			 [(char=? c #\x000D) (get-char p) (f)]
			 [(char=? c #\x0020) (get-char p) (f)]
			 [(char-numeric? c) (get-char p) (cons c (f))]
			 [else (error "get-exp-digits" msg-e c)])))]
	      [else (error "get-exp-digits" msg-n)]))))

  (define get-exp
    (lambda (p)
      (let* ([e-part (get-char p)]
	     [s-part (get-char p)]
	     [msg-e-sign "The exp part of the number is missing \"E,e,+,-\""])
	(if (and (and (not (eof-object? e-part)) (not (eof-object? s-part)))
		 (or (char=? e-part #\E) (char=? e-part #\e))
		 (or (char=? s-part #\+) (char=? s-part #\-)))
	    (cons e-part (cons s-part (get-exp-digits p)))
	    (error "get-exp" msg-e-sign)))))

  (define get-frac
    (lambda (p)
      (let f ([res '()])
	(let ([c (lookahead-char p)])
	  (cond [(eof-object? c) res]
		[(char=? c #\,) res]
		[(char=? c #\]) res]
		[(char=? c #\}) res]
		[(char=? c #\x0009) (get-char p) (f res)]
		[(char=? c #\x000A) (get-char p) (f res)]
		[(char=? c #\x000D) (get-char p) (f res)]
		[(char=? c #\x0020) (get-char p) (f res)]
		[(char-numeric? c) (get-char p) (f (cons c res))]
		[(or (char=? c #\e) (char=? c #\E))
		 (if (null? res)
		     (error "get-frac" "The fractional part of the number must include digit")
		     (append res (get-exp p)))]
		[else
		 (if (null? res)
		     (error "get-frac" "The fractional part of the number must include digit")
		     (error "get-frac" "The fractional part of the number include invalid digit" c))])))))

  (define get-digits
    (lambda (p)
      (let f ()
	(let ([c (lookahead-char p)])
	  (cond [(eof-object? c) '()]
		[(char=? c #\,) '()]
		[(char=? c #\]) '()]
		[(char=? c #\}) '()]
		[(char=? c #\x0009) (get-char p) (f)]
		[(char=? c #\x000A) (get-char p) (f)]
		[(char=? c #\x000D) (get-char p) (f)]
		[(char=? c #\x0020) (get-char p) (f)]
		[(char=? c #\.) (get-char p) (cons #\. (get-frac p))]
		[(char-numeric? c) (get-char p) (cons c (f))]
		[(or (char=? c #\e) (char=? c #\E)) (get-exp p)]
		[else (error "get-digits" "The integer part of the number include an invalid char" c)])))))

  (define get-int
    (lambda (p)
      (let ([c (lookahead-char p)]
	    [msg-zero "Behind the starting number zero can not follow any digit"])
	(cond [(char=? c #\0)
	       (get-char p)
	       (let f ()
		 (let ([c (lookahead-char p)])
		   (cond [(eof-object? c) '(#\0)]
			 [(char=? c #\,) '(#\0)]
			 [(char=? c #\]) '(#\0)]
			 [(char=? c #\}) '(#\0)]
			 [(char=? c #\x0009) (get-char p) (f)]
			 [(char=? c #\x000A) (get-char p) (f)]
			 [(char=? c #\x000D) (get-char p) (f)]
			 [(char=? c #\x0020) (get-char p) (f)]
			 [(char=? c #\.) (get-char p) (cons #\0 (cons #\. (get-frac p)))]
			 [(or (char=? c #\e) (char=? c #\E)) (cons#\0 (get-exp p))]
			 [else (error "get-int" msg-zero c)])))]
	      [(or (char>=? c #\1) (char<=? c #\9)) (get-digits p)]
	      [else (error "get-int" "Unknown error" c)]))))

  (define json-get-number
    (lambda (p)
      (let ([c (lookahead-char p)]
	    [msg-sign "The sign part of the number must be followed by digits"]
	    [msg-null "Can not get any number"]
	    [msg-error "Unknown error"])
	(string->number
	  (list->string
	    (cond [(eof-object? c) (error "json-get-number" msg-null)]
		  [(char=? c #\-)
		   (get-char p)
		   (let ([c (lookahead-char p)])
		     (if (char-numeric? c)
			 (cons #\- (get-int p))
			 (error "json-get-number" msg-sign)))]
		  [(char-numeric? c) (get-int p)]
		  [else (error "json-get-number" msg-error)]))))))

  (define get-word
    (lambda (p)
      (list->string
	(let f ()
	  (let ([c (lookahead-char p)])
	    (cond
	      [(eof-object? c) '()]
	      [(char-alphabetic? c) (cons (get-char p) (f))]
	      [else '()]))))))

  (define json-get-ntf
    (lambda (p expect-str return)
      (let ([str (get-word p)]
	    [msg (string-append "Json invalid for " expect-str)])
	(if (not (string=? expect-str str))
	    (error "json-get-ntf" msg str)
	    return))))

  (define get-escape-char
    (lambda (p)
      (let ([c (get-char p)])
	(cond [(char=? c #\b) #\x0008]
	      [(char=? c #\f) #\x000C]
	      [(char=? c #\n) #\x000A]
	      [(char=? c #\r) #\x000D]
	      [(char=? c #\t) #\x0009]
	      [(char=? c #\") #\"]
	      [(char=? c #\\) #\\]
	      [(char=? c #\/) #\/]
	      [(char=? c #\u)
	       (let* ([str (get-string-n p 4)]
		      [num (string->number str 16)])
		 (if (not (eq? #f num))
		     (integer->char num)
		     (error "get-escape-char" "Invalid hex number" str)))]
	      [else (error "get-escape-char" "Invalid escape char" c)]))))

  (define json-get-string
    (lambda (p)
      (get-char p)
      (list->string
	(let f ()
	  (let ([c (get-char p)])
	    (cond [(eof-object? c)
		   (error
		     "json-get-string"
		     "String ends with EOF instead of \"")]
		  [(char<? c #\x0020)
		   (error
		     "json-get-string"
		     "String can not include control character")]
		  [(char=? c #\\) (cons (get-escape-char p) (f))]
		  [(char=? c #\") '()]
		  [else (cons c (f))]))))))

  (define set-pair
    (lambda (p ht)
      (let ([name (json-get-string p)])
	(let f ()
	  (let ([c (lookahead-char p)])
	    (cond [(eof-object? c)
		   (error "set-pair" "Object pair error" name)]
		  [(char=? c #\x0009) (get-char p) (f)]
		  [(char=? c #\x000A) (get-char p) (f)]
		  [(char=? c #\x000D) (get-char p) (f)]
		  [(char=? c #\x0020) (get-char p) (f)]
		  [(char=? c #\:) (get-char p) (f)]
		  [else (let ([value (json-parser p)])
			  (hashtable-set! ht name value))]))))))

  (define json-get-object
    (lambda (p)
      (get-char p)
      (let ([ht (make-hashtable string-hash string=?)])
	(let f ()
	  (let ([c (lookahead-char p)])
	    (cond [(eof-object? c)
		   (error "json-get-object" "Object ends with EOF instead of \"}\"")]
		  [(char=? c #\x0009) (get-char p) (f)]
		  [(char=? c #\x000A) (get-char p) (f)]
		  [(char=? c #\x000D) (get-char p) (f)]
		  [(char=? c #\x0020) (get-char p) (f)]
		  [(char=? c #\,) (get-char p) (f)]
		  [(char=? c #\}) (get-char p) ht]
		  [else (set-pair p ht) (f)]))))))

  (define json-get-array
    (lambda (p)
      (get-char p)
      (list->vector
	(let f ()
	  (let ([c (lookahead-char p)])
	    (cond [(eof-object? c)
		   (error "json-get-array" "Array ends with EOF instead of \"]\"")]
		  [(char=? c #\x0009) (get-char p) (f)]
		  [(char=? c #\x000A) (get-char p) (f)]
		  [(char=? c #\x000D) (get-char p) (f)]
		  [(char=? c #\x0020) (get-char p) (f)]
		  [(char=? c #\,) (get-char p) (f)]
		  [(char=? c #\]) (get-char p) '()]
		  [else (let ([value (json-parser p)])
			  (cons value (f)))]))))))

  (define json-parser
    (lambda (p)
      (let f ()
	(let ([c (lookahead-char p)])
	  (cond [(eof-object? c) (error "json-parser" "Expecting value")]
		[(char=? c #\x0009) (get-char p) (f)]
		[(char=? c #\x000A) (get-char p) (f)]
		[(char=? c #\x000D) (get-char p) (f)]
		[(char=? c #\x0020) (get-char p) (f)]
		[(char=? c #\[) (json-get-array p)]
		[(char=? c #\{) (json-get-object p)]
		[(char=? c #\") (json-get-string p)]
		[(char=? c #\t) (json-get-ntf p "true" #t)]
		[(char=? c #\n) (json-get-ntf p "null" '())]
		[(char=? c #\f) (json-get-ntf p "false" #f)]
		[(char=? c #\-) (json-get-number p)]
		[(char-numeric? c) (json-get-number p)]
		[else (error "json-parser" "Json invalid")])))))

  (define json->scheme
    (case-lambda
      [(p) (let ((msg "The parameter can be a string, a port or empty"))
	     (cond [(port? p) (json-parser p)]
		   [(string? p) (json-parser (open-string-input-port p))]
		   [else (error "json->scheme" msg)]))]
      [() (json-parser (current-input-port))])))
