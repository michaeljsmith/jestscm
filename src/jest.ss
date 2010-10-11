#! /usr/bin/env mzscheme

;--------------------------------------------------------------------------------
;
;  Tested under:
;  MzScheme v4.2.1
;
;
;  Copyright (c) 2010 Michael Smith <msmith@msmith.id.au>
;
;  http://github.com/michaeljsmith/jest
;
;  Permission is hereby granted, free of charge, to any person obtaining a copy
;  of this software and associated documentation files (the "Software"), to
;  deal in the Software without restriction, including without limitation the
;  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
;  sell copies of the Software, and to permit persons to whom the Software is
;  furnished to do so, subject to the following conditions:
;  
;  The above copyright notice and this permission notice shall be included in
;  all copies or substantial portions of the Software.
;  
;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;  IN THE SOFTWARE.
;
;--------------------------------------------------------------------------------

#lang scheme

(require scheme/list)
(require scheme/string)

(define (_read-file filename)
	(call-with-input-file
		filename
		(lambda (f)
			(let read-rest ()
				(let ((data (read f)))
					(if (eof-object? data)
						'()
						(cons data (read-rest))))))))

(define base-rules '())
(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))

(define (scheme-evaluate fm)
	(let ((quoted-list
					(if (list? fm)
						(cons
							(car fm)
							(let quote-list ((ls (cdr fm)))
								(cond
									((null? ls) null)
									((list? ls) (cons `(quote ,(car ls)) (quote-list (cdr ls))))
									(else ls))))
						fm)))
		;(printf "evaluate-builtin: ~a~n" fm)
		(eval quoted-list eval-ns)))

(define evaluate-builtin scheme-evaluate)

; Basic evaluation function, implemented in scheme. This function is the
; lowest-level method of evaluating a function, used for bootstrapping
; higher level self-hosting evaluations. Ideally this function will not
; actually be run during use, a higher level version will be self-compiled
; to a backend. This function takes a fallback scheme lambda to use
; to evaluate any undefined forms, which allows standard scheme expressions
; to be compiled.
; Note that this evaluation scheme is very simply implemented, and in particular
; scoping is dynamic. Lexical scoping is implemented at a higher level.
(define (evaluate-using-rules orig-rules orig-fm)

	(define (resolve fm fallback)
		(let recurse ((rules orig-rules))
			(define (match-const val fm)
				(cond
					((equal? val fm) '(#t ()))
					(else '(#f))))
			(define (match-var name fm)
				`(#t ((,name ,fm))))
			(define (match-fm ptn fm)
				(cond
					((null? ptn) (if (null? fm) '(#t ()) '(#f)))
					((pair? ptn)
					 (if (pair? fm)
						 (let* ((hd-rslt (match-ptn (car ptn) (car fm)))
										(hd-scs (car hd-rslt))
										(tl-rslt (if hd-scs (match-ptn (cdr ptn) (cdr fm)) '(#f)))
										(tl-scs (car tl-rslt))
										(scs (and hd-scs tl-scs)))
							 (if scs
								 (let ((hd-bdngs (cadr hd-rslt))
											 (tl-bdngs (cadr tl-rslt)))
									 (let merge ((to-mrg hd-bdngs) (bdngs tl-bdngs))
										 (if (null? to-mrg)
											 (list #t bdngs)
											 (let*
												 ((bdng (car to-mrg))
													(name (car bdng))
													(value (cadr bdng))
													(existing (assv name bdngs))
													(scs (or (not existing) (equal? value (cadr existing))))
													(new-bdngs (if scs (if (not existing) (cons bdng bdngs) bdngs) '())))
												 (if scs
													 (merge (cdr to-mrg) new-bdngs)
													 '(#f))))))
								 '(#f)))
						 '(#f)))
					(else
						(match-ptn ptn fm))))
			(define (match-ptn ptn fm)
				(let ((match-ptn-rslt
								(let ((ptn-tp (car ptn))
											(ptn-val (cadr ptn)))
									(cond
										((eqv? ptn-tp 'const) (match-const ptn-val fm))
										((eqv? ptn-tp 'var) (match-var ptn-val fm))
										((eqv? ptn-tp 'fm) (match-fm ptn-val fm))
										(else (car car))))))
					match-ptn-rslt))
			(define (bind-and-evaluate bdngs fm)
				(let ((new-rules
								(let bind ((bs bdngs))
									(if (null? bs)
										orig-rules
										(cons
											(let* ((bdng (car bs))
														 (name (car bdng))
														 (value (cadr bdng)))
												(list (list 'const name) (list 'quote value)))
											(bind (cdr bs)))))))
					(evaluate-using-rules new-rules fm)))
			(cond
				((null? rules)
				 (apply fallback (list fm)))
				(else
					(begin
						(let* ((rule (car rules))
									 (rule-ptn (car rule))
									 (rule-expr (cadr rule))
									 (match-rslt (match-ptn rule-ptn fm))
									 (match-scs (car match-rslt)))
							(if match-scs
								(bind-and-evaluate (cadr match-rslt) rule-expr)
								(recurse (cdr rules)))))))))

	(define (evaluate-using-rules-with-fallback fallback in-rules in-fm)
		(let
			((eval-rslt
				 (cond
					 ((not (list? in-fm)) (resolve in-fm fallback))
					 ((eqv? 'quote (car in-fm))
						(cadr in-fm))
					 ((eqv? 'evaluate (car in-fm))
						(let* ((rules (evaluate-using-rules-with-fallback scheme-evaluate in-rules (cadr in-fm)))
									 (fm (evaluate-using-rules-with-fallback scheme-evaluate in-rules (caddr in-fm))))
							;(printf "Dynamic evaluate (old): rules=~a fm=~a~n" rules fm)
							(evaluate-using-rules rules fm)))
					 (else
						 (begin
							 ;(printf "Standard evaluate: ~a ~a~n" in-fm in-rules)
							 (let
								 ((subfms (let eval-subfms ((subfms in-fm))
														(if (null? subfms)
															null
															(cons (evaluate-using-rules in-rules (car subfms))
																		(eval-subfms (cdr subfms)))))))
								 (resolve subfms fallback)))))))
			eval-rslt))

	(define (macro-failed exp-fm)
		(evaluate-using-rules-with-fallback scheme-evaluate orig-rules orig-fm))
	(let
		((eval-rslt
			 (begin
				 ;(printf "evaluating: fm=~a rules=~a~n" orig-fm orig-rules)
				 (cond
					 ((and (list? orig-fm) (eqv? 'quote (car orig-fm)))
						(cadr orig-fm))
					 ((and (list? orig-fm) (eqv? 'evaluate (car orig-fm)))
						(let* ((rules (evaluate-using-rules orig-rules
																															(cadr orig-fm)))
									 (fm (evaluate-using-rules orig-rules (caddr orig-fm))))
							;(printf "Dynamic evaluate: rules=~a fm=~a~n" rules fm)
							(evaluate-using-rules rules fm)))
					 (else
						 (begin
							 ;(printf "Standard evaluate: ~a ~a~n" orig-fm orig-rules)
							 (resolve (list  '_evaluate orig-rules orig-fm) macro-failed)))))))
		eval-rslt))

(define (push-base-rule rl)
	(set! base-rules (cons rl base-rules)))

; Basic rules for compiling a form representing a rule, using a user-friendly
; pattern format, to a rule with a lower-level pattern compatible with the above
; evaluation functions. These rules must themselves be, somewhat tediously, written
; by hand using the low-level format.
(push-base-rule '((const compile-pattern) 'compile-pattern))
(push-base-rule '((const compile-rule) 'compile-rule))
(push-base-rule '((const cons) 'cons))
(push-base-rule '((const list) 'list))
(push-base-rule '((const append) 'append))

(push-base-rule '((fm ((const compile-pattern) . (fm ((var rules) .
												(fm ((var x) . (fm ()))))))) (list 'var x)))
(push-base-rule '((fm ((const compile-pattern) . (fm ((var rules) .
												(fm ((fm ()) . (fm ()))))))) '(fm ())))
(push-base-rule '((fm ((const compile-pattern) . (fm ((var rules) .
												(fm ((fm ((var hd) . (var tl))) . (fm ())))))))
									(list 'fm (cons (compile-pattern rules hd) (compile-pattern rules tl)))))
(push-base-rule '((fm (
											 (const compile-pattern) .
											 (fm ((var rules) . (fm ((fm (
																 (const quote) .
																 (fm ((var x).(fm ()))))) . (fm ())))))))
									(list 'const x)))
(push-base-rule '((fm (
											 (const compile-pattern) .
											 (fm ((var rules) . (fm ((fm ((const quote)
																 . (fm ((fm (
																				(const quote) .
																				(fm ((var x) . (fm ()))))) . (fm ()))))) . (fm ())))))))
									(list 'fm (cons (list 'const 'quote) (list 'fm (cons (list 'var x)
																																			 (list 'fm '())))))))

(push-base-rule '((fm ((const compile-pattern) fm ((var rules)
																				 fm ((fm ((const unquote) fm ((var expr) fm ()))) fm ()))))
									(list (quote const) (evaluate rules expr))))

(push-base-rule '((fm ((const compile-rule) . (fm ((var rules) . (fm ((var ptn) . (fm ((var expr) .
																(fm ())))))))))
									(list (compile-pattern rules ptn) expr)))

(define global-rules base-rules)
(define (push-global-rule rl)
	(set! global-rules (cons rl global-rules)))

(define (directory-name-from-path path)
	(let find-slashes ((k 0) (p "./") (l (string-length path)))
		(cond
			((= k l) p)
			((char=? (string-ref path k) #\/) (find-slashes (+ 1 k) (substring path 0 k) l))
			(else (find-slashes (+ 1 k) p l)))))

(define (file-name-from-path path)
	(let find-slashes ((k 0) (p path) (l (string-length path)))
		(cond
			((= k l) p)
			((char=? (string-ref path k) #\/) (find-slashes (+ 1 k) (substring path (+ 1 k) l) l))
			(else (find-slashes (+ 1 k) p l)))))

(define (include-rules-from-file path expression-wrapper)
	(define (load-from-port p)
		(port-count-lines! p)
		(let ((rslt (void)))
			(let read-next-data ()
				(let ((src (read p)))
					;(set! src stx)
					;(unless (eof-object? stx)
					(if (eof-object? src)
						rslt
						(cond
							;((not (list? src)) (set! rslt (evaluate-using-rules global-rules src)))
							((null? src) (set! rslt null))
							((and (list? src) (eqv? (car src) 'define))

							 (let ((ptn (cadr src))
										 (expr (cddr src)))
								 (push-global-rule (evaluate-using-rules
																	 global-rules
																	 `(compile-rule (quote ,global-rules) (quote ,ptn)
																									(quote ,(expression-wrapper expr)))))))
							(else (begin
											;(printf "Evaluating top-scope: ~a~n" src)
											(set! rslt
												(evaluate-using-rules global-rules
																							`(evaluate (quote ,global-rules) (quote ,src))))))))
					(if (eof-object? src)
						rslt
						(read-next-data))))))
	(let ((filename (file-name-from-path path))
				(dirname (directory-name-from-path path))
				(olddirname (current-directory)))
		(current-directory dirname)
		(let ((rslt (call-with-input-file filename load-from-port)))
			(current-directory olddirname)
			rslt)))

(include-rules-from-file "src/evaluate.jest" (lambda (fm) (car fm)))
(include-rules-from-file "src/quasiquote.jest" (lambda (fm) (car fm)))
(include-rules-from-file "src/scope.jest" (lambda (fm) (car fm)))
(include-rules-from-file "src/module.jest" (lambda (fm) (car fm)))
(include-rules-from-file "src/import.jest" (lambda (fm) (car fm)))
