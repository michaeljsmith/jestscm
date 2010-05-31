#! /usr/bin/env mzscheme

;--------------------------------------------------------------------------------
;
;  Tested under:
;  MzScheme v4.1.5 (Ubuntu Linux 10.04)
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
;  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS:wa
;  IN THE SOFTWARE.
;
;--------------------------------------------------------------------------------

;--------------------------------------------------------------------------------
; TODO:
; * Auto defining operators.
; * 
;--------------------------------------------------------------------------------

#lang scheme

(require scheme/list)
(require scheme/string)

; Basic evaluation function, implemented in scheme. This function is the
; lowest-level method of evaluating a function, used for bootstrapping
; higher level self-hosting evaluations. Ideally this function will not
; actually be run during use, a higher level version will be self-compiled
; to a backend. This function takes a fallback scheme lambda to use
; to evaluate any undefined forms, which allows standard scheme expressions
; to be compiled.
; Note that this evaluation scheme is very simply implemented, and in particular
; scoping is dynamic. Lexical scoping is implemented at a higher level.
(define (evaluate-using-rules-with-fallback fallback in-rules in-fm)
	(define (resolve fm)
		(let recurse ((rules in-rules))
			(define (match-const val fm)
				(cond
					((pair? fm) '(#f))
					((eqv? val fm) '(#t ()))
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
										in-rules
										(cons
											(let* ((bdng (car bs))
														 (name (car bdng))
														 (value (cadr bdng)))
												(list (list 'const name) (list 'quote value)))
											(bind (cdr bs)))))))
					(evaluate-using-rules-with-fallback fallback new-rules fm)))
			(cond
				((null? rules)
				 (apply fallback (list fm)))
				(else (let* ((rule (car rules))
										 (rule-ptn (car rule))
										 (rule-expr (cadr rule))
										 (match-rslt (match-ptn rule-ptn fm))
										 (match-scs (car match-rslt)))
								(if match-scs
									(bind-and-evaluate (cadr match-rslt) rule-expr)
									(recurse (cdr rules))))))))
	(printf "eval: ~a~n" in-fm)
	(let
		((eval-rslt
			 (cond
				 ((not (list? in-fm)) (resolve in-fm))
				 ((eqv? 'quote (car in-fm))
					(begin
						(cadr in-fm)))
				 (else
					 (begin
						 (let
							 ((subfms (let eval-subfms ((subfms in-fm))
													(if (null? subfms)
														null
														(cons (evaluate-using-rules-with-fallback fallback in-rules (car subfms))
																	(eval-subfms (cdr subfms)))))))
							 (resolve subfms)))))))
		eval-rslt))

; Wrapper for the above evaluation function which uses standard scheme eval()
; to handle fallback cases.
(define base-rules '())
(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))
(define (evaluate-using-rules rules fm)
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
			(eval quoted-list eval-ns)))
	(evaluate-using-rules-with-fallback scheme-evaluate rules fm))
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

(push-base-rule '((fm ((const compile-pattern) . (fm ((var x) . (fm ()))))) (list 'var x)))
(push-base-rule '((fm ((const compile-pattern) . (fm ((fm ()) . (fm ()))))) '(fm ())))
(push-base-rule '((fm ((const compile-pattern) . (fm ((fm ((var hd) . (var tl))) . (fm ())))))
									(list 'fm (cons (compile-pattern hd) (compile-pattern tl)))))
(push-base-rule '((fm (
											 (const compile-pattern) .
											 (fm ((fm (
																 (const quote) .
																 (fm ((var x).(fm ()))))) . (fm ())))))
									(list 'const x)))
(push-base-rule '((fm (
											 (const compile-pattern) .
											 (fm ((fm ((const quote)
																 . (fm ((fm (
																				(const quote) .
																				(fm ((var x) . (fm ()))))) . (fm ())) . (fm ())))) . (fm ())))))
									(list 'fm (cons (list 'const 'quote) (list 'var x)))))
(push-base-rule '((fm ((const compile-rule) . (fm ((var ptn) . (fm ((var expr) . (fm ())))))))
									(list (compile-pattern ptn) expr)))

; Some scheme functions for evaluating expressions represented using quoted
; scheme forms.
(define (compile-operator op)
	(let ((rslt
					(evaluate-using-rules
						base-rules
						`(compile-rule (quote (quote ,op)) (quote (quote ,op))))))
		rslt))

(define (define-base-operator op)
	(push-base-rule (compile-operator op)))

(define (compile-rule ptn expr)
	(evaluate-using-rules
		base-rules
		`(compile-rule (quote ,ptn) (quote ,expr))))

(define (define-base-rule ptn expr)
	(push-base-rule (compile-rule ptn expr)))

(define (include-rules-from-file filename)
	(define (load-from-port p)
			(port-count-lines! p)
			(let read-next-data ()
				(let ((src (read p)))
							;(set! src stx)
							;(unless (eof-object? stx)
							(unless (eof-object? src)
								;(set! src (ruse-perform-reader-expansions (syntax->source stx)))
								(cond
									((not (list? src)) (evaluate-using-rules base-rules src))
									((null? src) null)
									((eqv? (car src) 'define)
									 (let ((ptn (cadr src))
												 (expr (caddr src)))
										 (push-base-rule (evaluate-using-rules
																			 base-rules
																			 `(compile-rule (quote ,ptn) (quote ,expr))))))
									(else (evaluate-using-rules base-rules src))))
							(unless (eof-object? src)
								(read-next-data)))))
	(call-with-input-file filename load-from-port))

(include-rules-from-file "src/scope.jest")
(include-rules-from-file "src/quasi.jest")

(define (evaluate-expression fm)
	(evaluate-using-rules base-rules `(evaluate base-rules (quote ,fm))))

(printf "---------------------~n")
(evaluate-expression
	'`hello)

;(evaluate-expression
;	'(scope
;		 (define 'foo 'foo)
;		 (define ('foo s)
;		   s)
;		 (foo 1)))

;(evaluate-expression
;	'(scope
;		 (define 'double 'double)
;		 (define ('double y)
;			 (+ y y))
;		 (double 3)))

;(evaluate-expression
;	'(scope
;		 (define 'foo 'foo)
;		 (define ('foo x)
;			 (scope
;				 (define 'bar 'bar)
;				 (define ('bar y)
;					 (+ x y))
;				 (bar 3)))
;		 (foo 4)))

;(evaluate-expression
;	'(scope
;		 (define 'bar 'bar)
;		 (define ('bar y)
;			 (+ 1 y))
;		 (define 'foo 'foo)
;		 (define ('foo x)
;			 (bar x))
;		 (foo 2)))

;(evaluate-expression
;	'(scope
;		 (define 'foo 'foo)
;		 (define ('foo x)
;			 x)
;		 (define 'bar 'bar)
;		 (define ('bar y)
;			 (scope
;				 (foo (+ 1 y))))
;		 (bar 1)))

;(evaluate-expression
;	'(scope
;		 (define 'foo 'foo)
;		 (define ('foo x)
;			 (define 'baz 'baz)
;			 (define ('baz z)
;				 (bar x))
;			 (baz x))
;		 (define 'bar 'bar)
;		 (define ('bar y)
;			 y)
;		 (foo 1)))
