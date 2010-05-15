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
;  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
;  IN THE SOFTWARE.
;
;--------------------------------------------------------------------------------

;--------------------------------------------------------------------------------
; Steps:
; * Define apply.
; * Define rule for compiling rules.
; * Define main evaluate rule, including macro step.
; * Define scope rule.
; * Define def.
; * Define quantify.
;--------------------------------------------------------------------------------

#lang scheme

(define (evaluate-using-rules fallback in-rules in-fm)
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
										 ((bdng (begin (printf "merging: ~a into ~a~n" to-mrg bdngs) (car to-mrg)))
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
			(printf "match-ptn~n  ptn=~a~n  fm=~a~n" ptn fm)
			(let ((ptn-tp (car ptn))
						(ptn-val (cadr ptn)))
				(cond
					((eqv? ptn-tp 'const) (match-const ptn-val fm))
					((eqv? ptn-tp 'var) (match-var ptn-val fm))
					((eqv? ptn-tp 'fm) (match-fm ptn-val fm))
					(else (car car)))))
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
				(evaluate-using-rules fallback new-rules fm)))
	  (cond
		((null? rules)
		 (begin
		   (printf "rules exhausted for expr ~a~n" fm)
		   (let ((rslt (apply fallback (list fm))))
				 (printf "  fallback rslt (~a):~n    ~a~n" fm rslt)
				 rslt)))
		(else (let* ((rule (car rules))
					 (rule-ptn (car rule))
					 (rule-expr (cadr rule))
					 (match-rslt (match-ptn rule-ptn fm))
					 (match-scs (car match-rslt)))
				(if match-scs
				  (bind-and-evaluate (cadr match-rslt) rule-expr)
				  (recurse (cdr rules))))))))
  (cond
	((not (list? in-fm)) (resolve in-fm))
	((eqv? 'quote (car in-fm))
	 (begin
	   (printf "quote (~a) = ~a~n" in-fm (cadr in-fm))
	   (cadr in-fm)))
	(else
	  (begin
		(printf "in-fm = ~a~n" in-fm)
		(resolve
		  (let eval-subfms ((subfms in-fm))
			(if (null? subfms)
			  null
			  (cons (evaluate-using-rules fallback in-rules (car subfms))
					(eval-subfms (cdr subfms))))))))))

(define base-rules '())
(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))
(define (evaluate-builtin rules fm)
  (define (scheme-evaluate fm)
	(let ((quoted-list
					(cons
						(car fm)
						(let quote-list ((ls (cdr fm)))
							(cond
								((null? ls) null)
								((list? ls) (cons `(quote ,(car ls)) (quote-list (cdr ls))))
								(else ls))))))
	  (printf "scheme eval ~a~n" quoted-list)
	  (eval quoted-list eval-ns)))
  (printf "eval-builtin ~a~n" fm)
  (evaluate-using-rules scheme-evaluate rules fm))
(define (push-base-rule rl) (set! base-rules (cons rl base-rules)))

(push-base-rule '((const compile-pattern) 'compile-pattern))
(push-base-rule '((const rule) 'rule))
(push-base-rule '((const cons) 'cons))
(push-base-rule '((const list) 'list))

(push-base-rule '((fm ((const compile-pattern) . (fm ((var x) . (fm ()))))) (list 'const x)))
(push-base-rule '((fm ((const compile-pattern) . (fm ((fm ((const quote) . (fm ((var x).(fm ())))))))))
									(list 'var x)))
(push-base-rule '((fm ((const compile-pattern) . (fm ((fm ()) . (fm ()))))) '(const ())))
(push-base-rule '((fm ((const compile-pattern) . (fm ((fm ((var hd) . (var tl))) . (fm ())))))
				  (cons (compile-pattern hd) (compile-pattern tl))))
(push-base-rule '((fm ((const rule) . (fm ((var ptn) . (fm ((var expr) . (fm ())))))))
				  (list (compile-pattern ptn) expr)))

(define (compile-operator op)
  (let ((rslt
		  (evaluate-builtin
			base-rules
			`(rule (quote ,op) (quote ,op)))))
	(printf "compile-operator returned ~a~n" rslt)
	rslt))

(define (define-base-operator op)
  (push-base-rule (compile-operator op)))

(define (compile-rule ptn expr)
  (evaluate-builtin
		base-rules
		`(rule (quote ,ptn) (quote ,expr))))

(define (define-base-rule ptn expr)
  (push-base-rule (compile-rule ptn expr)))

(define-base-operator 'evaluate-list)
(define-base-rule
  '(evaluate-list 'rules ('head . 'tail))
  '(cons (evaluate rules head) (evaluate-list rules tail)))

;(define-base-rule
;  '(evaluate-list 'rules ())
;  'null)
;
;(define-base-rule
;  '(evaluate 'rules 'fm)
;  '(evaluate-using-rules rules (evaluate-list rules fm)))
;
;(define-base-rule
;   '(second 'x0 'x1)
;   'x1)
;
;(define-base-rule
;  '(evaluate-scope-clauses 'rules (head . tail))
;  '(second (evaluate rules head) (evaluate-scope-clauses rules tail)))
;
;(define-base-rule
;  '(evaluate-scope-clauses 'rules ())
;  'null)
;
;(define-base-rule
;  '(evaluate-scope-clauses 'rules ('clause))
;  '(evaluate rules clause))
;
;(define-base-rule
;  '(evaluate-scope-clauses 'rules ((define 'rule) . tail))
;  '(evaluate-scope-clauses (cons (evaluate rules rule) rules) tail))
;
;(define-base-rule
;  '(evaluate 'rules (scope . 'clauses))
;  '(evaluate-scope-clauses rules clauses))
