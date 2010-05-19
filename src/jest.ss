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

(require scheme/list)
(require scheme/string)

(define dbg-indnt 0)
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
				(evaluate-using-rules fallback new-rules fm)))
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
	(printf "in-fm: ~a~nrules: ~a~n~n" in-fm in-rules)
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
									 (cons (evaluate-using-rules fallback in-rules (car subfms))
												 (eval-subfms (cdr subfms)))))))
							 (resolve subfms)))))))
		eval-rslt))

(define base-rules '())
(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))
(define (evaluate-builtin rules fm)
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
  (evaluate-using-rules scheme-evaluate rules fm))
(define (push-base-rule rl) (set! base-rules (cons rl base-rules)))

(push-base-rule '((const compile-pattern) 'compile-pattern))
(push-base-rule '((const compile-rule) 'compile-rule))
(push-base-rule '((const cons) 'cons))
(push-base-rule '((const list) 'list))
(push-base-rule '((const append) 'append))

(push-base-rule '((fm ((const compile-pattern) . (fm ((var x) . (fm ()))))) (list 'const x)))
(push-base-rule '((fm ((const compile-pattern) . (fm ((fm ()) . (fm ()))))) '(fm ())))
(push-base-rule '((fm ((const compile-pattern) . (fm ((fm ((var hd) . (var tl))) . (fm ())))))
				  (list 'fm (cons (compile-pattern hd) (compile-pattern tl)))))
(push-base-rule '((fm (
											 (const compile-pattern) .
											 (fm ((fm (
																 (const quote) .
																 (fm ((var x).(fm ()))))) . (fm ())))))
									(list 'var x)))
(push-base-rule '((fm ((const compile-rule) . (fm ((var ptn) . (fm ((var expr) . (fm ())))))))
				  (list (compile-pattern ptn) expr)))

(define (compile-operator op)
  (let ((rslt
		  (evaluate-builtin
			base-rules
			`(compile-rule (quote ,op) (quote (quote ,op))))))
	rslt))

(define (define-base-operator op)
  (push-base-rule (compile-operator op)))

(define (compile-rule ptn expr)
  (evaluate-builtin
		base-rules
		`(compile-rule (quote ,ptn) (quote ,expr))))

(define (define-base-rule ptn expr)
  (push-base-rule (compile-rule ptn expr)))

(define-base-operator 'evaluate-list)
(define-base-rule
  '(evaluate-list 'rules ('head . 'tail))
  '(cons (list 'quote ('evaluate rules head)) ('evaluate-list rules tail)))

(define-base-rule
  '(evaluate-list 'rules ())
  ''())

(define-base-operator 'evaluate)
(define-base-rule
	'(evaluate 'rules 'fm)
	'(second
		 (printf "evaluate:~n  rules=~a~n  fm=~a~n~n" rules fm)
		 (evaluate-impl rules fm)))

(define-base-operator 'evaluate-impl)
(define-base-rule
  '(evaluate-impl 'rules 'fm)
  '(evaluate-builtin rules fm))

(define-base-rule
  '(evaluate-impl 'rules ('head . 'tail))
  '(evaluate-builtin rules ('evaluate-list rules (cons head tail))))

(define-base-rule
	'(evaluate-impl 'rules ('scope-sym rule 'ptn 'expr)) ; Could this be a rule?
	'(compile-rule-pattern-expression-pair
		 (wrap-rule-with-evaluate ptn expr (list (compile-rule ''expr (list 'cons scope-sym 'expr))))))

(push-base-rule
	'((fm ((const evaluate-impl)
				 . (fm ((var rules)
						. (fm ((fm ((const quote)
												. (fm ((var val)
															 . (fm ()))))) . (fm ())))))))
		val))

(define-base-operator 'evaluate2)
(define-base-rule
	'(evaluate2 'rules 'fm)
	'(second
		 (printf "evaluate2: ~a~n~n" (list 'evaluate (list 'quote rules) (list 'quote fm)))
		 (evaluate-builtin rules (list 'evaluate (list 'quote rules) (list 'quote fm)))))

(define-base-operator 'second)
(define-base-rule
   '(second 'x0 'x1)
   'x1)

(define-base-operator 'evaluate-scope-clauses)
(define-base-rule
  '(evaluate-scope-clauses 'scope-sym 'rules 'defs ('head . 'tail))
  '(second (evaluate2 rules head) (evaluate-scope-clauses defs scope-sym rules tail)))

(define-base-rule
  '(evaluate-scope-clauses 'scope-sym 'rules 'defs ())
  ''())

(define-base-rule
  '(evaluate-scope-clauses 'scope-sym 'rules 'defs ('clause))
  '(second
		 (printf "Evaluating last clause: ~a~n~n" clause) ; TODO: Modify this to handle redirected rules.
		 (evaluate2
			 (cons
				 (compile-rule
					 (list 'evaluate-impl ''rules (cons scope-sym ''expr))
					 (list 'evaluate2 (list 'quote rules) 'expr))
				 defs)
			 clause)))


;(list
;		 (list 'evaluate-impl ''rules ptn)
;		 (list 'evaluate2 (generate-binding-code-from-pattern ptn lexical-rules) (list 'quote expr)))

(define-base-rule
  '(evaluate-scope-clauses 'scope-sym 'rules 'defs ((define 'rule) . 'tail))
  '(evaluate-scope-clauses scope-sym rules (cons (evaluate2 rules (cons scope-sym rule)) defs) tail))

(define-base-operator 'gensym)
(define-base-operator 'scope)
(define-base-rule
  '(evaluate-impl 'rules (scope . 'clauses))
  '(evaluate-scope-clauses (gensym "scope") rules '() clauses))

(define-base-operator 'extract-bindings-from-pattern)
(define-base-rule
	'(extract-bindings-from-pattern 'ptn)
	''())

(define-base-rule
	'(extract-bindings-from-pattern ('hd . 'tl))
	'(append
		 (extract-bindings-from-pattern hd)
		 (extract-bindings-from-pattern tl))) ; Might cause duplicates, but shouldn't matter.

(push-base-rule
	'((fm ((const extract-bindings-from-pattern)
				 . (fm ((fm ((const quote) . (fm ((var name) . (fm ()))))) . (fm ())))))
		(list name)))

(define-base-operator 'generate-binding-code-from-bindings)
(define-base-rule
	'(generate-binding-code-from-bindings ('bdng . 'bdng-tl) 'lexical-rules)
	'(list 'cons
				 (list 'list (list 'list ''const (list 'quote bdng)) (list 'evaluate 'rules bdng))
				 (generate-binding-code-from-bindings bdng-tl lexical-rules)))
(define-base-rule
	'(generate-binding-code-from-bindings () 'lexical-rules)
	'(list 'quote lexical-rules))

(define-base-operator 'generate-binding-code-from-pattern)
(define-base-rule
	'(generate-binding-code-from-pattern 'ptn 'lexical-rules)
	'(generate-binding-code-from-bindings (extract-bindings-from-pattern ptn) lexical-rules))

(define-base-operator 'wrap-rule-with-evaluate)
(define-base-rule
	'(wrap-rule-with-evaluate 'ptn 'expr 'lexical-rules)
	'(list
		 (list 'evaluate-impl ''rules ptn)
		 (list 'evaluate2 (generate-binding-code-from-pattern ptn lexical-rules) (list 'quote expr))))

(define-base-operator 'compile-rule-pattern-expression-pair)
(define-base-rule
	'(compile-rule-pattern-expression-pair ('ptn 'expr))
	'(compile-rule ptn expr))

(define (evaluate-expression fm)
	(evaluate-builtin base-rules `(evaluate base-rules (quote ,fm))))

(evaluate-expression
	'(scope
		 (define (rule foo 'foo))
		 (define (rule (foo 's) s))
		 (foo 1)))

;(evaluate-expression
;	'(scope
;		 (define (rule (foo 'x) x))
;		 (define (rule (bar 'y) (foo y)))
;		 (bar 2)))

;(evaluate-expression
;	'(scope
;		 (define (rule double 'double))
;		 (define (rule (double 'y) (+ y y)))
;		 (double 3)))

;(evaluate-expression
;	'(scope
;		 (define (rule foo 'foo))
;		 (define
;			 (rule (foo 'x)
;						 (scope
;							 (define (rule bar 'bar))
;							 (define
;								 (rule (bar 'y) (+ x y)))
;							 (bar 3))))
;		 (foo 4)))

;(evaluate-expression
;	'(scope
;		 (define (rule bar 'bar))
;		 (define (rule (bar 'y) (+ 1 y)))
;		 (define (rule foo 'foo))
;		 (define (rule (foo 'x) (bar x)))
;		 (foo 2)))
