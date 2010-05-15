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
		  ((eqv? val fm) '(#t ()))))
	  (define (match-var name fm)
		`(#t ((,name ,fm))))
	  (define (match-fm ptn fm)
		(cond
		  ((null? ptn) (if (null? fm) '(#t ()) '(#f)))
		  ((pair? ptn)
		   (if (pair? fm)
			 (let* ((hd-rslt (match-ptn (car ptn) (car fm)))
					(hd-scs (car hd-rslt))
					(tl-rslt (if hd-scs (match-fm (cdr ptn) (cdr fm)) '(#f)))
					(tl-scs (car tl-rslt))
					(scs (and hd-scs tl-scs)))
			   (if scs
				 (let ((hd-bdngs (cdr hd-rslt))
					   (tl-bdngs (cdr tl-rslt)))
				   (let merge ((to-mrg hd-bdngs) (bdngs tl-bdngs))
					 (if (null? to-mrg)
					   bdngs
					   (let*
						 ((bdng (car to-mrg))
						  (name (car bdng))
						  (value (cadr bdng))
						  (existing (assv name bdngs))
						  (scs (or (not existing) (equal? value (cadr existing))))
						  (new-bdngs (if scs (if (not existing) (cons bdng bdngs) bdngs) '())))
						 (if scs
						   (merge (cdr to-merge) new-bdngs)
						   '(#f))))))
				 '(#f)))
			 '(#f)))
		  (else
			(match-ptn ptn fm))))
	  (define (match-ptn ptn fm)
		(let ((ptn-tp (car ptn))
			  (ptn-val (cadr ptn)))
		  (cond
			((eqv? ptn-tp 'const) (match-const ptn-val fm))
			((eqv? ptn-tp 'var) (match-var ptn-val fm))
			((eqv? ptn-tp 'fm) (match-fm ptn-val fm)))))
	  (define (bind-and-evaluate bdngs fm)
		(let ((new-rules
				(let bind ((bs bdngs))
				  (if (null? bs)
					in-rules
					(cons
					  (let* ((bdng (car bs))
							 (name (car bdng))
							 (value (cadr bdng)))
						(list (list 'const name) value)
						(bind (cdr bdngs))))))))
		  (evaluate-using-rules fallback new-rules fm)))
	  (if (null? rules)
		(apply fallback (list fm))
		(let* ((rule (car rules))
			   (rule-ptn (car rule))
			   (rule-expr (cadr rule))
			   (match-rslt (match-ptn rule-ptn fm))
			   (match-scs (car match-rslt)))
		  (if match-scs
			(bind-and-evaluate (cadr match-rslt) rule-expr)
			(recurse (cdr rules)))))))
  (resolve
	(let resolve-subfms ((subfms fm))
	  (if (null? subfms)
		null
		(cons (resolve (car subfms)) (resolve-subfms (cdr subfms)))))))

(define base-rules '())
(define-namespace-anchor ns-anchor)
(define eval-ns (namespace-anchor->namespace ns-anchor))
(define (evaluate-builtin rules fm)
  (define (scheme-evaluate fm) (eval bi-expr eval-ns))
  (evaluate-using-rules scheme-evaluate rules fm))
(define (push-base-rule rl) (set! base-rules (cons rl base-rules)))

(define sample-rule
  '((fm ((const multiply-int) (fm ((const int) (var x))) (fm ((const int) (var y)))))
	(* x y)))

;(def (rule (compile-pattern 'x) (const x)))
;(def (rule (compile-pattern (quote 'x)) (var x)))
;(def (rule (compile-pattern ()) (const ())))
;(def (rule (compile-pattern ('hd . 'tl)) (cons (compile-pattern hd) (compile-pattern tl))))
;(def (rule
;	   (rule @ptn @expr)
;	   (cons (compile-pattern ptn) expr)))

(push-base-rule '((fm ((const compile-pattern) (var x))) '(const x)))
(push-base-rule '((fm ((const compile-pattern) (fm ((const quote) (var x))))) '(var x)))
(push-base-rule '((fm ((const compile-pattern) (fm ()))) '(const ())))
(push-base-rule '((fm ((const compile-pattern) (fm ((var hd) . (var tl)))))
				  (cons '(compile-pattern hd) '(compile-pattern tl))))
(push-base-rule '((fm ((const rule) (var ptn) (var expr)))
				  (cons '(compile-pattern ptn) expr)))

(push-base-rule
  (evaluate-builtin
	base-rules
	'(rule
	   (evaluate-list 'rules 'list)
	   (cons (evaluate rules (car list)) (evaluate-list rules (cdr list))))))

(push-base-rule
  (evaluate-builtin
	base-rules
	'(rule
	   (evaluate-list 'rules ())
	   null)))

(push-base-rule
  (evaluate-builtin
	base-rules
	'(rule
	   (evaluate 'rules 'fm)
	   (evaluate-using-rules rules (evaluate-list rules fm)))))

(push-base-rule
  (evaluate-builtin
	base-rules
	'(rule
	   (second 'x0 'x1)
	   x1)))

(push-base-rule
  (evaluate-builtin base-rules
	'(rule
	   (evaluate-scope-clauses 'rules (head . tail))
	   (second (evaluate rules head) (evaluate-scope-clauses rules tail)))))

(push-base-rule
  (evaluate-builtin base-rules
	'(rule
	   (evaluate-scope-clauses 'rules ())
	   null)))

(push-base-rule
  (evaluate-builtin base-rules
	'(rule
	   (evaluate-scope-clauses 'rules ('clause))
	   (evaluate rules clause))))

(push-base-rule
  (evaluate-builtin base-rules
	'(rule
	   (evaluate-scope-clauses 'rules ((define 'rule) . tail))
	   (evaluate-scope-clauses (cons (evaluate rules rule) rules) tail))))

(push-base-rule
  (evaluate-builtin base-rules
	'(rule
	   (evaluate 'rules (scope . 'clauses))
	   (evaluate-scope-clauses rules clauses))))
