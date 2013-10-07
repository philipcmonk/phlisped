#lang racket

(provide (all-defined-out))

(struct gnode (id name) #:prefab)

(struct parent-gnode gnode (childs vars) #:prefab)

(struct variable-gnode gnode (defined) #:prefab)

(struct function-gnode variable-gnode (args) #:prefab)

(struct argument-gnode gnode ((argument #:auto)) #:auto-value #t #:prefab)

(struct terminal-gnode gnode ((terminal #:auto)) #:auto-value #t #:prefab)

