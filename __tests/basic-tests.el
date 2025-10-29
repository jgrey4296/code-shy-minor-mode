;;; basic-tests.el -*- lexical-binding: t; no-byte-compile: t; -*-
(require 'buttercup)
(require 'code-shy-minor-mode)

(describe "sanity"
  ;; Specs:
  (it "is a sanity test" (expect t :to-be (not nil)))
  (it "has the sensor function" (expect (functionp 'code-shy-sensor-h) :to-be t))
)
