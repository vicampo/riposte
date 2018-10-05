#lang br/quicklang

(require (file "grammar.rkt")
         (file "tokenizer.rkt"))

(module+ main
  (syntax->datum
   (parse (tokenize-file "/Users/jessealama/src/vicampo/platform/tests/riposte/vip-2962.rip"))))
