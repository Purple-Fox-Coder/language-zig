; https://github.com/tree-sitter-grammars/tree-sitter-zig/blob/master/queries/injections.scm

; NOTE: May implement at a later date to interface with pulsar, but currently I
;       do not see a reason to(?)

((comment) @injection.content
  (#set! injection.language "comment"))

; TODO: add when asm is added
; (asm_output_item (string) @injection.content
;   (#set! injection.language "asm"))
; (asm_input_item (string) @injection.content
;   (#set! injection.language "asm"))
; (asm_clobbers (string) @injection.content
;   (#set! injection.language "asm"))
