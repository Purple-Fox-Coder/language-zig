; https://github.com/tree-sitter-grammars/tree-sitter-zig/blob/master/queries/highlights.scm

; Variables

; enum
(enum_declaration
  (container_field
    name: (identifier) @variable.declaration.enum.zig))

(error_set_declaration
  (identifier) @variable.declaration.enum.zig)

; general variable declarations
(variable_declaration
  (identifier) @variable.declaration.zig)

; Parameters
(parameter
  name: (identifier) @variable.parameter.zig)

(payload
  (identifier) @variable.parameter.zig)

; Types

(parameter
  type: (identifier) @support.other.storage.type.zig)

(parameter
  type: (pointer_type
    (identifier) @support.other.storage.type.zig))

; not really a standard in zig, but I will support this naming
; convention anyway ([IDENT]_t as a type)
((identifier) @support.other.storage.type.zig
  (#match? @support.other.storage.type.zig "_t$"))

; The more standard zig naming convention for types
((identifier) @support.other.storage.type.zig
  (#match? @support.other.storage.type.zig "^[A-Z_][a-zA-Z0-9_]*")
  ; avoid matching constants
  (#not-match? @support.other.storage.type.zig "^[A-Z][A-Z_0-9]+$"))

(variable_declaration
  (identifier) @entity.declaration.type.enum
  "="
  [
    (struct_declaration ) @entity.name.type.struct.zig
    (enum_declaration   ) @entity.name.type.enum.zig
    (union_declaration  ) @entity.name.type.union.zig
    (opaque_declaration ) @entity.name.type.opaque.zig
  ])

[
  (builtin_type)
  "anyframe"
] @support.storage.type.builtin.zig

; Constants
((identifier) @constant.other.zig
  (#match? @constant.other.zig "^[A-Z][A-Z_0-9]+$")
  (#set! capture.shy))

[
  "null"
  "unreachable"
  "undefined"
] @constant.language.null.zig

(boolean) @constant.language.boolean._TYPE_.zig

; Labels
(block_label
  (identifier) @entity.name.label.zig)

(break_label
  (identifier) @entity.name.label.zig)

; Fields
(assignment_expression
  (field_expression
      member: (identifier) @variable.declaration.member.zig))

; I haven't been able to find where this gets set up
; (field_initializer
;   .
;   (identifier) @variable.declaration.member.zig)

; I don't believe this will do what we want, but leaving it here anyway
; (field_expression
;   (_)
;   member: (identifier) @variable.member)

; This is handled via other methods
; (container_field
;   name: (identifier) @variable.member)

; Functions
((builtin_identifier) @keyword.control.directive.include.zig
  (#match? @keyword.control.directive.include.zig "^@(import|cImport|cInclude)$"))

((builtin_identifier) @keyword.control.directive.define.zig
  (#match? @keyword.control.directive.define.zig "^@cDefine$"))

; list copied from https://ziglang.org/documentation/master/#Builtin-Functions
; @import was removed due to it having its own match (see above)
; same for the @c<thing> functions
((builtin_identifier) @keyword.control.directive.zig
  (#match? @keyword.control.directive.zig "^@(addrSpaceCast|addWithOverflow|alignCast|alignOf|as|atomicLoad|atomicRmw|atomicStore|bitCast|bitOffsetOf|bitSizeOf|branchHint|breakpoint|mulAdd|byteSwap|bitReverse|offsetOf|call|clz|cmpxchgStrong|cmpxchgWeak|compileError|compileLog|constCast|ctz|cUndef|cVaArg|cVaCopy|cVaEnd|cVaStart|divExact|divFloor|divTrunc|embedFile|enumFromInt|errorFromInt|errorName|errorReturnTrace|errorCast|export|extern|field|fieldParentPtr|FieldType|floatCast|floatFromInt|frameAddress|hasDecl|hasField|inComptime|intCast|intFromBool|intFromEnum|intFromError|intFromFloat|intFromPtr|max|memcpy|memset|memmove|min|wasmMemorySize|wasmMemoryGrow|mod|mulWithOverflow|panic|popCount|prefetch|ptrCast|ptrFromInt|rem|returnAddress|select|setEvalBranchQuota|setFloatMode|setRuntimeSafety|shlExact|shlWithOverflow|shrExact|shuffle|sizeOf|splat|reduce|src|sqrt|sin|cos|tan|exp|exp2|log|log2|log10|abs|floor|ceil|trunc|round|subWithOverflow|tagName|This|trap|truncate|EnumLiteral|Int|Tuple|Pointer|Fn|Union|Struct|Enum|typeInfo|typeName|TypeOf|unionInit|Vector|volatileCast|workGroupId|workGroupSize|workItemId)$"))

; The "foo" in `foo();`.
(call_expression
  function: (identifier) @support.other.function.zig)

; The "foo" in `thing->troz->foo(...)`.
(call_expression
  function: (field_expression
    member: (identifier) @support.other.function.zig))

(function_declaration
  name: (identifier) @entity.name.function.zig)

; Builtins
"c" @support.variable.builtin.zig

"..." @keyword.operator.ellipsis.zig

((identifier) @support.variable.builtin.zig
  (#eq? @support.variable.builtin.zig "_"))

; Keywords
(calling_convention) @keyword.control.directive.zig

[
  "asm"
  "defer"
  "errdefer"
  "test"
  "error"
  "const"
  "var"
] @keyword.control._TYPE_.zig

[
  "struct"
  "union"
  "enum"
  "opaque"
] @keyword._TYPE_.storage.type.zig

[
  "async"
  "await"
  "suspend"
  "nosuspend"
  "resume"
] @keyword.control._TYPE_.zig

"fn" @keyword.function.zig

[
  "and"
  "or"
  "orelse"
  "||"
] @keyword.operator.logical._TYPE_.zig

[
  "return"
  "if"
  "else"
  "switch"
  "for"
  "while"
  "break"
  "continue"
  "usingnamespace"
  "try"
  "catch"
] @keyword.control._TYPE_.zig

[
  "export"
  "volatile"
  "allowzero"
  "noalias"
  "addrspace"
  "align"
  "callconv"
  "linksection"
  "pub"
  "inline"
  "noinline"
  "extern"
  "comptime"
  "packed"
  "threadlocal"
] @storage.modifier._TYPE_.zig

; Operator
"=" @keyword.operator.assignment.zig

[
  "*="
  "*%="
  "*|="
  "/="
  "%="
  "+="
  "+%="
  "+|="
  "-="
  "-%="
  "-|="
  "<<="
  "<<|="
  ">>="
  "&="
  "^="
  "|="
] @keyword.operator.assignment.compound.zig

[
  "=="
  "!="
  ">"
  "<"
  ">="
  "<="
] @keyword.operator.comparison.zig

[
  "~"
  "&"
  "^"
  "|"
  "<<"
  ">>"
  "<<|"
  ; "|>>"
] @keyword.operator.bitwise.zig

"++" @keyword.operator.increment.zig
; "--" @keyword.operator.decrement.zig

[
  "-"
  "-%"
  "-|"
  "+"
  "+%"
  "+|"
  "*"
  "*%"
  "*|"
  "**"
  "/"
  "%"
] @keyword.operator.arithmetic.zig

; (unary_expression ["+" "-" "!"] @keyword.operator.unary.zig)

[
  ".*"
  ".?"
  "?"
  ".."
  "!"
] @keyword.operator.zig

; Literals
(character) @character.literal.constant.zig

([
  (string)
  (multiline_string)
] @string.quoted.double.zig
  (#set! "priority" 95))

(integer) @number.literal.constant.zig

(float) @number.float.literal.constant.zig

(boolean) @boolean.literal.constant.zig

(escape_sequence) @constant.character.escape.zig

; Punctuation
";" @punctuation.terminator.statement.zig

"{" @punctuation.definition.block.begin.bracket.curly.zig
"}" @punctuation.definition.block.end.bracket.curly.zig
"(" @punctuation.definition.begin.bracket.round.zig
")" @punctuation.definition.end.bracket.round.zig
"[" @punctuation.definition.array.begin.bracket.square.zig
"]" @punctuation.definition.array.end.bracket.square.zig

[
  "."
  ","
  ":"
  "=>"
  "->"
] @punctuation.separator._TYPE_.zig

(payload
  "|" @punctuation.seperator.zig)

; Comments
(comment) @comment.zig

((comment) @comment.documentation.zig
  (#match? @comment.documentation.zig "^(//!|///)"))
