import { Repository, TmLanguage } from "./TmLanguage.d.ts";

const inc = (...refs: string[]) =>
  refs.map((ref) => ({ "include": ref } as const));

export const ScopeName = "howlite" as const satisfies string;
export type ScopeNameT = typeof ScopeName;

export type LiteralIntName = `constant.numeric.int.${ScopeNameT}`;
const literalIntPattern =
  "[+-]?(0x([0-9A-Fa-f]*))|(0o[0-7]*)|(0b[01]*)|[0-9]+" as const satisfies string;

export type OpRangeName = `keyword.operator.range.${ScopeNameT}`;
const opRangePattern = "\\.\\." as const satisfies string;

export type DeclKeywordName =
  | `storage.type.${"function" | "type"}.${ScopeNameT}`
  | `storage.modifier.extern.${ScopeNameT}`;
export const declKeyword = {
  "decl-kw-func": {
    name: `storage.type.function.${ScopeName}`,
    match: "\\b(func)\\b",
  },
  "decl-kw-type": {
    name: `storage.type.type.${ScopeName}`,
    match: "\\b(type)\\b",
  },
  "decl-kw-extern": {
    name: `storage.modifier.extern.${ScopeName}`,
    match: "\\b(extern)\\b",
  },
} as const satisfies Repository<DeclKeywordName>;

export type TyExprName =
  | `support.type.primitive.null.${ScopeNameT}`
  | `support.type.primitive.bool.${ScopeNameT}`
  | `support.type.primitive.int.${ScopeNameT}`
  | LiteralIntName
  | OpRangeName;
export const tyExprPatterns = {
  "ty-primitive-null": {
    name: `support.type.primitive.null.${ScopeName}`,
    match: "\\b(null)\\b",
  },
  "ty-primitive-bool": {
    name: `support.type.primitive.bool.${ScopeName}`,
    match: "\\b(bool)\\b",
  },
  "ty-primitive-int-range": {
    name: `support.type.primitive.int.${ScopeName}`,
    match:
      `(${literalIntPattern})\\s*(${opRangePattern})?\\s*(${literalIntPattern})?`,
    captures: {
      "1": { name: "constant.numeric.int.howlite" },
      "2": { name: "keyword.operator.range.howlite" },
      "3": { name: "constant.numeric.int.howlite" },
    },
  },
} as const satisfies Repository<TyExprName>;

export type Name = `source.${ScopeNameT}` | DeclKeywordName | TyExprName;

export default {
  name: "Howlite",
  scopeName: `source.${ScopeName}`,
  patterns: [
    ...inc(
      "#decl-kw-func",
      "#decl-kw-type",
      "#decl-kw-extern",
      "#ty-primitive-null",
      "#ty-primitive-bool",
      "#ty-primitive-int-range",
    ),
  ],
  repository: { ...declKeyword, ...tyExprPatterns },
} as const satisfies TmLanguage<Name>;
