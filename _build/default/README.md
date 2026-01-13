# Top-down = Bottom-up

Tiny toolkit for normalising global/local session types and checking their projections.

- Library entrypoints: `Normalise.encode`/`encode_local`, `Automaton.of_global`, `Coinductive.project` (knowledge-set), `Inductive.project` (syntactic), `Automaton_to_local.automaton_to_local`.
- Data: `case studies/global/*.global` and `case studies/local/**/*.st`.
- Typical flow: normalise → build automaton → project → render local types.

CLI (`dune exec stc -- <command>`):
- `parse-global <file>` pretty-prints a global type.
- `project <file> [<role>] [--coinductive|-c|--inductive|-i] [--full|-f|--plain|-p] [--out-dir dir] [--emit-global dot|json path] [--emit-locals dot|json dir]` (shortcuts: `-cf`, `-cp`, `-if`, `-ip`; omitting `<role>` projects all roles).
- `check <file>` runs well-formedness + balance.
- `synth <local-dir> [--out file] [--emit-global dot|json path] [--emit-locals dot|json dir]` synthesises a global type and can export intermediate automata.
- `case-studies [--path dir] [--no-types]` runs the bundled benchmarks.
- `automaton-global <file> [--format dot|json] [--out path]` exports the global automaton (after normalisation).
- `automaton-local <file> [--format dot|json] [--out path]` exports the local automaton.

Examples:

```
dune build
dune exec stc -- project "case studies/global/example1.global" alice
dune exec stc -- case-studies
```
