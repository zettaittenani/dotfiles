# Coding Discipline (applies to ALL repositories)

When making non-trivial changes in any repository, run these checks **before** writing code, not after CI catches them. The cost of a 1-minute upfront look is much lower than the cost of multiple reactive CI cycles.

## Before writing the first edit

- **Read the project's lint config** (`eslint.config.*`, `.eslintrc.*`, `biome.json`, `ruff.toml`, ...) for strict rules that reject patterns you commonly use. Examples to look for in TypeScript projects:
  - `@typescript-eslint/consistent-type-assertions` with `assertionStyle: "never"` forbids **all** `as` casts in production code
  - `@typescript-eslint/no-explicit-any`
  - `@typescript-eslint/no-non-null-assertion` (forbids `!`)
  - `no-restricted-imports`, custom plugin rules (eg `coedo/*`)
- Pay attention to **test-file overrides** — some rules are off in tests but on in production code, and the same pattern that's OK in a `*.test.ts` is rejected elsewhere.
- For each new pattern you are about to introduce (type guard, error class, config object, validation helper, ...), **grep the codebase for an existing example and mirror it**. Inventing a new shape is a last resort; the project almost always has a precedent.
- For code style choices that are not purely lint-driven (file layout, dependency injection, error handling), look at the nearest sibling file before inventing your own structure.

## When CI or local lint fails

- **If the same lint rule fails twice in a row, stop patching the line.** Sweep the diff for every occurrence of that pattern and fix them together. Three reactive single-line fixes for the same rule is a smell — it means the rule was not internalized.
- After a fix, **run the full lint locally** (eg `pnpm run lint`, `npm run lint`, `cargo clippy`) before pushing. A passing local lint costs <1 minute; a failed CI cycle costs much more and burns reviewer/bot time.
- Treat CI failures as a signal to look at the rule, not just the line. The line is downstream; the rule (and the codebase's stance on it) is the upstream.

## Notes

- This applies to every repository unless the user explicitly opts out for a one-off task.
- The discipline matters most in unfamiliar codebases. In a codebase you've worked in for months, the conventions are internalized; in a fresh one, they have to be discovered explicitly.
- See also: [[pr_workflow_global]] for the PR mechanics this discipline supports.
