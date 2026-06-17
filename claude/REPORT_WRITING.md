# Report Writing (applies to ALL repositories)

When the user asks you to write a report — investigation, experiment results, eval/benchmark summary, postmortem, design retrospective — follow this structure regardless of the repository or output format (Markdown file, PR description, Slack post, chat reply).

## Mandatory TL;DR

- **Every report MUST start with a TL;DR section** at the very top, before any other heading.
- The TL;DR states the conclusion and the most important number(s) — not the methodology.
- Keep it to 2-5 short lines. A reader who reads only the TL;DR should know what happened and what to do next.

## Required sections

After the TL;DR, structure the body so the following five questions are answered in order. Section names may be adapted to the context (Japanese / English / domain-specific), but the logical flow must be preserved:

1. **背景 / Background** — Why this report exists. What problem, hypothesis, or prior result motivated the work.
2. **検証目的 / Goal** — What specific question this report answers. State it as a falsifiable claim or measurable metric when possible.
3. **実施内容 / Method** — What was actually done: scope, variants compared, data sources, sample sizes, tools/models/versions, knobs that matter.
4. **結果 / Results** — What was observed, with **concrete data**. Tables, numbers, deltas, confidence intervals, error bars — whatever the data supports. Distinguish raw observation from interpretation.
5. **今後の推奨 / Next steps** — What to do next: ship / revert / re-test / escalate. Be specific (which knob, which branch, which owner, by when).

## Concrete-data requirement

- Every claim in 結果 / Next steps must be backed by a specific number, file path, commit, or quote — not a vibe.
- Prefer tables for any comparison across ≥2 variants or ≥2 metrics.
- When numbers are noisy, report mean **and** spread (sd / min-max / iter count). A single-run number without spread is not evidence of a difference.
- Cite the raw data location (file path, run ID, dashboard URL) so a reader can verify.

## Style

- Write in the language the user is working in (Japanese by default for this user, unless the artifact is a PR title / commit / English-mandated channel — see [[pr_workflow_global]]).
- No filler. No restating the section title in the first sentence. Lead with the finding.
- Avoid hedging that adds no information ("it seems that...", "we might consider..."). If uncertain, state the uncertainty as a number or a named risk.

## Notes

- This applies to every report unless the user explicitly opts out for a specific case.
- For very short ad-hoc summaries (single-paragraph replies), the TL;DR rule still applies; the five sections may collapse into a single paragraph as long as each question is answered.
