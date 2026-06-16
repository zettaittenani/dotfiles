# PR Workflow (applies to ALL repositories)

When the user asks to create a PR, follow this workflow regardless of the repository.

## Language

- **All commit messages, PR titles, and PR descriptions MUST be written in English.**
- This applies regardless of the language the user is conversing in.

## Steps

1. **Create a feature branch** off the default branch.
2. **First commit MUST be an empty commit** with `--allow-empty`, whose message becomes the PR title.
   - Use a Conventional Commits prefix: `feat:`, `fix:`, `chore:`, `docs:`, `refactor:`, `test:`, etc.
   - Scope is encouraged: `feat(ghostty): ...`, `chore(tmux): ...`.
   - Example: `git commit --allow-empty -m "feat(scope): short summary"`
3. **Add real changes** in subsequent commits.
   - **Keep commits as fine-grained as possible**: one logical change per commit.
   - Prefer many small commits over one large commit; squash-merge will consolidate them on `master`.
   - Each commit message also follows Conventional Commits.
4. **Push** the branch to `origin`.
5. **Create a draft PR** with `gh pr create --draft`.
   - **Title**: same as the first empty commit message.
   - **Description**: structured (e.g. Summary / Background / Change / Verification).
6. **Mark the PR as ready for review** (`gh pr ready <num>`) without waiting for explicit approval.
7. **Handle the CodeRabbit review** (see the dedicated section below) before merging.
8. **Merge the PR** (`gh pr merge <num> --squash --delete-branch`) without waiting for explicit approval.
9. **Return to the default branch** and pull (or confirm fast-forward).

## CodeRabbit review handling

After the PR is ready, CodeRabbit posts an automated review. Address it before merging.

- Wait for the review to finish (`gh pr checks <num>` shows CodeRabbit "Review completed"). List inline comments with `gh api repos/<owner>/<repo>/pulls/<num>/comments`.
- Handle **each** inline comment individually:
  - **If you address it**: make the fix in a **separate commit** (one logical fix per comment, Conventional Commits), push, then reply inline with `Fixed: <commit URL>` via `gh api repos/<owner>/<repo>/pulls/<num>/comments/<comment_id>/replies -f body=...`.
  - **If you do NOT address it**: reply inline with a clear reason why it is intentionally skipped.
- Commit fixes to the **same PR branch** — never open a separate PR for CodeRabbit fixes.
- Inline replies may be written in the language of the review (Japanese is fine); commit messages / PR title / description stay English per the Language rule.
- After all comments are handled and CI is green again, proceed to merge.

## Notes

- This applies to every repository unless the user says otherwise for a specific case.
- If the repository has a PR template, fill it in on top of the structure above (still in English).
- If CI is configured and required, mention the status briefly after merge.
