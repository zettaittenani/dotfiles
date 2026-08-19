# Worktree Workflow (applies to ALL repositories)

Work that needs its own branch happens in a **git worktree**, not by switching branches inside the main checkout.

Why: the main checkout stays on the default branch, so a running build / dev server / editor session there is never invalidated by a branch switch, and several branches (or several agents) can be in flight at once without fighting over one working tree.

## Rule

- Do **not** run `git checkout -b` / `git switch -c` in the main checkout to start work. Create a worktree instead.
- One branch = one worktree = one directory. Never reuse a worktree for an unrelated branch.
- The main checkout stays on the default branch (`master` / `main`) and is used for `pull` / review / inspection only.
- Exceptions where a worktree is unnecessary: read-only investigation, and changes committed directly to the default branch in repos where that is the accepted flow.

## Creating a worktree

Preferred, when the user drives it from tmux (`workmux` is installed by `mac_install.sh`):

```bash
workmux add <branch>          # worktree + tmux window in one step
workmux add <branch> --base master
workmux ls                    # list active worktrees
```

From a plain shell or an agent session:

```bash
git fetch origin
git worktree add ../<repo>-<branch> -b <branch> origin/<default-branch>
```

Keep worktrees as siblings of the main checkout (`../<repo>-<branch>`), not nested inside it, so watchers and glob-based tooling in the main checkout do not pick them up.

For a Claude Code subagent that edits files in parallel with others, `isolation: "worktree"` gives it its own worktree automatically.

## After creating one

Untracked and gitignored files are **not** carried into a new worktree. Bootstrap the environment before building: `.env` and friends have to be copied (or declared as file operations in `.workmux.yaml`), and dependency installs (`npm ci`, `pnpm i`, `bundle install`, ...) have to be re-run.

## Cleanup

Remove the worktree once the branch is merged or abandoned, so `git worktree list` stays readable:

```bash
workmux merge                 # merge, then drop worktree + tmux window + branch
workmux rm <branch>           # drop without merging

git worktree remove ../<repo>-<branch>
git branch -D <branch>
git worktree prune            # after a manual directory delete
```

## Relation to the PR workflow

In `PR_WORKFLOW.md`, step 1 ("create a feature branch off the default branch") means **create a worktree carrying that branch**. Every later step (empty first commit, fine-grained commits, push, draft PR, bot review handling, merge) runs inside that worktree. Step 9 ("return to the default branch and pull") is done in the main checkout, and the worktree is removed as part of it.

See also: [[pr_workflow_global]], [[coding_discipline_global]].
