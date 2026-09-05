---
name: git-commit
description: Prepare and create focused Git commits from reviewed working-tree changes with clear messages and proportional verification. Use when the user asks to commit, stage, or organize completed changes; do not push unless separately requested.
---

# Git Commit

Create reviewable commits while preserving unrelated user work.

## Workflow

1. Inspect `git status --short`, the active branch, and diffs for tracked and staged files. Treat all pre-existing changes as user-owned unless the current task clearly created them.
2. Group only one coherent purpose per commit. Exclude unrelated files, generated noise, secrets, credentials, local environment files, and temporary artifacts. Never stage everything blindly in a mixed working tree.
3. Review the exact staged diff with `git diff --cached` before committing. If staged content includes unrelated or suspicious changes, adjust the staging set without discarding the underlying files.
4. Run checks proportional to the change. Prefer the project's documented tests, build, formatting, or validation commands; report checks that could not run.
5. Write an imperative subject that explains the outcome, normally under 72 characters. Follow the repository's established convention when visible; otherwise use Conventional Commits such as `feat:`, `fix:`, `docs:`, `refactor:`, `test:`, or `chore:`. Add a body only when motivation, tradeoffs, or migration details are useful.
6. Commit without amending or bypassing hooks unless the user explicitly requests it. Afterward, verify the new commit summary and remaining working-tree changes.

Committing does not authorize pushing, opening a pull request, rewriting history, or including unrelated work. Never use `--no-verify`, force options, destructive cleanup, or automated co-author attribution unless explicitly requested.
