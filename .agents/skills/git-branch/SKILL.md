---
name: git-branch
description: Create, switch, or prepare Git branches with safe naming and a clean understanding of the current repository state. Use when starting isolated work or when the user asks to create or change branches; do not use for commits alone.
---

# Git Branch

Create branches without discarding or silently relocating the user's work.

Use the repository conventions in [CONTRIBUTING.md](../../../CONTRIBUTING.md). Default to `codex/<purpose>`; do not introduce a different type prefix unless requested. Creating branches does not imply committing, merging, or publishing.

## Workflow

1. Inspect `git status --short`, the current branch, and the available local branches. Fetch remote refs only when freshness matters and network access is authorized.
2. Preserve every existing tracked and untracked change. If switching would overwrite work, stop and explain the exact conflict instead of stashing, resetting, or cleaning automatically.
3. Choose the starting point from the user's request. When it is unspecified, branch from the current checked-out state. Do not guess a remote branch or rewrite history.
4. Use a short lowercase kebab-case name with the default `codex/` prefix, unless the user supplied a different name or convention.
5. Before creating the branch, verify that the exact name does not already exist locally or remotely. If it exists, switch to it only when that clearly matches the request; otherwise ask for a different name.
6. Create and switch with a non-destructive command such as `git switch -c <name> [<start-point>]`. Confirm the resulting branch and report whether pre-existing changes remained in the working tree.

Never delete, force-reset, force-push, rename a shared branch, or change upstream configuration unless the user explicitly requests that operation and the exact target has been verified.
