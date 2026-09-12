# Project workflow

## Portfolio thumbnails

This repository owns portfolio thumbnail creation. When incorporating or substantially revising an article, follow [maintenance/design/portfolio-thumbnails.md](maintenance/design/portfolio-thumbnails.md): read the article, use its concrete subjects and methods, and create a composition in the site's green chalkboard identity. Keep artwork local; do not import or overwrite `thumbnail.*` or `_metadata.yml` from ghost-writer. Validate the rendered card and accessible description before completing the article integration.

## Guidelines and task-specific skills

Use [CONTRIBUTING.md](CONTRIBUTING.md) for source ownership, proportional checks and Git integration conventions.

- Tests and script regressions: [.agents/skills/site-tests/SKILL.md](.agents/skills/site-tests/SKILL.md).
- Rendering, navigation and preview validation: [.agents/skills/site-validation/SKILL.md](.agents/skills/site-validation/SKILL.md).
- Branches: [.agents/skills/git-branch/SKILL.md](.agents/skills/git-branch/SKILL.md).
- Commits: [.agents/skills/git-commit/SKILL.md](.agents/skills/git-commit/SKILL.md).
- Merges and conflicts: [.agents/skills/git-merge/SKILL.md](.agents/skills/git-merge/SKILL.md).

Load the relevant skill for the requested operation. These instructions do not authorize Git mutations, push or deployment on their own. Preserve existing user authorization without asking for it again.

## Mandatory site preview

### Single content source

Keep one editable source for each piece of content. Home and About share `_content/header-about.qmd`; their work tables and résumé summaries use `_content/experience-table.qmd`. Generated HTML, Markdown and JSON must derive from those sources, following the public pages' include graph. Do not fix discrepancies by manually copying text between routes or editing `docs/`. Run `quarto run scripts/test-content-source.ts` after changes to content routing or generators. Academic records and detailed career narrative remain in `_content/header-experience.qmd`; publication records remain in their own canonical pages, and article catalogs derive from article metadata.

After completing any user request in this repository, generate or refresh the local Quarto preview before sending the final response.

1. Make sure the requested changes are complete and the site renders successfully.
2. Start or reuse the Quarto preview server.
3. Confirm that the local homepage responds successfully.
4. Open the homepage preview in the Codex browser so the user can inspect the result.

### Preview freshness is mandatory

After every completed user request, refresh the local preview and verify that it serves the current files before the final response. A successful render, HTTP 200, or opening a tab alone does not establish freshness.

- For site changes, stop the Quarto preview process owned by this task before rendering; a running preview can restore stale HTML from its in-memory state. Render the affected pages (the whole site for global changes), then restart the preview. Preserve other tasks' processes.
- For documentation-only changes, refresh and verify the latest valid preview; do not rerender unchanged pages unnecessarily.
- Check the public preview URL used by the user, including any local proxy. Verify a concrete marker from the change in the served HTML or resource: the new image path, updated text, navigation entry, or expected file bytes. Also check that the replaced marker is absent when applicable.
- Compare the relevant response with the current generated file in `docs/`. Account for legitimate preview-injected scripts; do not require byte-identical HTML when the server adds live-reload code. For unchanged pages, compare stable content against the latest valid output.
- If old content is served, identify and restart the stale task-owned server, regenerate the affected output if needed, and repeat the HTTP/content checks. Do not merely ask the user to clear the browser cache or add a cache-busting URL as a substitute for fixing stale server output.
- Open or reload the homepage only after these checks pass. Report freshness checks accurately and disclose any verification that could not be completed.

Do not open an incomplete or broken preview. If the task does not change the site, still open the latest valid preview. Skip this step only when the user explicitly asks not to open a preview or when the environment cannot provide one; in that case, state the reason clearly.

The project is a Quarto website whose generated output is written to `docs/`. Preserve the existing Quarto structure and GitHub Pages output unless the user requests a different architecture.
