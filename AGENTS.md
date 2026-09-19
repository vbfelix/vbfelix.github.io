# Project workflow

## Portfolio thumbnails

This repository owns portfolio thumbnail creation. When incorporating or substantially revising an article, follow [maintenance/design/portfolio-thumbnails.md](maintenance/design/portfolio-thumbnails.md): read the article, use its concrete subjects and methods, and create a composition in the site's green chalkboard identity. Keep artwork local; do not import or overwrite `thumbnail.*` or `_metadata.yml` from ghost-writer. Validate the rendered card and accessible description before completing the article integration.

## Guidelines and task-specific skills

Use [CONTRIBUTING.md](CONTRIBUTING.md) for source ownership, proportional checks and Git integration conventions.

Reusable skills come from the `vfx-llm-skills` library and are not maintained here. [recursos.yaml](recursos.yaml) declares the source, the pinned `ref`, the selected skills and this project's verification commands. The library's CLI writes the managed copies under `.vfx-llm-skills/gerado/` and installs the platform bridges as `vfx-`-prefixed skills in `.agents/skills/` for Codex and `.claude/skills/` for Claude Code. Do not edit those files; change `recursos.yaml` or the library instead.

Global skills:

- Branches: `vfx-criar-branch`.
- Commits: `vfx-criar-commit`.
- Branch integration: `vfx-integrar-branch`.
- Test authoring: `vfx-escrever-testes`.
- Content writing contract: `vfx-escrever-conteudo`.
- Declared verification commands: `vfx-verificar-alteracao`.
- Defect investigation before fixing: `vfx-investigar-defeito`.
- Push and merge into `origin/main` on explicit request: `vfx-publicar-alteracoes`.

Skills specific to this site, kept locally on top of the global ones:

- Site copy in Vinícius's voice: [.agents/skills/ghost-writer/SKILL.md](.agents/skills/ghost-writer/SKILL.md).
- Tests and script regressions: [.agents/skills/site-tests/SKILL.md](.agents/skills/site-tests/SKILL.md).
- Rendering, navigation and preview validation: [.agents/skills/site-validation/SKILL.md](.agents/skills/site-validation/SKILL.md).
- Unified local commands and preview: [.agents/skills/site-workflow/SKILL.md](.agents/skills/site-workflow/SKILL.md).

Merging into `main` and pushing to `origin/main` is `vfx-publicar-alteracoes`, which follows `vfx-integrar-branch`. The site-specific requirements it depends on — preserving `_freeze/`, committing the matching `docs/` output, regenerating `docs/` when a merge touches sources — live in [CONTRIBUTING.md](CONTRIBUTING.md).

To refresh the managed copies after changing `recursos.yaml` or the pinned `ref`, run the library CLI for each platform:

```powershell
python <fonte>/scripts/resources.py instalar --repositorio . --plataforma codex
python <fonte>/scripts/resources.py instalar --repositorio . --plataforma claude-code
```

`<fonte>` is the `source.path` recorded in `recursos.yaml`. Both commands are idempotent and can be repeated in any order; the CLI tracks state per platform and replaces a bridge it generated itself. It still refuses to overwrite a bridge that was edited by hand, which is the intended protection: restore the file or remove it, then install again.

`recursos.lock.json` records the resolved commit for the pinned `ref`. Version it together with `recursos.yaml` and the `vfx-*` bridges; `.vfx-llm-skills/` is generated and ignored by Git.

The library also ships hooks (`session-start`, `validate-changes`, `session-end`). This repository does not enable them: it already owns a `Stop` hook in [.codex/hooks.json](.codex/hooks.json) that validates the local AI configuration. Enabling the library's hooks would mean trusting `.codex/vfx-llm-hooks.json` in Codex and composing the same descriptors into Claude Code settings by hand, so add them to `use.hooks` only when that composition is actually wanted.

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

Use `./scripts/site.ps1 -Action preview -Refresh auto` as the default final command. It resolves the bundled Python and Quarto installations, refreshes stale output, owns its preview processes and confirms that the served home matches `docs/index.html`.

The project is a Quarto website whose generated output is written to `docs/`. Preserve the existing Quarto structure and GitHub Pages output unless the user requests a different architecture.
