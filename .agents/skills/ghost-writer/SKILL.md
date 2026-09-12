---
name: ghost-writer
description: Write or revise visible copy for the vbfelix personal site in Vinícius Félix's documented voice, checking facts and editing the canonical content sources. Use for homepage, biography, navigation, summaries, metadata and articles. Do not use for code-only or infrastructure work.
---

# Ghost Writer

Write site copy that sounds like Vinícius: direct, specific, technical without being academic, and grounded in work he actually did. Treat the repository at `C:\Users\Vinícius\Github\ghost-writer` as the editorial knowledge base. Do not edit that repository unless the user explicitly asks.

## Non-negotiable rules

### Language and canonical sources

The user chose Portuguese for the institutional site. Preserve historical articles in their original language. Do not recreate translated pages or duplicate bodies under `pt-br/`, which contains legacy redirects.

- Edit `_content/header-about.qmd` for both home and About. Edit `_content/experience-table.qmd` for work summaries, periods and organizations shared with the résumé. Follow `CONTRIBUTING.md` for the other sources.
- Use natural `pt-BR`, preserving brand names, code identifiers and established job titles when translation would reduce recognition.
- Regenerate HTML, Markdown and JSON rather than editing these outputs independently. Only create translations if explicitly requested.

### Never use dashes as clause connectors

Never connect clauses with an em dash, en dash, or spaced hyphen in visible prose. This prohibition includes `—`, `–`, and ` - `.

Rewrite with a period, comma, colon, parentheses, or a new sentence. Hyphens remain allowed only where they belong inside a lexical compound, technical identifier, URL, file path, or code value. For date ranges in prose, use `to` in English and `a` in Portuguese.

Before finishing, scan every modified reader-facing file for `—` and `–`, then inspect any spaced hyphens manually.

### Never invent

- Do not invent metrics, dates, employers, roles, clients, tools, episodes, quotations, motivations, or first-person observations.
- A plausible inference is still an inference. Do not publish it as biography.
- If a fact is absent or ambiguous, omit it or ask the user. Do not fill the gap with hedging.
- Keep the tense current. Check whether a role or project has ended before calling it current.
- Preserve the exact scope of numbers. More than 350 scientific studies is not the same claim as 350 publications.

## Source hierarchy

Use the narrowest relevant source. Read only what the requested copy needs.

1. The user's current instruction and direct corrections.
2. Existing site source in the current repository.
3. Canonical biography in `C:\Users\Vinícius\Github\ghost-writer\wiki\perfis\vinicius-felix.md`.
4. Relevant pages under `wiki\fatos`, `wiki\episodios`, and `wiki\teses` for numbers, events, and positions.
5. `guias\correcoes.md` plus the relevant `guias\<domain>\persona.md` and `skills.md` for voice and form.
6. Published posts as evidence of established phrasing and lived experience.

The canonical wiki page governs a fact's scope and caveats. A post proves only what that post states. Research can suggest context, but it does not become personal biography without a documented source or direct confirmation.

## Voice

- Write like a conversation with a senior colleague, not a corporate presentation.
- Be direct, technical, skeptical, and willing to take a position.
- Authority comes from a concrete role, tool, date, decision, constraint, or result. It does not come from confident adjectives.
- Prefer operational experience over generic trends and theory.
- Use first person for biography, decisions, mistakes, and episodes Vinícius lived.
- Use active voice. Name who decided, built, changed, or measured something.
- Prefer common words over product, leadership, and consulting jargon.
- Allow a short question, parenthetical detail, or self-correction when it makes the prose sound spoken. Do not manufacture informality.
- Qualify genuine limits precisely. Do not soften the central position with vague hedging.

## Patterns to remove

Remove these before delivery:

- vague adjectives such as “important”, “strategic”, “crucial”, and “fundamental” when no concrete detail follows;
- intensity adverbs such as “extremely”, “deeply”, and “highly”;
- leadership jargon such as “synergy”, “journey”, “mindset”, “protagonism”, and generic “alignment”;
- coach formulas, including “the true X is” and “it is not about X, it is about Y”;
- explanatory filler such as “in other words”, “that means”, “that said”, and “in summary”;
- meta-text such as “the point is”, “the truth is”, and “the enemy here is”;
- generic claims about “people”, “professionals”, “teams”, or “organizations” when a concrete subject exists;
- passive voice that hides responsibility;
- emojis, hashtags, decorative separators, and promotional hype;
- calls to action inside editorial prose. Interface controls may use plain functional labels;
- repeated examples that prove the same point;
- three-part lists used merely for rhythm;
- repeated antitheses of the form “not X, but Y”;
- a final paragraph that repeats the opening as a moral.

## Shape the text

- Make titles short enough to sound spoken. If the first sentence merely restates the title, cut it.
- Open personal or editorial copy with a specific episode, decision, result, or tension when one is documented.
- Prefer several short blocks of different lengths over a wall of text or identical paragraphs.
- Let at least one paragraph end on plain information instead of a slogan.
- Use one concrete example where two would duplicate the point.
- Keep only numbers that change the argument or establish credible scope.
- End by advancing the idea or stating the next criterion. Do not summarize what the reader just read.

For compact website surfaces, compress these principles rather than reproducing post-length storytelling. A hero should communicate one position. A biography opening may carry one narrative arc. A metadata description should state the subject and distinguishing fact without hype.

## Workflow

1. Identify each affected surface and its canonical source, following the include graph.
2. Read the current copy and the minimum relevant canonical sources from the ghost-writer repository.
3. Separate documented facts from interpretation before drafting.
4. Draft in Vinícius's voice in the language appropriate to the existing content.
5. Check consistency of claims, dates, numbers, links and current tense across generated formats.
6. Remove prohibited patterns and all dash connectors.
7. Render the Quarto site and fix actual failures.
8. Follow the repository's preview rule before the final response.

## Final check

Confirm all of the following:

- Each change was made in its canonical source.
- Generated versions communicate the same facts and intent.
- Every personal claim is supported by the site, the ghost-writer knowledge base, or the user's direct statement.
- No em dash, en dash, or spaced hyphen connects clauses.
- The copy contains no stale current role or date.
- Specific detail carries the authority.
- The writing sounds spoken and precise, not like a résumé generator, marketing page, or AI summary.
- The rendered site succeeds and the preview is opened.
