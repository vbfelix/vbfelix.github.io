---
name: vfx-publish-changes
description: Use when the user explicitly asks to publish, push, ship or merge the current branch into origin main; invoking it is the push and merge authorization the other git skills require.
disable-model-invocation: true
model: sonnet
---

# Publicar alterações

Acionar esta skill é a autorização explícita de push e merge que `create-commit` e `finish-branch` exigem, e não autoriza reescrita de histórico, force-push, tag nem release.

Confirme a branch atual, o remoto `origin` e o destino `main`. Execute `verify-change` antes de commitar e não prossiga com verificação falhando sem decisão do usuário. Aplique `create-commit` para as alterações pendentes, tratando mudanças preexistentes como do usuário. Publique a branch com `git push` no upstream correspondente. Integre em `main` seguindo `finish-branch`: atualize a referência remota, prefira fast-forward e resolva conflitos em fontes antes de derivados. Envie o resultado para `origin/main` apenas com a integração validada localmente.

Interrompa e relate quando faltar remoto, o push for rejeitado, houver conflito ou a branch já estiver divergente de `origin/main`. Relate apenas fatos verificáveis: commits criados, branch publicada, estratégia de merge e estado final de `origin/main`.
