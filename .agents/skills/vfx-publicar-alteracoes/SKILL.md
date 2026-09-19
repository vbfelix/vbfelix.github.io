---
name: vfx-publicar-alteracoes
description: Use when the user asks to publish, push, ship or integrate the current branch into origin main; invoking it is the explicit push and merge authorization the other git skills require.
---

# Publicar alterações

Acionar esta skill é a autorização explícita de push e merge que `criar-commit` e `integrar-branch` exigem, e não autoriza reescrita de histórico, force-push, tag nem release.

Confirme a branch atual, o remoto `origin` e o destino `main`. Execute `verificar-alteracao` antes de commitar e não prossiga com verificação falhando sem decisão do usuário. Aplique `criar-commit` para as alterações pendentes, tratando mudanças preexistentes como do usuário. Publique a branch com `git push` no upstream correspondente. Integre em `main` seguindo `integrar-branch`: atualize a referência remota, prefira fast-forward e resolva conflitos em fontes antes de derivados. Envie o resultado para `origin/main` apenas com a integração validada localmente.

Interrompa e relate quando faltar remoto, o push for rejeitado, houver conflito ou a branch já estiver divergente de `origin/main`. Relate apenas fatos verificáveis: commits criados, branch publicada, estratégia de merge e estado final de `origin/main`.
