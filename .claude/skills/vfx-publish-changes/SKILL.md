---
name: vfx-publish-changes
description: Use when the user explicitly asks to publish, push, ship or merge the current branch into origin main; the explicit request is the push and merge authorization the other git skills require. Not for opening a pull request, which is `open-pull-request`.
---

# Publicar alterações

Abra esta skill só diante de pedido explícito do usuário para publicar, feito neste turno ou em resposta a uma pergunta sua. O pedido é a autorização de push e merge que `create-commit` e `finish-branch` exigem, e não cobre reescrita de histórico, force-push, tag nem release. Sem o pedido, pare e pergunte.

O alcance vem do pedido. Pedido de push, sem mais, envia a branch atual e para. A integração em `main` só acontece quando o pedido fala em publicar, integrar, fazer merge ou em `main`; na dúvida, envie a branch e pergunte sobre a integração.

Confirme a branch atual, o remoto `origin` e o destino `main`. Execute `verify-change` antes de commitar e não prossiga com verificação falhando sem decisão do usuário. Aplique `create-commit` para as alterações pendentes, tratando mudanças preexistentes como do usuário.

O que vai para `origin/main` passa antes pela revisão independente: aplique `review-scope` ao que ainda não está lá, despache os agents que a classe de risco pede e trate o retorno com `address-review`. Achado de severidade alta ou crítica que continue aberto interrompe a publicação e vai ao usuário. Publicar sem essa revisão é decisão do usuário, e o relato diz que ela não foi feita.

Publique a branch com `git push` no upstream correspondente. Integre em `main` seguindo `finish-branch`: atualize a referência remota, prefira fast-forward e resolva conflitos em fontes antes de derivados. Envie o resultado para `origin/main` apenas com a integração validada localmente.

Interrompa e relate quando faltar remoto, o push for rejeitado, houver conflito ou a branch já estiver divergente de `origin/main`. Relate apenas fatos verificáveis: commits criados, branch publicada, estratégia de merge e estado final de `origin/main`.
