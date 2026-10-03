---
name: vfx-finish-branch
description: 'Use when work on a branch is done and the branch has to be closed: merged locally into its target, turned into a pull request or kept as is. Not for pushing to origin main, which is `publish-changes`.'
---

# Integrar branch

Confirme origem, destino e autorização. Rode `verify-change` na branch antes de integrar.

Quando o usuário não disse como fechar a branch, ofereça as três saídas e espere a escolha: integrar localmente no destino, abrir um pull request com `open-pull-request`, ou manter a branch como está. Descartar a branch só entra na conversa quando o usuário pede.

Para integrar localmente: atualize as referências, prefira fast-forward, preserve mudanças alheias e resolva conflitos em fontes antes de derivados. Depois do merge, rode `verify-change` de novo no destino: duas branches verdes podem produzir um resultado vermelho.

Remova a branch e a worktree dela só depois da integração verificada, e só as que você criou. Não reescreva histórico nem publique sem pedido explícito.
