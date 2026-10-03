---
name: vfx-create-branch
description: Use when starting a change that should not land directly on the current branch, when the user asks for a new branch, or when work has to run isolated in a separate working tree, such as a parallel task, an experiment or a delegated implementer, before editing files. Not for merging or closing a finished branch, which is `finish-branch`.
---

# Criar branch

Confira status, branch atual e referências necessárias. Preserve alterações rastreadas e não rastreadas; stash, reset, clean e mudança de upstream ficam fora deste passo.

Nomeie pela convenção do repositório; sem convenção, use `<tipo>/<assunto-em-kebab-case>`, com o tipo tirado da mudança, como `feat` ou `fix`.

**Branch no lugar** é o padrão: crie a partir da referência pedida e troque para ela.

**Worktree** quando o trabalho precisa correr isolado do checkout atual, como tarefa paralela, experimento ou implementador delegado:

1. Veja se já está isolado: `git rev-parse --git-dir` diferente de `git rev-parse --git-common-dir` significa que você já está numa worktree; trabalhe nela.
2. Prefira a ferramenta de worktree do próprio ambiente. Sem ela, crie com `git worktree add <pasta> -b <branch>`, numa pasta que o git ignora; se o `.gitignore` não a cobre, proponha a linha ao usuário.
3. Instale as dependências e rode `verify-change` na worktree nova. Começar de uma linha de base verde separa falha preexistente de falha sua.

Relate a branch, a pasta quando houver worktree e o resultado da verificação inicial.

Remova só worktree criada por você, depois que o trabalho foi integrado ou descartado a pedido do usuário, com `git worktree remove` sem forçar. Worktree com alteração não commitada fica e é relatada.
