---
name: git-merge
description: Preparar ou executar integração de branches e resolver conflitos neste repositório Quarto, preservando fontes e saídas publicadas. Use quando o usuário pedir merge ou integração; não implica push, deploy ou rebase.
---

# Merge

Siga a seção de merge em [CONTRIBUTING.md](../../../CONTRIBUTING.md).

1. Confirme a branch de origem, o destino e se o pedido autoriza executar ou apenas revisar a integração. Inspecione o estado, commits e diffs; não suponha nomes ou atualidade das referências remotas.
2. Proteja mudanças preexistentes. Use uma árvore limpa ou worktree isolado, sem stash automático. Prefira fast-forward quando possível; em divergência, preserve histórico com merge normal, salvo orientação diferente.
3. Em conflitos, examine ambos os lados e resolva as fontes antes de regenerar `docs/`. Não trate `_freeze/` como cache descartável. Não resolva tudo com ours/theirs sem avaliação.
4. Verifique `git ls-files -u`, revise o diff combinado e execute a validação proporcional descrita no guia. Em merge com conflitos, faça essas verificações antes de concluir o commit de merge.
5. Confirme o histórico resultante e o estado de trabalho. Se apenas preparou a integração, não a conclua sem autorização já presente. Relate decisões de conflito relevantes e limitações dos testes.

Não faça push, exclusão de branches, rebase ou force-push por consequência do merge. Não aborte operações iniciadas por outra pessoa. Siga a regra de preview de `AGENTS.md`.
