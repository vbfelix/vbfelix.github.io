---
name: git-merge
description: Integra a branch atual à main e faz push da main para origin/main neste repositório Quarto, preservando fontes e saídas publicadas. Use para executar merge e envio à main remota; respeite pedidos de apenas revisão, outro destino ou sem push.
---

# Merge na main e push

A execução desta skill tem como fluxo padrão integrar a branch atual à `main` local e enviar o resultado para `origin/main`. Não peça novamente o destino ou autorização de push quando o usuário solicitar esse fluxo. Um pedido para editar a própria skill altera suas instruções, sem executar operações Git por si só.

Pedidos explícitos de apenas preparar, revisar, usar outro destino ou não fazer push prevalecem sobre o padrão. Siga [CONTRIBUTING.md](../../../CONTRIBUTING.md) para preservar as fontes e escolher verificações proporcionais. Use as skills locais `git-branch` e `git-commit` quando essas operações forem necessárias.

## Preparar a integração

1. Confira `git status --short`, a branch atual, os commits e diffs pertinentes e a URL de `origin`. A origem padrão é a branch atual; o destino padrão é `main`, com envio para `origin/main`. Não crie nem altere remotos para contornar uma configuração ausente ou inesperada.
2. Atualize as referências com `git fetch origin` e compare a origem, `main` e `origin/main`. Não trate referências locais antigas como confirmação do estado remoto. Se houver falha de acesso, informe o bloqueio sem afirmar que houve envio.
3. Preserve mudanças preexistentes. Se o trabalho solicitado ainda não tiver commit, revise e registre apenas as alterações desse trabalho, incluindo suas saídas necessárias em `docs/`. Não inclua arquivos alheios nem use stash automático para obter uma árvore limpa. Use worktree isolado quando necessário para proteger trabalho não relacionado.
4. Se já estiver na `main`, confira se a integração já ocorreu e avance para o envio dos commits pendentes. Não crie um merge artificial da `main` consigo mesma. Se a `main` local não existir, use `origin/main` como base quando disponível; só peça esclarecimento se não houver uma base inequívoca.

## Integrar e validar

5. Em uma árvore limpa, integre primeiro os commits de `origin/main` à `main` local, se existirem, e depois a branch de origem. Prefira fast-forward quando possível. Em divergência, preserve o histórico com merge normal, salvo pedido explícito diferente.
6. Resolva conflitos nas fontes antes de regenerar `docs/`. Preserve `_freeze/` e os endereços públicos; não aceite um lado inteiro sem examinar o conteúdo perdido. Não aborte operações iniciadas por outra pessoa.
7. Verifique `git ls-files -u`, revise o resultado combinado e execute a validação proporcional do site. Registre a saída gerada correspondente antes do envio. Em merge com conflitos, faça essas verificações antes de concluir o commit de merge.

## Enviar para a main remota

8. Com a integração validada, execute `git push origin main`, salvo restrição explícita do usuário. Se não houver commits pendentes, confirme que as duas referências já coincidem.
9. Se o push for rejeitado porque a remota avançou, faça novo fetch, examine e integre os novos commits, valide as partes afetadas e tente o push normal novamente. Se a mesma concorrência persistir, informe a situação em vez de repetir indefinidamente. Se uma regra exigir pull request ou faltar permissão, não burle a proteção nem altere suas regras; informe a exigência e prepare a integração pelo caminho permitido.
10. Confira o hash de `main` contra `git ls-remote origin refs/heads/main`, a branch final e o estado de trabalho. Diferencie merge local concluído de push confirmado. O push não comprova que o GitHub Pages terminou a publicação.

Não faça force-push, rebase, exclusão de branches ou alterações nas proteções do repositório por consequência deste fluxo. Siga a regra de preview de `AGENTS.md` e informe os commits enviados, conflitos resolvidos e qualquer pendência real.
