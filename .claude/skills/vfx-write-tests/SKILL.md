---
name: vfx-write-tests
description: Use when a behavior change, a fixed defect or the acceptance criteria of a spec have no test yet, or tests have to be added or rewritten, before the change is called complete. Not for the full test-first cycle of a new feature, which is `implement-task`.
model: sonnet
---

# Escrever testes

Localize os testes existentes e siga a convenção local de nome, lugar e execução. Cada teste verifica comportamento observável: entrada, saída e efeito. Teste que espelha a implementação linha a linha, ou que congela texto e estilo, quebra na primeira refatoração sem apontar defeito nenhum. Nomeie cada teste pela regra que ele protege, com os termos do domínio.

**Regressão.** Escreva primeiro o teste que reproduz o defeito e execute-o: teste que já passa antes da correção não cobre o defeito. Corrija em seguida e execute de novo para ver o resultado mudar.

**Comportamento novo.** O teste vem antes do código de produção, no ciclo de `implement-task`. Cubra o caso principal e as bordas que o código realmente trata. Regra de domínio se testa no domínio, sem banco nem HTTP.

**Critérios de aceite.** Parta dos critérios da especificação; critério que não é verificável volta ao usuário antes de virar teste. Para cada um, use a fronteira mais externa que o repositório já testa, como rota HTTP, fluxo de interface ou comando, com o executor de `config.pilha` ou o dos testes existentes. Um teste por critério, nomeado pelo critério, no formato dado/quando/então, com o estado preparado pelo caminho do usuário ou por dados de semente declarados. Execute e relate a tabela de rastreio:

| Critério | Teste | Resultado |
| --- | --- | --- |

Critério sem teste fica como não demonstrado, com o motivo. Critério com teste falhando é defeito: siga `debug-defect`.
