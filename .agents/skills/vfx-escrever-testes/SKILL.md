---
name: vfx-escrever-testes
description: Use when a behavior change or a repaired defect still has no coverage, before treating the change as complete.
---

# Escrever testes

Localize os testes existentes e siga a convenção local de nome, lugar e execução.

Para uma regressão, escreva primeiro o teste que reproduz o defeito e execute-o: teste que já passa antes da correção não cobre o defeito. Corrija em seguida e execute de novo para ver o resultado mudar.

Para comportamento novo, cubra o caso principal e as bordas que o código realmente trata.

Cada teste verifica comportamento observável: entrada, saída e efeito. Teste que espelha a implementação linha a linha, ou que congela texto e estilo, quebra na primeira refatoração sem apontar defeito nenhum.
