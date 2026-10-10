---
name: vfx-debug-defect
description: Use when a bug, failing test, error message or unexpected behavior appears and its cause is not yet known, before proposing or applying any fix. Not for an outage in a deployed environment, which is `handle-incident`, nor for a failing CI run, which is `debug-ci`.
---

# Investigar defeito

Reproduza o defeito antes de propor correção: registre o comando, a entrada, a saída observada e em que ponto ela diverge do esperado. Defeito que não reproduz é hipótese sobre o relato, não sobre o código.

Vá do sintoma até a origem. Cada passo confirma onde o valor já está errado e onde ainda está correto, estreitando o intervalo até o ponto que o produz. Leitura e execução sustentam a conclusão; hipótese sem confirmação continua hipótese e é relatada como tal.

Corrija a origem identificada. Quando a correção ficar em outra camada — contorno, valor padrão, checagem nova —, diga qual é a origem e por que a correção fica fora dela.

Conte as correções tentadas. Na terceira que não resolve, pare: correção que revela problema novo em outro ponto indica defeito de desenho, e mais uma hipótese não o alcança. Leve ao usuário o que cada tentativa mostrou.

Cubra o defeito com `write-tests` e confirme com `verify-change` que o teste falhava antes e passa depois.

Depois cubra a classe do erro, além do caso relatado. Nomeie a regra que foi violada e liste as entradas vizinhas que a quebrariam pela mesma causa: a outra borda do mesmo intervalo, o valor vazio ou ausente, os demais chamadores da origem corrigida. Cada vizinha vira um teste na camada em que a origem está; teste só no sintoma deixa a mesma origem falhar por outro caminho. Vizinha que falha depois da correção é defeito novo, relatado e investigado do início. Vizinha que ficou sem teste entra no relato, com o motivo.
