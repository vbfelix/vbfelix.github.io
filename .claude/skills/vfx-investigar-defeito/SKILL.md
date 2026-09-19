---
name: vfx-investigar-defeito
description: Use when a bug, failing test or unexpected behavior shows up, before proposing or applying a fix.
---

# Investigar defeito

Reproduza o defeito antes de propor correção: registre o comando, a entrada, a saída observada e em que ponto ela diverge do esperado. Defeito que não reproduz é hipótese sobre o relato, não sobre o código.

Vá do sintoma até a origem. Cada passo confirma onde o valor já está errado e onde ainda está correto, estreitando o intervalo até o ponto que o produz. Leitura e execução sustentam a conclusão; hipótese sem confirmação continua hipótese e é relatada como tal.

Corrija a origem identificada. Quando a correção ficar em outra camada — contorno, valor padrão, checagem nova —, diga qual é a origem e por que a correção fica fora dela.

Cubra o defeito com `escrever-testes` e confirme com `verificar-alteracao` que o teste falhava antes e passa depois.
