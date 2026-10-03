---
name: vfx-verify-change
description: Use when a change is about to be committed, reviewed or reported as working and the checks the repository declares, such as tests, lint, types and build, have not run since the last edit. Not for checking a screen in a browser, which is `verify-ui`.
model: sonnet
---

# Verificar alteração

Leia `config.verify-change`. Execute apenas os comandos declarados, no diretório relativo e no timeout configurados. Sem verificação declarada, relate a ausência em vez de escolher um comando por conta própria.

Relate cada comando na forma: comando executado, resultado (`passou`, `falhou` ou `indisponivel`) e a linha da saída que sustenta o resultado. Comando não executado é `indisponivel`.

A afirmação sobre o estado da alteração vem da saída obtida depois da última edição. Saída anterior à edição não sustenta a afirmação: execute de novo antes de relatar.

Com verificação falhando, relate a falha e pare. Seguir mesmo assim é decisão do usuário.
