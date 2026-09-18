---
name: site-workflow
description: Executar o fluxo local unificado deste site Quarto para diagnóstico, testes, verificação, renderização e preview. Use quando o pedido envolver preparar ou conferir o ambiente local do site; não implica commit, merge ou publicação.
---

# Fluxo local do site

Use `scripts/site.ps1` como entrada única. Estas ações são os verificadores declarados em
`config.verificar-alteracao` do `recursos.yaml` e executados pela skill global `vfx-verificar-alteracao`.
Escolha a menor ação que prove o resultado:

* `doctor` confere as ferramentas e portas.
* `test` executa testes unitários e valida a configuração de IA.
* `check` confere fontes, links, recursos e arquivos derivados.
* `render` sincroniza geradores locais e renderiza o site.
* `preview` atualiza quando necessário, inicia os servidores e verifica a home.
* `verify` escolhe testes e renderização conforme os arquivos alterados.

Para a conferência final, execute `./scripts/site.ps1 -Action preview -Refresh auto`. Abra `http://127.0.0.1:4321/` no navegador do Codex somente depois de o comando confirmar que o conteúdo servido corresponde a `docs/index.html`.

Use o subagente `code_reviewer` para mudanças de comportamento em scripts ou configuração. Use `copy_reviewer` quando houver texto visível alterado. Não delegue mudanças triviais nem envie o repositório inteiro quando o diff e as fontes relacionadas forem suficientes.
