---
name: site-tests
description: Criar ou executar testes dos scripts e geradores deste site Quarto, especialmente regressões em metadados, datas, encoding e currículos. Use para mudanças de comportamento ou pedidos de testes; não exija testes novos para simples ajustes editoriais ou visuais.
---

# Testes do site

A skill global `vfx-write-tests` define o critério geral: seguir a convenção local, cobrir o caso que
falhava e um caso válido relacionado em uma regressão, e não criar testes que apenas espelham a
implementação ou congelam texto e estilo. Esta skill acrescenta os alvos e as verificações deste site.
Leia a matriz de verificações em [CONTRIBUTING.md](../../../CONTRIBUTING.md).

Os testes existentes são congelados (veja `AGENTS.md`): não edite, renomeie nem apague `scripts/test_*.py` ou `scripts/test-*.ts` para acomodar uma alteração. Comportamento novo ganha um arquivo de teste novo; teste existente que falha indica defeito no código. Mudar um teste exige pedido explícito do usuário e um commit só dele, com `SITE_ALLOW_TEST_EDIT=1`. Esta regra prevalece sobre a `vfx-write-tests`.

1. Identifique o comportamento afetado e os testes existentes em `scripts/test_*.py`. Execute os testes pertinentes com o Python disponível, sem assumir um caminho pessoal instalado.
2. Use diretórios temporários em testes que escrevem arquivos; não sobrescreva os dados do site.
3. Para geradores, confira saída válida e sincronização, incluindo `quarto run scripts/build-agent-assets.ts --check`. Não substitua esse comando por comparações de títulos ou snapshots extensos de HTML.
4. Para encoding, confira os bytes e a resposta HTTP: Markdown/texto com UTF-8 reconhecível e JSON sem BOM. Para currículos, preserve a precisão mensal e no máximo três linhas de descrição por cargo.
5. Execute os testes alterados e os testes relacionados. Amplie o escopo apenas se houver dependências, falhas ou riscos concretos.

Reporte o comando e seu resultado real, distinguindo testes unitários, validação de arquivos e conferência visual. Siga `AGENTS.md` para o preview ao encerrar.
