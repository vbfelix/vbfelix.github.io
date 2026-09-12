---
name: site-tests
description: Criar ou executar testes dos scripts e geradores deste site Quarto, especialmente regressões em metadados, datas, encoding e currículos. Use para mudanças de comportamento ou pedidos de testes; não exija testes novos para simples ajustes editoriais ou visuais.
---

# Testes do site

Leia a matriz de verificações em [CONTRIBUTING.md](../../../CONTRIBUTING.md).

1. Identifique o comportamento afetado e os testes existentes em `scripts/test_*.py`. Execute os testes pertinentes com o Python disponível, sem assumir um caminho pessoal instalado.
2. Para corrigir regressões, cubra o caso que falhava e um caso válido relevante. Use diretórios temporários em testes que escrevem arquivos; não sobrescreva os dados do site.
3. Para geradores, confira saída válida e sincronização, incluindo `quarto run scripts/build-agent-assets.ts --check`. Não substitua esse comando por comparações de títulos ou snapshots extensos de HTML.
4. Para encoding, confira os bytes e a resposta HTTP: Markdown/texto com UTF-8 reconhecível e JSON sem BOM. Para currículos, preserve a precisão mensal e no máximo três linhas de descrição por cargo.
5. Execute os testes alterados e os testes relacionados. Amplie o escopo apenas se houver dependências, falhas ou riscos concretos.

Reporte o comando e seu resultado real, distinguindo testes unitários, validação de arquivos e conferência visual. Siga `AGENTS.md` para o preview ao encerrar.
