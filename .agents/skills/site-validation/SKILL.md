---
name: site-validation
description: Validar renderização, navegação, recursos, dados para agentes e preview deste site Quarto após alterações ou antes de uma entrega. Use para conferência do site; não implica publicação.
---

# Validação do site

A skill global `vfx-verificar-alteracao` executa os verificadores declarados em `config.verificar-alteracao`
do `recursos.yaml` e relata comandos, resultado e checagens indisponíveis. Esta skill descreve o que conferir
neste site além desses comandos. Use [CONTRIBUTING.md](../../../CONTRIBUTING.md) para escolher verificações
proporcionais.

- Confira as fontes e o escopo antes de renderizar. Preserve `docs/`, `_freeze/` e redirecionamentos. Não limpe caches computacionais para resolver problemas de preview.
- Use `scripts/site.ps1 -Action render` para alterações globais. Para alterações isoladas, renderize as páginas afetadas, execute `scripts/check-site.py` e `quarto run scripts/build-agent-assets.ts --check`.
- A conferência de arquivos não substitui testes de comportamento dos scripts. Consulte a skill `site-tests` quando houver mudanças neles.
- Abra as páginas afetadas e confira o comportamento visível pertinente, como navegação, thumbnails ou links do currículo. Para CSS, confira também uma largura estreita quando o layout for afetado.
- Teste o conteúdo servido, não apenas o arquivo em disco. Confirme HTTP 200 da home e dos recursos alterados; para texto, confira Content-Type e acentos.
- Reutilize o preview válido ou inicie pelo fluxo documentado. Identifique os processos da tarefa antes de encerrar servidores. Se uma checagem falhar, corrija a causa antes de apresentar o resultado como validado.
- Siga a regra de atualidade do preview em `AGENTS.md`: para alterações no site, encerre o preview da tarefa antes de renderizar e reinicie depois. Confira na URL pública o conteúdo alterado, comparando-o com a saída atual em `docs/`. HTTP 200 sozinho não comprova que a versão está atualizada.

Ao concluir, abra a home conforme `AGENTS.md` e informe as verificações efetivamente executadas. Não prometa validação de navegadores, links externos ou mecanismos de busca que não foram testados.
