# Desenvolvimento e validação

## Fontes e arquivos gerados

- Edite páginas institucionais em `_content/` e metadados nos `.qmd` da raiz.
- Textos do Portfólio têm fonte em `vbfelix/ghost-writer`, branch `main`, pasta `posts/portfolio`. Execute `scripts/sync-portfolio.py` para atualizar `portfolio/`; não edite cópias importadas. Versione o manifesto `portfolio-source.json`. Recursos visuais locais podem ficar junto aos artigos. Veja o fluxo e a migração pendente no README.
- Preserve `_freeze/`: contém resultados necessários para renderizar sem reexecutar análises em R.
- `docs/` é a saída versionada do GitHub Pages. Inclua no commit a saída correspondente às mudanças de fonte; não edite HTML ou currículos gerados manualmente.
- Resumos dos cargos, empresas, períodos e logotipos ficam em `_content/experience-table.qmd`. Home, experiência e currículos leem essa mesma tabela. Não crie cópias editoriais para cada formato.
- Preserve redirecionamentos em `pt-br/` e endereços públicos existentes.

## Verificações proporcionais à mudança

### Thumbnails do Portfólio

As capas são exclusivas do portfólio, em `portfolio/<slug>/`. Blogposts em `posts/` não levam thumbnail: um artigo sincronizado sem capa já está completo.

O site é responsável por criar as capas do portfólio, seguindo [a direção visual das thumbnails](maintenance/design/portfolio-thumbnails.md). Ao incorporar um artigo do portfólio, leia seu conteúdo e produza uma composição específica com elementos do tema, na identidade de lousa verde. `thumbnail.*` e `_metadata.yml` pertencem ao site e não devem ser sobrescritos pela sincronização do ghost-writer. Confira a imagem e seu texto alternativo no card renderizado antes de concluir a integração.

### Blocos de código

Bloco de código tem fundo preto e texto de alto contraste, seguindo [a direção visual dos blocos de código](maintenance/design/code-blocks.md). O fundo é `$code-block-bg` em `custom_theme.scss`, distinto de `$code-bg`, que só atinge código inline. Ao mexer no tema ou no realce de sintaxe, meça o contraste no preview sobre o fundo composto, não sobre o valor declarado.

| Mudança | Verificação |
|---|---|
| Documentação e skills | Links locais, exemplos, frontmatter e coerência com os scripts atuais; reutilize o preview válido. Para skills globais, altere `recursos.yaml` ou a biblioteca e reinstale, nunca os arquivos gerados |
| Tema, CSS ou realce de sintaxe | Meça o contraste real do bloco de código no navegador, compondo transparências; texto base e ao menos um token colorido acima de 4,5:1 |
| Conteúdo, navegação ou CSS | Renderize as páginas afetadas e confira a página no navegador; para mudanças globais, renderize o site inteiro |
| Geradores e scripts | Execute testes pertinentes, gere a saída e verifique sincronização e idempotência |
| Currículos e metadados | Confira fatos, datas, fontes, limite dos resumos, JSON válido e sincronização dos formatos |
| Preview ou codificação | Confira status HTTP, Content-Type e acentos no conteúdo servido; arquivo correto em disco não prova exibição correta |
| Merge com fontes alteradas | Valide o resultado combinado e regenere `docs/` antes de concluir |

Comandos na raiz, com Quarto e Python disponíveis:

```powershell
python -m unittest discover -s scripts -p 'test_*.py'
./scripts/site.ps1 -Action render
./scripts/site.ps1 -Action check
./scripts/site.ps1 -Action preview -Refresh auto
```

`site.ps1` is the single local entry point. Use `-Action doctor` to diagnose tools, `-Action test` for unit tests and AI configuration, and `-Action verify -Scope staged` for proportional checks before a commit. Enable the versioned Git hook once per checkout with `git config core.hooksPath .githooks`.

Use `-Python <executável>` e `-Quarto <executável>` se não estiverem no PATH. O script procura também o Quarto instalado com RStudio. O preview público usa a porta 4321 e o Quarto a porta 4323 por padrão; encerre os processos da tarefa antes de iniciar servidores concorrentes. Não encerre processos de outras tarefas.

Para conferir uma renderização parcial:

```powershell
quarto render index.qmd --no-clean
python scripts/check-site.py
quarto run scripts/build-agent-assets.ts --check
quarto run scripts/test-content-source.ts
```

`site.ps1 -Action check` verifica o acervo, links e recursos locais, além dos arquivos para agentes; não substitui os testes unitários. A validação não verifica disponibilidade dos links externos nem garante funcionamento visual. A listagem de Portfólio pode avisar que não há arquivos enquanto nenhum projeto estiver cadastrado.

Escreva testes para comportamentos relevantes e regressões: datas inválidas, caracteres especiais, links quebrados e sincronização de formatos. Não crie testes que apenas repitam o código ou fixem palavras e estilos de uma mudança simples. Não afirme que um teste passou se não foi executado. Registre bloqueios e limitações observados.

## Branches e commits

As regras gerais de branch, commit e merge são as skills globais `vfx-criar-branch`, `vfx-criar-commit` e
`vfx-integrar-branch`, vindas de `vfx-llm-skills` e declaradas em [recursos.yaml](recursos.yaml). As seções
abaixo registram o que é específico deste repositório.

Antes de uma operação Git, confira `git status --short`, `git branch --show-current` e os diffs necessários. Mudanças preexistentes pertencem ao usuário; não as descarte nem as inclua automaticamente.

- Use `codex/<assunto-em-kebab-case>` para novas branches, salvo nome explícito do usuário. Confirme a base e a existência da branch; não suponha que a branch principal se chama `main`.
- Não use stash, reset destrutivo, clean ou force-push para contornar um estado de trabalho inesperado.
- Faça staging por arquivo ou trecho e revise `git diff --cached`. Evite `git add .` em uma árvore com trabalho misturado.
- Cada commit deve representar uma mudança coerente, com suas fontes e saídas necessárias. Use um assunto curto, como `fix: corrige codificação do currículo`, seguindo a convenção existente.
- Não remova saídas obrigatórias de `docs/` sob a justificativa de que são geradas. Separe alterações geradas sem relação com o objetivo do commit.
- Verifique `git diff --cached --check`; avalie avisos do gerador sem reformatar todo o site apenas para removê-los.
- Ao terminar, confira o commit e o estado restante. Criar instruções Git não autoriza executar commits, merges ou publicar alterações.

## Merge

Confirme origem, destino e a intenção do usuário antes da mutação. Inspecione os commits a integrar e os diffs. Uma referência remota local pode estar desatualizada; faça fetch quando necessário e informe se a rede impedir essa conferência.

Prefira fast-forward quando possível. Se as branches divergiram, use merge normal para preservar o histórico, salvo convenção do projeto ou escolha explícita por squash/rebase. Não reescreva histórico compartilhado por conveniência.

Faça a integração em uma árvore limpa ou em worktree isolado, sem transportar mudanças alheias. Em conflitos, resolva primeiro as fontes; para `docs/`, regenere a saída correspondente. Preserve `_freeze/` e verifique conflitos em dados e resultados computacionais individualmente. Não aceite um lado inteiro sem revisar o conteúdo perdido.

Antes de concluir um merge com conflitos, confira ausência de entradas não resolvidas com `git ls-files -u`, revise o diff e execute as verificações pertinentes. Se a resolução depender de uma decisão de conteúdo que não possa ser inferida, apresente o conflito concreto. Se precisar abandonar, use `git merge --abort` somente para o merge iniciado nessa operação e após conferir que nenhum trabalho novo será perdido.

Push, publicação no GitHub Pages, exclusão de branches e reescrita de histórico exigem escopo autorizado pelo usuário. Não os infira de um pedido para preparar, revisar ou fazer commit.

## Entrega

Ao terminar cada pedido, atualize o preview e confirme sua atualidade conforme `AGENTS.md`. Em alterações no site, encerre o servidor Quarto da tarefa antes de renderizar e reinicie depois, para evitar HTML antigo mantido em memória. Confira um trecho ou recurso concreto da mudança na resposta da URL pública, inclusive quando houver proxy. Compare o conteúdo relevante com `docs/`; scripts injetados de live reload podem diferir. Em tarefas apenas de documentação, confira e reabra a última saída válida. HTTP 200 e renderização bem-sucedida não substituem essa verificação.

Informe o resultado, as verificações executadas e limitações relevantes. Siga a regra de preview em `AGENTS.md`, inclusive para tarefas de documentação. Não faça commit ou merge apenas porque os documentos descrevem como fazê-los.
