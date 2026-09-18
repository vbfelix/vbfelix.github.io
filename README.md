# Site de Vinícius Félix

As orientações para testes, validações e Git ficam em [CONTRIBUTING.md](CONTRIBUTING.md). As skills reutilizáveis vêm da biblioteca `vfx-llm-skills`, declaradas em [recursos.yaml](recursos.yaml); as skills específicas do site ficam em `.agents/skills/`. Ambas são indicadas em [AGENTS.md](AGENTS.md), que vale também para o Claude Code via [CLAUDE.md](CLAUDE.md).

Site pessoal em Quarto, com interface em português e artigos preservados no idioma original. O GitHub Pages publica a pasta `docs/`.

## Estrutura

- `_content/`: conteúdo compartilhado das páginas institucionais e da home. Edite aqui.
- `_content/header-about.qmd`: fonte única do texto de “Sobre mim” e da página inicial. Os arquivos `header-about.qmd` e `index.qmd` na raiz apenas definem os metadados e incluem esse conteúdo.
- `_content/experience-table.qmd`: fonte única da tabela profissional e dos resumos usados nos currículos. A página inicial e a página de experiência incluem essa mesma tabela.
- `*.qmd`: metadados e includes das páginas públicas na raiz.
- `pt-br/*.html`: redirecionamentos estáticos dos endereços antigos para a raiz, preservando parâmetros e âncoras.
- `posts/`: fontes, referências e dados dos artigos.
- `portfolio/`: snapshot dos artigos de `posts/portfolio` da branch `main` de `vbfelix/ghost-writer`, além de recursos visuais locais. Não edite os textos importados aqui.
- `_freeze/`: resultados computacionais dos artigos. Preserve esta pasta para renderizar sem reexecutar as análises em R.
- `images/`, `files/`: imagens e documentos usados pelo site.
- `styles.css`, `custom_theme.scss`: estilos do site e configuração do Bootstrap.
- `scripts/`: geração do acervo, validação e comandos de manutenção. Filtros Quarto ficam em `scripts/filters/`.
- `maintenance/design/`: histórico das propostas e recursos visuais desativados.
- `docs/`: saída gerada e versionada para publicação. Não edite o HTML manualmente.

## Renderizar e conferir

Requisitos: Quarto e Python 3.10 ou superior. Os scripts Python usam apenas a biblioteca padrão. Para reexecutar os artigos, também é necessário o ambiente R e as dependências de cada análise.

No PowerShell, na raiz do repositório:

```powershell
./scripts/site.ps1 -Action preview
```

O comando atualiza o acervo, renderiza, valida os links locais e inicia o preview em `http://127.0.0.1:4321`. Use `-Python`, `-Quarto` e `-Port` para indicar executáveis ou porta alternativos. Encerre com Ctrl+C antes de iniciar outra renderização.

No Windows, esse comando coloca `scripts/preview-proxy.py` na porta pública e o Quarto duas portas acima. A camada local informa `text/plain; charset=utf-8` para Markdown e texto, evitando detecção incorreta dos acentos no navegador. Os arquivos publicados continuam em UTF-8; essa configuração HTTP vale apenas para o preview local.

```powershell
./scripts/site.ps1 -Action render
./scripts/site.ps1 -Action check
python -m unittest discover -s scripts -p 'test_*.py'
```

Equivalente multiplataforma:

```sh
python scripts/sync-portfolio.py
python scripts/build-writing.py
quarto render --no-clean
python scripts/check-site.py
quarto run scripts/build-agent-assets.ts --check
quarto preview --port 4321 --host 127.0.0.1 --no-browser
```

O render preserva as figuras computacionais já existentes. A validação percorre todas as páginas geradas, verifica links, âncoras e recursos locais, e falha se não houver home renderizada. Ela não verifica disponibilidade de sites externos.

## Acervo e compatibilidade

Os textos do Portfólio vêm de `posts/portfolio/<slug>/index.qmd` na branch `main` de [vbfelix/ghost-writer](https://github.com/vbfelix/ghost-writer). Edite e integre os artigos lá. `site.ps1 -Action render` e `-Action preview` buscam essa branch antes de renderizar. É necessário Git e acesso ao repositório remoto. Falhas de acesso interrompem o comando, sem fallback silencioso para arquivos antigos.

`python scripts/sync-portfolio.py` atualiza o snapshot em `portfolio/` e registra commit e hashes em `portfolio-source.json`. Versione ambos com `docs/`. O cache Git fica ignorado em `.cache/`; o checkout local do ghost-writer não é usado. `--check` confere o snapshot offline, sem afirmar que está na revisão remota mais recente. Para renderizar diretamente pelo Quarto, execute a sincronização antes.

Thumbnails (`thumbnail.*`) e `_metadata.yml` são de responsabilidade deste site e nunca são importados, mesmo quando existem na origem. Siga [a direção visual das thumbnails](maintenance/design/portfolio-thumbnails.md) ao criar uma capa com elementos específicos do artigo. Outras imagens locais que não existem na origem são preservadas; textos importados não devem ser editados no site. Alterações locais em arquivos gerenciados interrompem a sincronização. Arquivos removidos na origem são removidos apenas quando gerenciados pelo manifesto. Artigos locais anteriores à migração permanecem disponíveis enquanto não existem na origem e ficam registrados em `pendingMigration`; quando chegarem à `main`, serão substituídos pelo conteúdo remoto, preservando recursos visuais locais. O catálogo continua separado dos artigos históricos de `posts/`.

`python scripts/build-writing.py` atualiza apenas `_content/writing.qmd`; `--check` verifica se está atualizado sem gravar. O gerador aceita título e data em uma linha e categorias em lista inline, como nos artigos existentes. Formatos não suportados geram erro explícito.

Os breadcrumbs são gerados por `scripts/filters/breadcrumbs.lua`. A home usa `breadcrumbs: false`. Os arquivos HTML em `pt-br/` são copiados como recursos pelo Quarto e apenas redirecionam para a raiz. Não há uma segunda versão renderizada das páginas. Cada redirecionamento inclui URL canônica, instrução para não indexar e um link alternativo para navegadores sem JavaScript.

`python scripts/build-figure.py` reproduz a antiga figura de regressão em `maintenance/design/`. Ela é um recurso histórico e não faz parte da home atual.

Não remova `_freeze/` nem `docs/` como parte de uma limpeza de cache. `.quarto/`, `_site/`, `site_libs/` na raiz e caches Python são locais e ignorados pelo Git.

## Consulta por agentes e currículo

O hook `post-render` executa `scripts/build-agent-assets.ts` usando o Deno integrado ao Quarto, sem dependências externas. Ele gera os arquivos abaixo diretamente em `docs/`, inclusive em renderizações incrementais:

- `/llms.txt`: índice de fontes para agentes. `/llm.txt` é uma cópia de compatibilidade.
- `/curriculo.md`: currículo sintético, focado na formação e experiência profissional. `/llms-full.txt` preserva perfil, trajetória detalhada e documentos completos para agentes.
- `/curriculo.json`: esquema próprio versionado (`schemaVersion: 1.0`), com `work`, `education`, `sameAs` e `sources`. Experiência e formação têm datas `YYYY-MM`, sem dias inventados. `sourceRevision` identifica o conteúdo de origem por SHA-256, não a data de atualização biográfica.
- `/<pagina>.html.md`: versões Markdown das sete páginas institucionais, com links absolutos e sem elementos decorativos.
- Metadados JSON-LD `Person` e `ProfilePage` nas páginas de perfil, `WebPage` nas demais, e links HTML de descoberta dos formatos alternativos.

Edite os dados públicos em `_content/`, nunca nos arquivos gerados. A tabela de experiência é a fonte dos campos estruturados; mudanças em seu formato ou nas datas devem ser acompanhadas de ajuste no gerador, que falha diante de um formato não suportado. Publicações, certificações, participações e prêmios ficam nas versões textuais e em `llms-full.txt`, fora dos currículos sintéticos. Markdown e texto usam BOM UTF-8 para preservar acentos em servidores sem charset; JSON permanece sem BOM.

`quarto run scripts/build-agent-assets.ts --check` verifica sincronização dos arquivos e metadados. O comando `site.ps1` também executa essa conferência. O sitemap e o robots.txt continuam sendo gerados pelo Quarto. Esses recursos facilitam consulta e extração, mas não garantem indexação ou citações em respostas de IA.

As descrições sintéticas dos cargos ficam na coluna Trabalho de `_content/experience-table.qmd`. O currículo utiliza esse texto, separando até três frases em linhas, sem manter outra cópia editorial. A formação e a trajetória detalhada ficam em `_content/header-experience.qmd`. A quebra visual pode variar conforme a largura da tela.

O gerador para agentes resolve os mesmos includes dos `.qmd` públicos por meio de `scripts/content-source.ts`, inclusive includes aninhados. `quarto run scripts/test-content-source.ts` testa atualização das fontes, equivalência entre home e Sobre mim, tabela compartilhada e erros de includes cíclicos ou ausentes. Esse teste também roda em `site.ps1`.
