# Proposta de reformulação do portfólio

Referência: https://www.rubenmarcus.dev/ — consultada em 5 de setembro de 2026.
Branch: `codex/reforma-portfolio`.
Status: proposta aprovada e implementada na branch codex/reforma-portfolio. O experimento interativo permanece opcional para uma etapa futura.

## Direção recomendada

Conceito: **um caderno de investigação estatística**, com rigor de publicação científica e a presença pessoal de um portfólio. Perguntas, métodos, evidências e decisões organizam a experiência. A referência informa a clareza da apresentação e a facilidade de explorar o trabalho; a identidade visual será própria, pautada em estatística, pesquisa aplicada, dados e produto.

O site atual já tem abertura escura, retrato, números e trajetória. A principal mudança será transformar essa apresentação biográfica em uma porta de entrada para trabalhos, ideias e experiência.

## Identidade visual

- Base clara de papel (`#F4F1E9`), texto em tinta azul (`#202D40`) e linhas suaves (`#D6D1C5`).
- Azul mineral (`#355C80`) para links e séries principais; terracota (`#A34E35`) para observações em destaque. A cor comunica função e vem acompanhada de rótulos ou formas.
- Títulos em serifa editorial, texto em fonte sem serifa e números tabulares em metadados. Essa combinação aproxima o site de um ensaio científico contemporâneo.
- Retrato pequeno, com cores naturais e legenda, integrado à apresentação pessoal. A visualização estatística recebe o espaço principal da composição.
- Respiro de página impressa, legendas de figuras, alinhamentos por eixos e linhas finas. Fundos quadriculados ficam restritos às figuras, sem interferir na leitura.
- Artigos com largura confortável, equações bem compostas, tabelas legíveis, referências e código com destaque moderado.

## Elementos de dados e estatística

1. **Figura de abertura:** um diagrama de dispersão com tendência e faixa de incerteza, acompanhado de uma anotação sobre a passagem de observações para uma interpretação. Usar dados públicos identificados ou simulação claramente rotulada; a figura não deve sugerir resultados profissionais fictícios. Sem animação automática contínua.
2. **Projetos como investigações:** cada trabalho apresenta pergunta, dados disponíveis, método, contribuição pessoal, resultado e limitações. Uma pequena figura específica funciona como capa: série temporal, mapa, distribuição ou comparação, conforme o conteúdo.
3. **Trajetória em uma escala temporal:** eixo de anos com intervalos reais dos cargos e marcos de formação, com alternativa textual acessível. Evitar notas arbitrárias de domínio de ferramentas.
4. **Escritos com identidade gráfica própria:** miniaturas baseadas no assunto de cada texto, como distribuição, escala logarítmica ou paradoxo estatístico. Reaproveitar gráficos do acervo quando adequados.
5. **Pequeno experimento opcional:** uma seção de exploração sobre tamanho amostral e incerteza, com controle de tamanho da amostra e explicação curta. Deixar para a segunda etapa; só incluir se funcionar bem no celular e acrescentar valor didático.

Gráficos informativos terão título, eixos, unidades, fonte e descrição textual. Cores, símbolos e intervalos devem representar algo definido. Elementos puramente decorativos serão discretos e ignorados por leitores de tela.

## Estrutura da página inicial

| Ordem | Bloco | Conteúdo e função |
|---|---|---|
| 1 | Navegação | Nome, Trabalhos, Escritos, Sobre, Contato e seletor PT/EN. |
| 2 | Apresentação | Nome, posicionamento curto e figura estatística com legenda; retrato menor e acesso aos trabalhos. |
| 3 | Trabalhos selecionados | Três investigações verificáveis, com pergunta, método, contribuição e evidência do resultado. |
| 4 | Experiência em números | Reaproveitar os números existentes apenas depois de conferir fonte, período e significado. |
| 5 | Áreas de atuação | Estatística e pesquisa; dados e engenharia; estratégia e produto, acompanhadas de exemplos. |
| 6 | Escritos recentes | Três textos com título, assunto, data e acesso ao arquivo completo. |
| 7 | Trajetória resumida | Marcos profissionais e link para a página Sobre. |
| 8 | Contato | Convite breve para conversar e links públicos já existentes. |

No desktop, a abertura terá título editorial acima de uma composição assimétrica de apresentação pessoal e figura estatística. Os trabalhos aparecem em linhas amplas, com figura e síntese da investigação. No celular, texto, figura e legenda seguem uma ordem única; nenhum significado depende de hover.

## Organização do conteúdo

Publicações, certificações, participações e prêmios continuam acessíveis. A navegação principal fica mais curta, com esses conteúdos ligados à página Sobre e ao rodapé. Endereços existentes devem ser preservados.

Os candidatos a destaque devem vir do acervo real: software em R, análises publicadas e trabalhos profissionais documentados. Não apresentar um artigo como projeto entregue a cliente, nem atribuir resultados sem comprovação. A seleção final e os textos em português e inglês serão conferidos com a skill ghost-writer e sua base editorial durante a implementação.

A configuração atual exclui `posts/**` da renderização e há exclusões anteriores em `docs/posts/`. Antes de recolocar o blog na home, será necessário verificar a intenção dessa exclusão e preparar a renderização dos artigos com suas dependências. A proposta não reverte essas alterações anteriores.

## O que aproveitar da referência

Aproveitar a apresentação profissional imediata, a facilidade de explorar trabalhos e a proximidade entre experiência e textos. A composição, paleta, tipografia, imagens e interações seguem a linguagem estatística descrita nesta proposta, conforme a orientação do usuário de se inspirar sem copiar o estilo ou as cores.

Nesta primeira versão, manter as interações leves. Música, jogos, cenas 3D, métricas ao vivo e integração MCP adicionariam manutenção sem um objetivo definido para este portfólio. Disponibilidade profissional e ofertas comerciais só devem aparecer quando confirmadas.

## Implementação proposta

1. **Base visual e home:** revisar cores e tipografia, criar a figura estatística de abertura e os componentes de investigação, ajustar a navegação e sincronizar PT/EN.
2. **Trabalhos e escritos:** selecionar conteúdos comprovados, criar a apresentação dos trabalhos e recuperar a navegação dos artigos após conferir a configuração de renderização.
3. **Páginas internas e acabamento:** aplicar o sistema às páginas existentes, revisar leitura, acessibilidade, metadados, links e comportamento móvel.

Manter Quarto e GitHub Pages, com saída em `docs/`. Usar SCSS/CSS para a identidade visual e JavaScript apenas onde houver uma interação necessária.

## Critérios de conclusão da futura reforma

- Home e páginas internas renderizam; artigos destacados e arquivos vinculados abrem corretamente.
- Layout sem rolagem horizontal em celular, tablet e desktop.
- Navegação por teclado, foco visível, contraste adequado e suporte a movimento reduzido.
- PT/EN com estrutura equivalente e traduções revisadas.
- Dados profissionais e resultados rastreáveis às fontes editoriais.
- Preview Quarto validado e aberto para inspeção antes da entrega.

## Estado desta entrega

A branch foi criada a partir da `master` local, incluindo suas alterações não commitadas. A implementação inclui a identidade visual, home e acervo PT/EN, trajetória profissional, navegação e recuperação dos 31 artigos na renderização. O preview mostra a reforma. Não houve commit nem publicação.
