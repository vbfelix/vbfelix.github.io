# Como criar um segundo cérebro

Fonte: https://vbfelix.github.io/posts/0047-llm-wiki/index.html

### Joguei o acervo inteiro no modelo e mandei montar

Meu primeiro segundo cérebro custou 35 horas de trabalho num fim de semana e mais de 500 perguntas respondidas. Foi a segunda tentativa.

Na primeira, peguei o acervo da empresa em que eu trabalhava, com documentação, gravação de reunião e o que publicávamos na internet. Joguei tudo dentro do modelo e mandei montar. O resultado não foi horroroso. Só não me atendeu.

Refiz do avesso. A estrutura, as relações entre os conceitos e o método saíram de mim. O modelo fez o resto, e o resto era muito trabalho.

A ideia vinha de um texto curto de Andrej Karpathy chamado *LLM Wiki*. E se tornou o jeito mais simples que conheço de dar memória a um modelo. Depois, construí um segundo cérebro pessoal, que hoje guarda o que eu escrevo e o que eu leio. Nas duas vezes, o modelo fez o trabalho braçal melhor do que eu faria. As decisões continuaram comigo.

### O modelo esquece, então alguém tem de escrever

Chamo de segundo cérebro uma base de conhecimento que fica fora da minha cabeça e fora da conversa, e que o modelo consulta antes de responder.

Ela existe porque um LLM não guarda nada de uma conversa para a outra. O que ele sabe do seu trabalho é o que cabe na *context window*, a janela de texto que ele enxerga de uma vez. Fechou a sessão, ele volta ao zero.

A saída mais comum era o RAG, ou seja, a cada pergunta, um sistema busca nos seus documentos os trechos que parecem relevantes e os entrega ao modelo junto com a pergunta. Funciona. Só que, nas palavras de Karpathy, o modelo redescobre o conhecimento do zero a cada pergunta, e nada se acumula ([Karpathy, *LLM Wiki*](https://gist.github.com/karpathy/442a6bf555914893e9891c11519de94f)).

Karpathy foi membro fundador da OpenAI e diretor de IA da Tesla ([karpathy.ai](https://karpathy.ai/)). Em abril de 2026, publicou o *LLM Wiki* como um arquivo de ideias para colar no seu agente e construir a sua versão junto com ele. Agente, aqui, é o modelo que trabalha em etapas, lendo e escrevendo arquivos no seu computador.

A proposta muda o momento do trabalho. Em vez de procurar nos documentos brutos a cada pergunta, o modelo lê cada fonte uma vez e extrai o que importa. Depois integra o resultado numa wiki, um conjunto de páginas em Markdown ligadas entre si. Markdown é texto simples com marcação leve. O conhecimento é compilado uma vez e depois mantido em dia.

São três camadas:

 - **Fontes brutas**: os artigos, as transcrições e os dados que você escolheu, imutáveis, porque o modelo lê e nunca edita;
 - **Wiki**: a síntese que o modelo escreve e mantém e você lê, com páginas de resumo, de conceito, de comparação e de entidade, como uma pessoa ou uma empresa;
 - **Regras**: um arquivo `AGENTS.md`, que o agente lê ao começar e que diz como a wiki se organiza e o que fazer em cada operação.

E três operações:

 - **Ingestão**: o modelo lê uma fonte nova, escreve o resumo e atualiza as páginas que ela toca;
 - **Consulta**: o modelo responde a partir da wiki, com citação, e a resposta boa pode voltar para ela como página nova;
 - **_Lint_**: uma revisão periódica atrás de contradição, afirmação desatualizada e página órfã, aquela para a qual nenhuma outra aponta.

O `index.md` ajuda a navegar. É o catálogo, com cada página, um link e uma linha de resumo, e o modelo lê esse arquivo antes de abrir qualquer página. O índice, diz Karpathy, funciona surpreendentemente bem em escala moderada, com cerca de 100 fontes e centenas de páginas, sem precisar montar um RAG.

A aposta dele está na manutenção. O que cansa numa base de conhecimento, segundo ele, é o trabalho braçal: atualizar referência cruzada, manter resumo em dia e anotar quando um dado novo contradiz um velho. Pessoas abandonam wikis porque a manutenção cresce mais rápido que o valor. O modelo não cansa, então o custo de manter fica perto de zero.

A proposta também divide o trabalho. A pessoa escolhe as fontes, dirige a análise, faz as perguntas e pensa no que tudo aquilo significa. O modelo faz todo o resto, inclusive a wiki inteira. E o texto é abstrato de propósito: descreve um padrão e deixa estrutura, formato e ferramenta para você e o seu agente. Ficam em aberto duas perguntas que eu só enxerguei construindo: quem decide quando duas fontes discordam dentro da wiki, e como saber se a busca acha a página certa.

### Quinhentas perguntas que o modelo me fez

No segundo cérebro da empresa em que atuei, havia várias fontes, mas faltava um lugar central para elas e faltava curadoria, alguém que julgasse o que entra e o que vale. Contei o resto dessa construção em [*Criando meu clone com IA*](https://vbfelix.github.io/portfolio/0040-criando-meu-clone-com-ia/). Aqui fica a parte da wiki.

A primeira tentativa falhou porque deixei a estrutura com o modelo. Ele foi treinado no assunto de todo mundo, e não no nosso. Não tinha como saber qual conceito puxa qual, nem qual relação é só ruído. Isso vem de quem opera o negócio todo dia.

Na segunda, impus três regras:

 - **Pergunta crítica bloqueia**: certas lacunas tinham de ser respondidas antes de o conteúdo avançar;
 - **Internet proibida**: a base se apoiava só nas fontes fornecidas;
 - **Divergência tem dono**: cada conflito entre fontes, e cada resposta, precisava de alguém declarado que respondesse por ele.

As mais de 500 perguntas nasceram nos pontos de ambiguidade, e respondê-las era a curadoria. O modelo parava, perguntava e esperava alguém decidir.

Depois veio a estrutura. Defini as entidades daquele negócio e como elas se ligavam. Para cada tipo de pergunta, defini a página por onde a consulta começava, as ligações que ela seguia e onde parava.

Pus o segundo cérebro à prova num projeto que eu mesmo tinha desenhado e escrito. Chamei uma pessoa do time para analisá-lo comigo e com a IA, e perdi para a IA. Ela recuperou decisões, restrições e premissas que eu já não tinha na cabeça, e apontou bloqueios e dependências que não estavam explícitos. Boa parte do que ela sabia tinha saído de mim.

O trabalho braçal o modelo fez. Responder às mais de 500 perguntas, não.

### Uma wiki para o agente ler antes de mim

O meu segundo cérebro pessoal é separado do repositório onde escrevo. A manutenção é feita sobretudo pelo Claude Code, e eu fico na curadoria. As páginas são Markdown versionado em Git.

A raiz, resumida:

```text
vfx-llm-wiki/
├── AGENTS.md              as regras que o agente lê ao começar
├── raw/                   fontes brutas, imutáveis
│   ├── capturas/
│   └── manifesto.yaml     uma entrada por fonte, com id e origem
├── wiki/                  a síntese que o agente escreve
│   ├── conceitos/
│   ├── documentos/
│   ├── episodios/
│   ├── fatos/
│   ├── teses/
│   ├── ...
│   └── indices/
├── schema/
│   └── entidades.yaml     tipos de página, seções e relações
├── scripts/
│   └── wiki.py            buscar, ler, validar, lint, avaliar
└── tests/
```

Ele segue as três camadas e acrescenta o que a proposta deixa em aberto:

 - **Página tipada**: conceito, documento, episódio, fato, tese e outros, cada tipo com as suas seções;
 - **Procedência**: toda página aponta as fontes que a sustentam, e o *lint* avisa quando falta declarar os limites;
 - **Relação com duas pontas**: quando uma página aponta para outra, a outra fica sabendo, e duas páginas que divergem levam a relação `contradiz`;
 - **Ingestão que muda a síntese**: uma fonte só termina de entrar quando alimenta as páginas que ela sustenta;
 - **Leitura em três níveis**: buscar, ler a visão resumida e só então abrir a seção necessária.

Um exemplo mostra quase tudo de uma vez: o ensaio de Harrison Chase, cofundador da LangChain. Quando ele entrou, virou a página `documento/langchain/wiki-memory` e passou a sustentar outras cinco, entre elas `conceito/rag` e `pessoa/harrison-chase`. Este é o começo dela, com alguns campos a menos:

```yaml
---
id: documento/langchain/wiki-memory
tipo: documento
titulo: Memória em formato de wiki para agentes, da LangChain
status: rascunho
atualizado_em: '2026-10-14'
fontes:
- FONTE-1325
relacoes:
  relacionado_a:
  - organizacao/langchain
  - pessoa/harrison-chase
  - conceito/wiki-memory
  - conceito/memoria-de-longo-prazo-de-agentes
  - conceito/rag
limites: Ensaio de opinião de um cofundador de fornecedor, sem dados medidos. Os exemplos e a conversa
  com um conhecido de uma empresa de pesquisa são relato dele, sem conferência; a captura não traz imagens
  nem links do original.
---
```

O campo de que eu mais gosto é `limites`. Ele diz o que a página não sustenta, e a visão resumida, que o agente lê antes de abrir a página, traz esse campo logo no começo. Aqui, ele avisa que o ensaio é opinião do cofundador de um fornecedor, sem dado medido.

Cada fonte bruta tem um id no manifesto. A deste ensaio é a `FONTE-1325`, e a primeira de todas, a `FONTE-001`, é o próprio *LLM Wiki*. As cinco páginas da lista apontam de volta para esta página como fonte, e é isso que a relação com duas pontas exige.

A divergência também vira relação. A minha tese de que validar pelo uso, lançando e vendo se as pessoas usam e pagam, substitui o *discovery* tradicional, a pesquisa com clientes antes de construir, está em `tese/validacao-por-uso-substitui-o-discovery` e aponta `contradiz` para quatro livros. Um deles é *Continuous Discovery Habits*, de Teresa Torres, e a página do livro mostra a mesma relação vista do outro lado. Quem consulta a tese encontra, na mesma leitura, quem diverge dela.

### 25 mil *tokens* antes da primeira resposta

O `index.md` não chegou a quebrar. Ficou caro. Com 395 páginas, dentro da faixa que Karpathy diz que ele aguenta, o arquivo que o agente lia antes de qualquer consulta já tinha 104 KB, cerca de 25 mil *tokens*. *Token* é o pedaço de texto que o modelo processa, e é por ele que se paga o uso. Eram 25 mil *tokens* da *context window* gastos antes de a pergunta começar.

Karpathy avisa que, conforme a wiki cresce, o índice deixa de bastar e é hora de ter busca de verdade. Cheguei nesse ponto cedo. O índice deu lugar à leitura em três níveis. A busca devolve a página, o trecho que casou e a seção a abrir. A visão resumida mostra resumo, limites, fontes e o tamanho de cada seção, antes de o agente abrir qualquer uma. O `index.md` virou um guia de 786 bytes.

Antes de mexer na busca, medi. O agente montou 34 consultas de referência, com a página certa de cada uma definida na curadoria, e contou em quantas ela vinha em primeiro lugar. É o acerto na primeira posição. Com o texto das páginas indexado por seção, **o acerto na primeira posição subiu de 0,735 para 0,971**, e o tempo de cada busca caiu de 0,841 s para 0,050 s.

Sem essa régua, eu teria lido o problema como falta de conteúdo e saído escrevendo página. O conteúdo estava lá. A busca é que só olhava o nome, o título e o resumo de cada página, e não lia o texto delas.

A régua também tinha limite. As 34 consultas pediam uma página pelo nome, por um termo do texto ou por uma pergunta direta. Depois, o conjunto cresceu para 138 consultas, com tipos novos de pergunta: em inglês, com erro de digitação, com palavra flexionada. Nele, o acerto na primeira posição da busca de setembro ficou em 0,681. As perguntas pelo nome e pelo título continuaram certas, e a queda se concentrou nos tipos novos. Foi essa régua maior que guiou a busca seguinte, que chegou a 0,978 nas mesmas 138. São perguntas escritas por quem desenhou a busca: protegem contra regressão.

Nenhum desses defeitos se fechou sozinho. Todos foram encontrados e corrigidos com curadoria.

### Cada um desconfia do modelo de um jeito

Duas implementações abertas me ajudaram a separar o que é do padrão e o que é escolha de cada um.

A LangChain lançou em julho de 2026 o OpenWiki, que, no modo para repositórios, gera e mantém a wiki do código. No `AGENTS.md`, ele põe só um ponteiro curto para a wiki, porque ela inteira gastaria contexto demais em toda execução ([LangChain, *Introducing OpenWiki*](https://www.langchain.com/blog/introducing-openwiki-an-open-source-agent-for-repo-documentation)). Na versão 0.4.0, cada afirmação relevante de uma página passou a guardar a evidência no código que a sustenta. A evidência é um endereço como `repo://src/scheduler.ts#L393-L404`, um trecho de linhas de um arquivo. Quando esse código muda, a afirmação fica marcada como desatualizada, sem chamar modelo nenhum, e o agente reconfere na atualização seguinte ([LangChain, *Building Self-Correcting Memory in OpenWiki*](https://www.langchain.com/blog/self-correcting-memory-openwiki)).

Fabio Akita foi por outro caminho. O ai-memory dele transforma as sessões do próprio agente numa wiki em Markdown versionada em Git. As páginas são pequenas: `decisions/use-ledger-table-for-reconciliation.md` guarda uma decisão, e `gotchas/payment-importer-timezone.md`, uma armadilha. O Markdown é a fonte de verdade, e o banco de busca é só um índice que se refaz a partir dele. Uma rotina em segundo plano revisa as sessões encerradas e escreve páginas depois de validar evidência e caminho. Com a aprovação ligada, as propostas esperam uma pessoa aprovar ([Akita, *ai-memory*](https://www.akitaonrails.com/2026/06/16/ai-memory-memoria-longo-prazo-karpathy-wiki-auto-aprendizado-hermes-projetos/)).

Chase lembra um limite que, para mim, vale para as duas e para a minha. Wiki não é toda a memória: serve melhor ao conhecimento durável de um domínio, e não necessariamente ao estado de curto prazo da conversa, às preferências de quem usa ou a registros de eventos frequentes ([Chase, *Wiki Memory*](https://www.langchain.com/blog/wiki-memory)).

Akita chegou ao ai-memory depois de recomendar outra implementação do padrão e voltar atrás depois de uma semana de uso ([Akita, *Memória de Agentes IA*](https://www.akitaonrails.com/2026/05/18/memoria-agentes-karpathy-llm-wiki-agentmemory/)). Ele resume numa frase o risco de deixar a memória se escrever sozinha: "Memória ruim é pior que falta de memória." A falta obriga a perguntar. A memória ruim dá certeza falsa ([Akita, *ai-memory*](https://www.akitaonrails.com/2026/06/16/ai-memory-memoria-longo-prazo-karpathy-wiki-auto-aprendizado-hermes-projetos/)).

### Sete passos para errar menos do que eu errei

Se eu começasse hoje, faria nesta ordem. Serve para qualquer agente que leia e escreva arquivos e siga um arquivo de instruções. Um chat que não mexe em arquivos não basta.

 - **Um domínio só**: escolha uma área e as perguntas que você responde toda semana. Foi o acervo inteiro de uma vez, sem estrutura, que derrubou a minha primeira tentativa;
 - **Fonte separada da síntese**: guarde as fontes numa pasta que o modelo lê e não edita. Com elas intactas, qualquer página errada pode ser refeita;
 - **Regras antes da primeira página**: escreva no `AGENTS.md` que tipos de página existem, como cada uma cita a fonte e onde declara o que não sustenta, como o campo `limites` da minha wiki. É a parte que a proposta deixa para você;
 - **Uma fonte por vez**: ingira, leia o que mudou e só então siga, como o próprio Karpathy prefere fazer. A fonte só terminou de entrar quando alimentou as páginas que ela sustenta;
 - **Dono para cada divergência**: quando o modelo perguntar, responda, e registre quem responde por cada conflito. As minhas mais de 500 perguntas eram esse trabalho;
 - **Consultas de referência**: antes de mexer na busca, escreva um conjunto pequeno de perguntas cuja página certa você já conhece, com o nome da página, um termo que só aparece no texto e uma pergunta inteira, como "o que é RAG?". Varie os tipos de pergunta com o tempo, porque foram os tipos novos que mostraram o que a régua pequena escondia;
 - **_Lint_ com critério**: rode a revisão de tempos em tempos e decida o que só avisa e o que impede de seguir. Na minha wiki, o *lint* só avisa, e quem barra é a validação, que confere se cada página cumpre as regras. Bloquear por qualquer sinal trava o trabalho sem deixar a wiki mais correta.

Eu não peço mais ao modelo para montar o meu segundo cérebro. Peço para manter o que eu desenhei, e atuo nas perguntas que ele não sabe responder.
