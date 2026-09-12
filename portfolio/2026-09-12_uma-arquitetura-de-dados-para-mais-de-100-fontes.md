---
tipo: portfolio
data: '2026-09-12'
status: draft
titulo: Uma arquitetura de dados para mais de 100 fontes
site:
  categories: [data engineering]
  lang: pt-BR
episodio: episodio/da-arquitetura-de-dados-a-lideranca-de-produto-na-datlo
referencias: []
fatos:
- fato/fontes-integradas-na-arquitetura-de-dados-da-datlo
- fato/pipelines-na-arquitetura-de-fontes-externas
- fato/processos-na-arquitetura-de-fontes-externas
- fato/projetos-entregues-com-a-arquitetura-de-fontes-externas
- fato/tamanho-do-time-na-comparacao-de-entregas-da-arquitetura-de-fontes-externas
fontes: [FONTE-260, FONTE-261, FONTE-262]
---

## Brief

Caso de construção de uma arquitetura para fontes externas, com compreensão semântica, nomenclatura comum, tratamento, validação e vinculação. Público: profissionais de dados e lideranças técnicas. Preservar o resultado relatado em projetos de dados sem derivar produtividade individual, causalidade isolada ou redução de custos.

## Portfolio

Construí uma arquitetura que chegou a mais de 100 fontes de dados. Nenhuma delas era minha.

Eu não controlava a qualidade na origem, os formatos, a frequência de atualização ou os nomes usados para representar as informações. Limites, irregularidades e erros faziam parte do trabalho. Precisava transformar essa diversidade em um ativo centralizado, reutilizável e compreensível para a equipe e para quem consumia os dados.

A primeira condição para isso vinha antes da integração: eu precisava entender o que estava trazendo para dentro.

### Se não sabe o que é, não entra

O critério era simples: se eu não soubesse explicar o significado de um dado, ele não podia entrar na arquitetura. Se quem executava o pipeline, a sequência de etapas que coleta e transforma os dados, não conseguia explicar a informação, como o usuário conseguiria?

Ter acesso a uma base pública e ao seu dicionário não garantia esse entendimento. Era necessário pesquisar o contexto, examinar os registros e, muitas vezes, analisar o comportamento dos dados para investigar o que estava acontecendo.

Cheguei a ligar para a Agência Nacional de Energia Elétrica (ANEEL) para validar uma informação. Também li um livro sobre solos para entender um conceito. Levamos essa exigência a sério: saber abrir um arquivo não encerrava o trabalho de compreender seu conteúdo.

### Dar nome também era construir a arquitetura

Com tantas origens, eu precisava de ativos que pudessem ser usados em diferentes projetos e por diferentes pessoas. Isso exigia uma taxonomia, um sistema comum para organizar e nomear fontes e dados.

Construí um padrão de nomenclatura para que todos falassem a mesma língua. A centralização precisava vir acompanhada de uma forma compartilhada de identificar as informações.

Esse trabalho fazia parte do reuso. Para alguém utilizar um dado em outro projeto, precisava conseguir reconhecê-lo e entender o que ele representava. A arquitetura tinha de servir à equipe inteira.

### Do dado cru ao dado utilizável

Ingerir uma base, isto é, trazê-la da origem para o nosso ambiente, era apenas o começo. Depois de entender seu funcionamento, precisávamos testar, decodificar os valores, ajustar formatos e normalizar os registros para um padrão de uso.

Quando era possível, corrigíamos os dados. Quando um erro era identificado, substituíamos o valor por nulo, registrando a ausência de um valor utilizável naquele campo.

A camada Silver era a etapa da arquitetura em que o dado tratado precisava estar apto para consumo. O critério de chegada era exigente: as informações precisavam passar pelas transformações e validações necessárias antes de serem disponibilizadas.

As principais exigências desse trabalho estavam conectadas:

- **Entender o significado antes de integrar.** Pesquisa e validação com a origem ajudavam a esclarecer o que o dado representava e seus limites.
- **Padronizar para permitir reuso.** Nomenclatura e tratamento criavam uma base comum para trabalhar com fontes diferentes.
- **Testar antes de disponibilizar.** A validação automática fazia parte do caminho até o usuário e dava suporte às alterações realizadas pela equipe.

### Cruzar dados quando não há uma chave comum

Em um banco organizado para relacionar tabelas, uma chave primária identifica um registro, e uma chave estrangeira referencia um registro de outra tabela. Ao combinar fontes independentes, nem sempre tínhamos esses identificadores em comum.

O trabalho de integração podia exigir outras formas de vinculação:

- **Correspondência aproximada:** os chamados *fuzzy joins* procuram relacionar registros pela semelhança entre valores, em vez de exigir igualdade exata.
- **Relações espaciais:** os cruzamentos usam a localização e a relação entre geometrias para associar os dados.
- **Modelos de vinculação:** combinam informações para avaliar quais registros correspondem à mesma entidade.

Isso ampliava o trabalho necessário para tornar as fontes utilizáveis em conjunto. Entender os dados também era uma condição para decidir como relacioná-los.

### Aprendizados, lições, erros e principais impactos

A arquitetura reuniu **mais de 100 fontes, 200 pipelines e 380 processos**. Com esse ativo, conseguimos **dobrar a quantidade de projetos de dados entregues com metade do tamanho do time**.

A equipe podia trabalhar em diferentes partes da arquitetura com validação automática, voltada a impedir que erros chegassem ao usuário. O resultado combinava reuso dos ativos com uma forma comum de tratar e conferir os dados.

Dessa construção, destaco:

- **A responsabilidade pelo significado continuava sendo nossa.** Mesmo quando a fonte era pública e tinha dicionário, compreender a informação exigia investigação. A ligação para a ANEEL e a leitura sobre solos fizeram parte desse trabalho.
- **Reuso exigia entendimento compartilhado.** Centralizar dados vinha acompanhado de organizar nomes e formas de uso, para que o conhecimento pudesse circular pela equipe.
- **Disponibilizar cedo demais era uma armadilha.** O dado precisava chegar à Silver tratado e validado. Estar acessível não bastava para estar pronto para consumo.
- **O impacto apareceu nas entregas da equipe.** O crescimento da quantidade de projetos entregues foi o resultado relatado dessa arquitetura. A quantidade de fontes dimensionava o ativo; a qualidade continuava sendo o critério para construí-lo.

## Evidências

- raw:FONTE-260: relato do autor recebido em 2026-09-12. Sustenta autoria da arquitetura, fontes externas, falta de controle sobre origem, pesquisa semântica, ligação à ANEEL, leitura sobre solos, taxonomia, tratamento, Silver, testes, formas de vinculação, escala e resultados da equipe.
- raw:FONTE-261: o autor esclarece que entregas eram projetos de dados.
- raw:FONTE-262: confirma o caso da Datlo, determina anonimato e esclarece correção quando possível e nulificação de valores com erros identificados na Silver.
- **Não documentado.** Períodos comparados para o dobro de projetos e a metade do time, Q-063. O artigo preserva os resultados declarados sem calcular produtividade por pessoa ou atribuir causalidade exclusiva.
- **Não documentado.** Regra que distingue os 200 pipelines dos 380 processos, Q-064. As contagens são apresentadas nas unidades do autor, sem inferir a relação entre elas.
- Destino preparado: `posts/portfolio/0038-arquitetura-fontes-externas/index.qmd`; destino no site: `posts/0038-arquitetura-fontes-externas/index.qmd`. Numeração local anterior até 0037; site no clone de consulta até 0031. Sem capa ou assets.

## Notes

- Título fornecido pelo autor preservado.
- A referência à ANEEL identifica a instituição contatada, não uma recomendação institucional ou validação de toda a arquitetura. Livro sobre solos não identificado nominalmente no relato; não inventar título ou conceito estudado.
- Os cruzamentos aproximados, espaciais e por modelos são explicados sem inventar algoritmos, critérios de aceitação, taxas de erro ou ferramentas específicas.
- “Não deixando erro chegar ao usuário” foi desenvolvido como finalidade da validação automática. Não afirmar garantia universal de ausência de erros ou cobertura total dos testes.
- Fechamento com entregas, impacto relatado e aprendizados em bullets. Não transformar o resultado em quatro vezes a produtividade, número de demissões ou economia de custos.

- Empresa confirmada pelo autor, mas omitida do corpo e do QMD por decisão expressa em raw:FONTE-262. Relações editoriais internas preservam a página canônica do caso.
