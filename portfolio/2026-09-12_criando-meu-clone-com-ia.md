---
tipo: portfolio
data: '2026-09-12'
status: draft
titulo: Criando meu clone com IA
site:
  categories: [AI, product]
  lang: pt-BR
episodio: episodio/segundo-cerebro-de-produto
tese: tese/o-salto-vem-do-sistema-nao-do-prompt
referencias: [referencia/karpathy-llm-wiki]
fatos:
- fato/horas-de-construcao-do-segundo-cerebro-de-produto
- fato/perguntas-respondidas-na-construcao-do-segundo-cerebro-de-produto
fontes: [FONTE-265, FONTE-266, FONTE-048]
---

## Brief

Relato de construção de um segundo cérebro para apoiar o time de produto quando o autor não estivesse disponível. Foco em curadoria, regras de procedência, estrutura de raciocínio, personalidade e especialização por skills. Público de produto e dados. O relato atual complementa os episódios já publicados sobre construção e teste. Empresa e ferramenta não são nomeadas no corpo. O título usa clone como metáfora, sem promessa de reprodução integral do autor.

## Portfolio

Na época da Copa, brincamos na empresa que precisávamos de um Vini Jr. Um clone meu para responder às perguntas e apoiar o time quando eu não estivesse disponível.

Tínhamos várias fontes de conhecimento, mas faltavam centralidade e curadoria. Um dos desafios era saber como consultar aquele conteúdo e estruturar as respostas. A informação existia; organizar seu uso fazia parte do problema.

Foi dessa brincadeira que veio a ideia de salpicar uma wiki com a minha personalidade e tentar criar meu clone com inteligência artificial (IA). Criticismo, ceticismo extremo e uma pitada de chatice. Essa parte funcionou: às vezes, a quantidade de perguntas irritava.

### Um fim de semana e alguns vestibulares

Quando conheci o conceito de [LLM Wiki, apresentado por Andrej Karpathy](https://gist.github.com/karpathy/442a6bf555914893e9891c11519de94f), fiquei maravilhado. LLM é a sigla em inglês para grande modelo de linguagem, o tipo de modelo usado para compreender e gerar texto. A proposta era usar esses modelos para construir e manter uma base de conhecimento organizada, com páginas ligadas entre si e alimentadas por fontes.

O que me atraía era organizar informação para a própria IA consultar. Eu queria consultas mais rápidas, com menor gasto de tokens, as unidades de texto processadas pelo modelo, e menos alucinações, respostas que parecem plausíveis, mas não têm sustentação.

Meu primeiro segundo cérebro foi para a empresa em que eu trabalhava. O maior desafio estava na entrada: selecionar e incorporar conteúdo curado, lidar com múltiplas fontes e resolver muitas divergências.

Foram **35 horas de trabalho em um fim de semana e mais de 500 perguntas respondidas**. Acredito que fiz alguns vestibulares ali.

O acervo exigia decisões. O fato de uma informação estar escrita não encerrava a discussão sobre o que ela significava ou como deveria ser usada. Para organizar esse trabalho, impus algumas regras:

- **Perguntas críticas bloqueavam conteúdo.** Certas lacunas precisavam ser respondidas antes de permitir o avanço.
- **Consultar a internet era proibido.** O trabalho precisava se apoiar nas fontes fornecidas.
- **Divergências e respostas precisavam de dono declarado.** Era necessário identificar quem respondia por elas.

A construção da base exigia minha participação justamente nos pontos em que havia ambiguidade. O volume de perguntas fazia parte desse esforço de curadoria.

### O clone precisava saber por onde pensar

A wiki, por si só, não foi suficiente. Eu queria que o time tivesse acesso também à forma como eu pensava a empresa.

Passei para ela uma estrutura de entidades e ligações. As entidades eram os elementos que eu distinguia naquele contexto; as ligações registravam como eles se relacionavam. Essa organização servia de base para orientar a consulta.

Dependendo da pergunta, eu definia por onde começar. A partir das relações, indicava por onde seguir e até onde chegar. O conhecimento ganhava caminhos de investigação.

Isso fazia diferença para o que eu estava tentando construir. Ter conteúdo disponível atendia à necessidade de memória. Para apoiar o trabalho do time, eu também precisava explicitar como usava aquele conteúdo para examinar uma questão.

Meu critério precisava aparecer na estrutura. A IA teria de conseguir percorrer relações que, para mim, faziam parte do entendimento da empresa.

### Um pouco de mim, inclusive a chatice

A brincadeira com o Vini Jr. deu nome ao tipo de apoio que queríamos. Usei minhas instruções pessoais e a memória do meu chat para trazer características do meu comportamento: criticismo, ceticismo extremo e disposição para questionar.

A personalidade tinha uma função no trabalho. Eu queria que ela examinasse o motivo por trás de um pedido e insistisse nos pontos que precisavam ser esclarecidos. Às vezes, isso significava fazer perguntas suficientes para irritar quem estava do outro lado.

O objetivo era não deixar a pessoa seguir sem o motivo correto.

Esse comportamento complementava as regras da base. Havia conhecimento organizado para consultar, caminhos para investigar e uma postura de questionamento diante do que ainda precisava de fundamento.

### Especializar para trabalhar com o time

Criei também **skills**, conjuntos de instruções para executar operações específicas. Nesse caso, eram voltadas aos processos do trabalho de produto.

Eu definia o formato da entrega e o aprofundamento esperado em cada operação. Isso permitia padronizar o trabalho do time e organizar a forma de atuar com aquele conhecimento.

O segundo cérebro passou a combinar:

- **Uma base curada**, com fontes e divergências tratadas.
- **Uma estrutura de consulta**, com entidades, relações e caminhos definidos conforme a pergunta.
- **Um comportamento crítico**, que questionava motivos e premissas.
- **Instruções para operações de produto**, com formato e profundidade controlados.

Era essa combinação que aproximava a ferramenta do apoio que eu queria oferecer quando não estivesse disponível.

### Quando coloquei meu clone à prova

Em um teste, peguei um projeto que eu mesmo tinha desenhado e escrito. Trouxe uma pessoa do time para analisá-lo comigo e com a IA.

Eu perdi para ela.

A IA recuperou decisões antigas, restrições e premissas que eu já não tinha na cabeça. Apontou possíveis bloqueios, dependências e implicações que não estavam explícitos. Também conectou informações que eu conhecia separadamente.

O desconforto estava em reconhecer que boa parte daquele conhecimento tinha saído de mim. Eu conhecia os pedaços, mas não tinha feito todas aquelas ligações ao examinar o projeto.

Foi uma demonstração concreta do que eu buscava: um sistema capaz de usar o contexto que eu havia organizado para questionar o trabalho, inclusive o meu.

### Aprendizados, limites e principais impactos

Não sei se foi a melhor aplicação possível de uma wiki. Foi uma tentativa de clonagem que apoiou meu time e, naquele teste, trouxe pontos que eu havia deixado passar.

Da construção, ficaram alguns aprendizados:

- **A curadoria exigiu julgamento.** As horas e as perguntas fizeram parte do trabalho de organizar fontes, esclarecer divergências e atribuir responsabilidade pelas respostas.
- **Conhecimento disponível não bastou para o apoio que eu queria.** Precisei explicitar relações, caminhos de consulta e critérios que usava para pensar a empresa.
- **A personalidade precisava servir ao trabalho.** O ceticismo e as perguntas insistentes tinham o objetivo de exigir fundamento antes do avanço.
- **A especialização ajudou a padronizar a atuação.** As skills definiam operações de produto, formato de entrega e profundidade.
- **O teste mostrou o valor de recuperar e conectar contexto.** A IA levantou questões que eu não havia levantado no meu próprio projeto.

Talvez meu clone não fosse tão criativo. Mas, naquele teste, lembrou do que eu tinha esquecido e fez ligações que eu deixei passar. Era esse tipo de apoio que eu queria tornar disponível para o time.

## Evidências

- raw:FONTE-265: mensagem de Vinícius Félix em 2026-09-12, arquivada integralmente. Sustenta inspiração em Karpathy, primeiro segundo cérebro para a empresa, curadoria, divergências, 35 horas em um fim de semana, mais de 500 perguntas, regras, estrutura de entidades e percursos, personalidade a partir das instruções e da memória do chat, skills de produto, padronização e apoio ao time.
- raw:FONTE-266: complemento do autor em 2026-09-12. Sustenta falta de centralidade e curadoria apesar das várias fontes, desafio de consulta e estruturação dos retornos, brincadeira com Vini Jr. na época da Copa e sua ligação à decisão de incorporar personalidade. Não identifica edição da Copa ou data do fim de semana.
- raw:FONTE-048: relato do teste, entregue em 2026-09-02. Sustenta projeto desenhado e escrito pelo autor, participação de uma pessoa do time, recuperação de decisões, restrições e premissas, e achados de bloqueios, dependências e ligações. A comparação é qualitativa e não controlada.
- Referência Karpathy: autoria e conteúdo conferidos no gist em 2026-09-12. Vinícius confirmou nominalmente o endereço nesta conversa na mesma data. A descrição geral da wiki é apoiada por essa referência; não se atribuem a Karpathy as regras particulares adotadas no projeto.
- Os dois fatos declarados registram esforço de construção. A formulação anterior “mais de 30 horas”, no post de 27/08/2026, é compatível com 35 horas e foi preservada na precedência do fato.
- **Não documentado.** Data do fim de semana de construção, Q-067. A data editorial não é a data da execução.
- Rapidez, redução de tokens e de alucinações são objetivos do autor, sem métricas comparativas fornecidas. “Ela não esquece” foi situado no teste e na recuperação do contexto, sem garantia universal de memória ou correção.
- O relato não identifica nomes das skills, formatos concretos das entregas ou regras exatas de percurso. O corpo preserva o nível de detalhe fornecido, sem inventar implementação.
- Destino local: posts/portfolio/0040-criando-meu-clone-com-ia/index.qmd. Destino no site: posts/0040-criando-meu-clone-com-ia/index.qmd. Numeração local conferida até 0039; clone de consulta do site até 0031.
- Sem capa ou assets. Validação estrutural; Quarto indisponível no ambiente.

## Notes

- Título fornecido pelo autor preservado. Clone permanece metáfora de apoio ao time, sem alegação de substituição integral.
- Artigo desenvolvido a partir do relato atual, com o teste já arquivado como exemplo concreto. A história do amigo e da primeira tentativa não foi repetida, para manter o foco na construção do clone.
- Empresa e ferramenta não nomeadas no corpo. A proibição de internet é regra do projeto narrado, não restrição à apuração editorial desta conversa.
- Wiki realimentada: episódios de construção e teste, duas teses canônicas, dois fatos novos e referência confirmada. Nenhuma entidade nova para duplicar os episódios existentes.
- Rascunho preparado em 2026-09-12; sem publicação, commit ou push.
