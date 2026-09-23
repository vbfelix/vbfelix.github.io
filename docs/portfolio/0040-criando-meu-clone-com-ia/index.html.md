# Criando meu clone com IA

Fonte: https://vbfelix.github.io/portfolio/0040-criando-meu-clone-com-ia/index.html

![Esquema conceitual em lousa verde: documentos passam por uma etapa de curadoria e formam uma rede de conhecimento ligada a um assistente, que devolve perguntas ao examinar um projeto.](https://vbfelix.github.io/portfolio/0040-criando-meu-clone-com-ia/thumbnail.svg)

Na época da Copa, brincamos na empresa que precisávamos de um Vini Jr. Um clone meu para responder às perguntas e apoiar o time quando eu não estivesse disponível.

Tínhamos várias fontes de conhecimento, mas faltavam centralidade e curadoria. Um dos desafios era saber como consultar aquele conteúdo e estruturar as respostas. A informação existia; organizar seu uso fazia parte do problema.

Foi dessa brincadeira que veio a ideia de salpicar uma wiki com a minha personalidade e tentar criar meu clone com inteligência artificial (IA). Criticismo, ceticismo extremo e uma pitada de chatice. Essa parte funcionou: às vezes, a quantidade de perguntas irritava.

# Um fim de semana e alguns vestibulares

Quando conheci o conceito de [LLM Wiki, apresentado por Andrej Karpathy](https://gist.github.com/karpathy/442a6bf555914893e9891c11519de94f), fiquei maravilhado. LLM é a sigla em inglês para grande modelo de linguagem, o tipo de modelo usado para compreender e gerar texto. A proposta era usar esses modelos para construir e manter uma base de conhecimento organizada, com páginas ligadas entre si e alimentadas por fontes.

O que me atraía era organizar informação para a própria IA consultar. Eu queria consultas mais rápidas, com menor gasto de tokens, as unidades de texto processadas pelo modelo, e menos alucinações, respostas que parecem plausíveis, mas não têm sustentação.

Meu primeiro segundo cérebro foi para a empresa em que eu trabalhava. O maior desafio estava na entrada: selecionar e incorporar conteúdo curado, lidar com múltiplas fontes e resolver muitas divergências.

Foram **35 horas de trabalho em um fim de semana e mais de 500 perguntas respondidas**. Acredito que fiz alguns vestibulares ali.

O acervo exigia decisões. O fato de uma informação estar escrita não encerrava a discussão sobre o que ela significava ou como deveria ser usada. Para organizar esse trabalho, impus algumas regras:

- **Perguntas críticas bloqueavam conteúdo.** Certas lacunas precisavam ser respondidas antes de permitir o avanço.
- **Consultar a internet era proibido.** O trabalho precisava se apoiar nas fontes fornecidas.
- **Divergências e respostas precisavam de dono declarado.** Era necessário identificar quem respondia por elas.

A construção da base exigia minha participação justamente nos pontos em que havia ambiguidade. O volume de perguntas fazia parte desse esforço de curadoria.

# O clone precisava saber por onde pensar

A wiki, por si só, não foi suficiente. Eu queria que o time tivesse acesso também à forma como eu pensava a empresa.

Passei para ela uma estrutura de entidades e ligações. As entidades eram os elementos que eu distinguia naquele contexto; as ligações registravam como eles se relacionavam. Essa organização servia de base para orientar a consulta.

Dependendo da pergunta, eu definia por onde começar. A partir das relações, indicava por onde seguir e até onde chegar. O conhecimento ganhava caminhos de investigação.

Isso fazia diferença para o que eu estava tentando construir. Ter conteúdo disponível atendia à necessidade de memória. Para apoiar o trabalho do time, eu também precisava explicitar como usava aquele conteúdo para examinar uma questão.

Meu critério precisava aparecer na estrutura. A IA teria de conseguir percorrer relações que, para mim, faziam parte do entendimento da empresa.

# Um pouco de mim, inclusive a chatice

A brincadeira com o Vini Jr. deu nome ao tipo de apoio que queríamos. Usei minhas instruções pessoais e a memória do meu chat para trazer características do meu comportamento: criticismo, ceticismo extremo e disposição para questionar.

A personalidade tinha uma função no trabalho. Eu queria que ela examinasse o motivo por trás de um pedido e insistisse nos pontos que precisavam ser esclarecidos. Às vezes, isso significava fazer perguntas suficientes para irritar quem estava do outro lado.

O objetivo era não deixar a pessoa seguir sem o motivo correto.

Esse comportamento complementava as regras da base. Havia conhecimento organizado para consultar, caminhos para investigar e uma postura de questionamento diante do que ainda precisava de fundamento.

# Especializar para trabalhar com o time

Criei também **skills**, conjuntos de instruções para executar operações específicas. Nesse caso, eram voltadas aos processos do trabalho de produto.

Eu definia o formato da entrega e o aprofundamento esperado em cada operação. Isso permitia padronizar o trabalho do time e organizar a forma de atuar com aquele conhecimento.

O segundo cérebro passou a combinar:

- **Uma base curada**, com fontes e divergências tratadas.
- **Uma estrutura de consulta**, com entidades, relações e caminhos definidos conforme a pergunta.
- **Um comportamento crítico**, que questionava motivos e premissas.
- **Instruções para operações de produto**, com formato e profundidade controlados.

Era essa combinação que aproximava a ferramenta do apoio que eu queria oferecer quando não estivesse disponível.

# Quando coloquei meu clone à prova

Em um teste, peguei um projeto que eu mesmo tinha desenhado e escrito. Trouxe uma pessoa do time para analisá-lo comigo e com a IA.

Eu perdi para ela.

A IA recuperou decisões antigas, restrições e premissas que eu já não tinha na cabeça. Apontou possíveis bloqueios, dependências e implicações que não estavam explícitos. Também conectou informações que eu conhecia separadamente.

O desconforto estava em reconhecer que boa parte daquele conhecimento tinha saído de mim. Eu conhecia os pedaços, mas não tinha feito todas aquelas ligações ao examinar o projeto.

Foi uma demonstração concreta do que eu buscava: um sistema capaz de usar o contexto que eu havia organizado para questionar o trabalho, inclusive o meu.

# Aprendizados, limites e principais impactos

Não sei se foi a melhor aplicação possível de uma wiki. Foi uma tentativa de clonagem que apoiou meu time e, naquele teste, trouxe pontos que eu havia deixado passar.

Da construção, ficaram alguns aprendizados:

- **A curadoria exigiu julgamento.** As horas e as perguntas fizeram parte do trabalho de organizar fontes, esclarecer divergências e atribuir responsabilidade pelas respostas.
- **Conhecimento disponível não bastou para o apoio que eu queria.** Precisei explicitar relações, caminhos de consulta e critérios que usava para pensar a empresa.
- **A personalidade precisava servir ao trabalho.** O ceticismo e as perguntas insistentes tinham o objetivo de exigir fundamento antes do avanço.
- **A especialização ajudou a padronizar a atuação.** As skills definiam operações de produto, formato de entrega e profundidade.
- **O teste mostrou o valor de recuperar e conectar contexto.** A IA levantou questões que eu não havia levantado no meu próprio projeto.

Talvez meu clone não fosse tão criativo. Mas, naquele teste, lembrou do que eu tinha esquecido e fez ligações que eu deixei passar. Era esse tipo de apoio que eu queria tornar disponível para o time.
