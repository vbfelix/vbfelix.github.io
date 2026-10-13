# SubQ, o analfabeto ganhou um colega que lê o contrato inteiro

Fonte: https://vbfelix.github.io/posts/0044-subq-snap/index.html

### O modelo que não escreve também lê pouco

Em 12 de outubro de 2026, a Subquadratic lançou o SubQ Snap, um modelo de decisão que aceita até 384 mil *tokens* de texto e imagem em uma chamada ([Subquadratic, *Introducing SubQ Snap*](https://subq.ai/introducing-subq-snap)). No mesmo anúncio, a empresa afirma que os modelos de decisão de hoje param em 32 mil *tokens*.

Para a Subquadratic, quem para em 32 mil decide sem a maior parte do que precisaria ver. A promessa do Snap é ler de uma vez um repositório de código ou um contrato com os anexos.

Fiquei animado com o anúncio, e o motivo passa longe do placar. Os números deste texto sobre o Snap e o Jev vêm todos da Subquadratic, e eu leio cada um como alegação de quem vende.

O que me animou foi o problema que o lançamento expõe. Um modelo que não escreve ainda precisa ler.

### O analfabeto resolveu metade da conta

Escrevi sobre o Jev, da TypeSafe, em [Jev, o modelo analfabeto](https://vbfelix.github.io/posts/0033-jev-e-modelos-de-decisao/). Um modelo de decisão recebe um contexto, uma pergunta e uma lista fechada de respostas. Em vez de escrever um texto, devolve uma probabilidade para cada resposta.

O apelido vinha daí: o Jev não escreve. Fechei aquele texto perguntando por que estamos pedindo texto a um modelo quando o sistema só precisa de uma decisão.

A pergunta continua boa. Ela só olha para um lado da chamada.

Toda chamada a um modelo tem dois custos. O de ler é processar a entrada inteira antes de qualquer resposta. O de responder é produzir a saída, que num LLM comum é texto gerado *token* a *token*.

O modelo de decisão praticamente zera o segundo, porque não gera texto. O primeiro fica onde estava. O anúncio do Snap diz isso sem rodeio: tirar a geração remove o custo da saída e mantém o de ler a entrada ([Subquadratic](https://subq.ai/introducing-subq-snap)).

A tabela de preços conta a mesma história. O Snap cobra US$ 0,05 por milhão de *tokens* de entrada e nada pela saída.

Sobrou a leitura.

### Escolher pode exigir ler o repositório

Resposta curta não garante contexto curto. A escolha entre três ferramentas pode depender de evidência espalhada por um repositório inteiro ou por um histórico longo de interações. O anúncio lista outros casos: a restrição que o usuário deu 40 turnos atrás, a função definida em outro módulo e a cláusula que está em outro documento ([Subquadratic](https://subq.ai/introducing-subq-snap)).

É aí que a conta aperta.

No contexto longo, a leitura corrói a economia que o modelo de decisão fez ao não escrever. A Subquadratic diz que é por isso que os modelos de decisão ficaram nos 32 mil *tokens*.

Quem precisava decidir sobre um repositório ou um contrato inteiro tinha duas saídas:

 - **Ler tudo**: mandar o material inteiro em uma chamada e pagar a conta maior, em tempo e em dinheiro;
 - **Reduzir a entrada**: truncar, resumir, recuperar só os trechos que parecem relevantes ou fatiar o texto em pedaços, o *chunking*.

A segunda saída esconde uma decisão. Truncar, resumir, recuperar e fatiar escolhem, antes do modelo, o que ele vai ver e o que não vai ([Subquadratic](https://subq.ai/introducing-subq-snap)). E cada corte perde de um jeito. O truncamento pode levar embora a restrição dita 40 turnos atrás. A recuperação pode deixar de fora a evidência relevante. O resumo pode apagar o detalhe que só depois se mostra importante.

O modelo de decisão decide sobre o que sobrou. A primeira decisão do fluxo já foi tomada, e quem tomou foi o corte.

O Snap é a aposta numa terceira saída: ler tudo sem pagar a conta maior. E o que sustenta a aposta se chama *Subquadratic Sparse Attention* (SSA). A empresa afirma que, com ela, o custo da atenção cresce de forma linear com o contexto, e explica a ideia em [*How SSA Makes Long Context Practical*](https://subq.ai/how-ssa-makes-long-context-practical).

### O Jev fez a prova com o livro rasgado

O teste que mais me interessou no anúncio se chama Decision LCR. A Subquadratic adaptou um *benchmark* de contexto longo da Artificial Analysis: 100 perguntas escritas por pessoas sobre conjuntos de documentos reais, cada conjunto com 72 mil a 115 mil *tokens*. As perguntas viraram decisões de múltipla escolha ([Subquadratic](https://subq.ai/introducing-subq-snap)).

Nenhum conjunto cabe em 32 mil *tokens*. O Snap leu cada um inteiro, em uma chamada, e o Jev leu em *chunks*. O resultado: **o Snap marcou 74,0 lendo os documentos inteiros, e o Jev, 48,0 lendo em pedaços**.

São 26 pontos. Para ter a escala: sem documento nenhum, os modelos marcam entre 24 e 25 nesse teste, e o chute puro dá 19,2.

Só que esse número mede duas coisas de uma vez. Uma é a diferença entre os dois modelos. A outra é a diferença entre ler tudo e ler fatiado. O teste não separa uma da outra.

O anúncio também não descreve como os documentos foram fatiados para o Jev, nem como as respostas dos pedaços viraram uma decisão só. Outro fatiamento poderia dar outro placar.

Por isso não leio os 26 pontos como prova de que o Snap raciocina melhor. Leio como um indício do que se perde quando alguém corta o texto antes de o modelo decidir. E quem escolheu a tesoura foi a concorrente do Jev.

No contexto curto, a distância é bem menor, ali o Snap marca 61,7 e o Jev, 57,9.

As duas réguas são diferentes, e não dá para subtrair uma da outra. Ainda assim, a vantagem grande aparece justamente onde um dos dois precisou cortar a entrada.

O placar também tem derrota. Em 16 testes do Decision Index que a empresa associa a fluxos corporativos, o Snap lidera em 13. Dos três que perde, dois são por menos de 2 pontos. O terceiro é o When2Call: 67,0 contra 74,6 do Jev.

Esse teste mede uma decisão: chamar a ferramenta, perguntar, responder ou recusar. O anúncio desenha quatro papéis para o Snap dentro de um agente. O primeiro é o de roteador, que decide entre chamar uma ferramenta, perguntar ao usuário ou responder direto.

Antes de levar esse placar para uma decisão de arquitetura, eu anotaria quatro ressalvas:

 - **Quem mediu**: todos os números vêm do anúncio da Subquadratic, e o teste de contexto longo foi adaptado por ela;
 - **Custo linear**: é a alegação da empresa para o SSA;
 - **Acesso**: o Snap está em prévia pública, com capacidade limitada, e a empresa avisa que pode abrir lista de espera;
 - **Calibração**: no post do Jev, cobrei que a confiança do modelo fosse testada nos dados de quem adota, isso não muda aqui.

Nenhuma dessas ressalvas derruba a ideia. Elas mudam o que eu faria com ela.

### Antes de escolher o modelo, conte o que ele precisa ler

Ao desenhar uma etapa de decisão, eu começaria por duas contas: onde está a evidência e quanto dela chega ao modelo.

Se a evidência cabe na janela do modelo de decisão que você já usa, o que escrevi no post do Jev continua valendo. Classifique antes de gastar e confira a calibração nos seus dados.

Quando ela não cabe, existe um corte antes do modelo, e esse corte já é uma decisão. Trocar de modelo é agora mais uma opção.
