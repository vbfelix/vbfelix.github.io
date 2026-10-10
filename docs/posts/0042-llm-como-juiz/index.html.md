# O juiz que prefere a resposta de cima

Fonte: https://vbfelix.github.io/posts/0042-llm-como-juiz/index.html

### Tanto acordo quanto entre humanos, até trocar a ordem

Em junho de 2023, o GPT-4 concordou com 58 avaliadores humanos em 85% das comparações entre duas respostas de modelos de linguagem. Os humanos concordaram entre si em 81%. A conta deixa os empates de fora ([Zheng et al., *Judging LLM-as-a-Judge with MT-Bench and Chatbot Arena*](https://arxiv.org/abs/2306.05685)).

O mesmo artigo fez um segundo teste. Os autores entregaram ao GPT-4 duas respostas parecidas, geradas pelo mesmo modelo. Pediram a comparação duas vezes, trocando a ordem. O veredito se manteve em 65% dos casos. Em 30%, o juiz favoreceu a resposta que vinha primeiro.

Gosto desse artigo porque ele dá a boa e a má notícia juntas. Um LLM pode concordar com gente tanto quanto gente concorda com gente. E pode trocar de veredito porque a folha estava em cima da pilha.

Um modelo que dá nota é um examinador. E examinador tem vícios.

### Entre o código que só confere regra e o humano que custa caro

LLM como juiz, ou *LLM-as-a-judge* em inglês, é usar um modelo para avaliar a saída de outro. O juiz recebe a resposta e um critério escrito, a rubrica. Devolve um veredito, que pode ser uma nota ou um passa ou falha.

No [post sobre evals](https://vbfelix.github.io/posts/0037-seu-prompt-passou-no-teste-do-olhometro/), o juiz era um dos três jeitos de avaliar uma resposta. Ficava entre o código, que só confere regra, e a revisão humana, que é lenta e cara. Aqui ele é o assunto.

A documentação da Anthropic descreve o juiz como rápido, escalável e adequado a julgamento complexo. A mesma página manda testar a confiabilidade dele antes de escalar ([Anthropic, *Define success criteria and build evaluations*](https://platform.claude.com/docs/en/test-and-evaluate/develop-tests)).

Os autores do artigo de 2023 descrevem três formatos de julgamento:

- **_Pairwise comparison_**, a comparação em pares: o juiz lê uma pergunta e duas respostas. Diz qual é a melhor ou declara empate;
- **_Single answer grading_**, a nota direta: o juiz lê uma resposta só e dá o veredito sobre ela;
- **_Reference-guided grading_**, a nota com referência: o juiz recebe também uma resposta de referência para comparar.

Os dois testes da abertura eram *pairwise comparison*. É o formato que eu usaria para decidir entre duas versões de um *prompt*. Para vigiar uma falha específica, eu usaria o *single answer grading*.

### O examinador tem lugar preferido e gosta de texto comprido

O juiz é outro LLM e leva os vícios dele para a correção. O artigo dá nome a três vieses:

- **Viés de posição**: o juiz favorece uma resposta pelo lugar em que ela aparece. É o teste de troca de ordem da abertura. Dos três juízes testados, GPT-4, GPT-3.5 e Claude-v1, o GPT-4 foi o mais consistente;
- **Viés de verbosidade**: o juiz prefere a resposta mais longa, mesmo quando ela não é melhor. Os autores pegaram 23 respostas com lista numerada e pediram uma reescrita da lista sem informação nova. Depois puseram a reescrita antes da lista original. O GPT-3.5 e o Claude-v1 preferiram a versão inchada em 91,3% dos casos;
- **Autopreferência**: o juiz favorece o texto que ele mesmo gerou. O artigo não teve dados para concluir. No ano seguinte, outro grupo mostrou que modelos como o GPT-4 conseguem distinguir o próprio texto, e ligou essa capacidade à força da preferência ([Panickssery, Bowman e Feng, *LLM Evaluators Recognize and Favor Their Own Generations*](https://arxiv.org/abs/2404.13076)).

O viés de posição tem um agravante. Ele aparece mais quando as duas respostas têm qualidade próxima e quase some quando uma é muito melhor que a outra.

Na minha leitura, isso atinge o uso mais comum do juiz. Quem compara duas versões do mesmo *prompt* quase sempre compara respostas parecidas.

Esses números são de modelos de 2023, com o *prompt* padrão do artigo, e eu não os aplicaria a um juiz de hoje. O viés, porém, voltou a aparecer num estudo maior, com 15 juízes, e de novo dependeu da distância de qualidade entre as respostas ([Shi et al., *Judging the Judges: A Systematic Study of Position Bias in LLM-as-a-Judge*](https://arxiv.org/abs/2406.07791)).

### "A resposta é boa?" é a pergunta que estraga o juiz

Se eu fosse montar um juiz amanhã, tomaria seis decisões antes de escrever o *prompt*:

- **Uma rubrica por juiz**: "a resposta é boa?" deixa o juiz escolher o que olhar. "A resposta promete um prazo que a política não prevê?" tem resposta certa.
- **Veredito binário**: Hamel Husain afirma que as pessoas não sabem o que fazer com um 3 ou um 4 ([Husain, *Using LLM-as-a-Judge For Evaluation: A Complete Guide*](https://hamel.dev/blog/posts/llm-judge/));
- **Justificativa antes do veredito**: o juiz escreve a crítica primeiro e o passa ou falha depois.
- **Exemplos corrigidos por gente**: respostas reais, com o veredito e a crítica de quem entende do assunto, entram no *prompt*. 
- **Ordem trocada no _pairwise comparison_**: o juiz roda duas vezes, com as respostas invertidas. Uma resposta só vence se ganhar nas duas, e veredito que muda vira empate. É a saída conservadora que o próprio artigo propõe;
- **Modelo diferente do avaliado**: a documentação da Anthropic trata como boa prática, e eu vejo nisso a defesa contra a autopreferência.

Para o *single answer grading* de uma falha só, o *prompt* fica assim. O caso é um assistente de atendimento, e a falha é prometer o que a política não dá:

```text
Você vai avaliar a resposta de um assistente de atendimento.

<politica>
O cancelamento sem multa vale até 7 dias após a compra.
</politica>

<pergunta>
{pergunta_do_cliente}
</pergunta>

<resposta>
{resposta_do_assistente}
</resposta>

<rubrica>
A resposta falha se afirmar prazo, valor ou condição que não está na política.
A resposta passa se disser só o que a política diz ou se avisar que não sabe.
</rubrica>

<exemplos>
{respostas_corrigidas_por_uma_pessoa}
</exemplos>

Escreva primeiro a crítica, em até três frases, citando o trecho que pesou.
Depois escreva o veredito em <veredito>: passa ou falha.
```

A pergunta e a resposta mudam a cada chamada. Os exemplos mudam só quando alguém corrige mais respostas. O resto é fixo, e eu versionaria junto com o código da aplicação. Para vigiar outra falha, escreveria outro juiz, com outra rubrica.

### Examinador também faz prova

Já apoiei, com uma equipe, a avaliação de provas da área médica. Havia questões de conhecimento e tarefas práticas, e nas tarefas práticas a nota vinha de examinadores. A pergunta ali era de confiabilidade: dois examinadores dão a mesma nota ao mesmo candidato? Prova em que a nota depende de quem corrige mede o examinador junto com o candidato.

O LLM como juiz é um examinador novo na banca, e eu o trataria do mesmo jeito. Antes de usar o veredito dele:

- Corrigir à mão cerca de 30 respostas reais, com passa ou falha e uma crítica em cada uma. É o ponto de partida de Husain, que segue corrigindo até parar de aparecer falha nova. São menos que as cem respostas que o post sobre evals manda ler, porque o objetivo é outro: lá era descobrir as falhas, aqui é calibrar o juiz de uma delas;
- Rodar o juiz nas mesmas respostas, menos as que viraram exemplo no *prompt*, e ler as discordâncias uma a uma. A taxa de concordância sozinha engana: um juiz que aprova tudo acerta toda resposta boa e nenhuma ruim.
- Repetir em casa os dois testes do artigo: trocar a ordem das respostas e inchar uma resposta sem informação nova. Se o veredito muda, o juiz tem viés de posição ou de verbosidade;
- Refazer a conferência a cada troca do modelo do juiz, porque os vieses variam de um modelo para outro.

Sobra o caso em que você e um colega corrigem as mesmas 30 respostas e discordam. Lembre dos 81% do começo: os avaliadores humanos do artigo discordaram entre si em quase uma de cada cinco comparações. Na minha leitura, quando duas pessoas não chegam ao mesmo veredito, o defeito costuma estar na rubrica. Reescreva a rubrica até vocês concordarem. Só depois vale cobrar o juiz.
