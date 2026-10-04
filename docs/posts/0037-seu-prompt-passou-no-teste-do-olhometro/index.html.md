# Seu prompt passou no teste do olhômetro

Fonte: https://vbfelix.github.io/posts/0037-seu-prompt-passou-no-teste-do-olhometro/index.html

### Um modelo que tirou 42 e merecia 95

Em janeiro de 2026, a Anthropic contou que o Claude Opus 4.5 marcou 42% no CORE-Bench, um *benchmark* que pede a um agente para reproduzir resultados de artigos científicos. Depois de corrigir o teste, a nota foi para 95% ([Anthropic, *Demystifying evals for AI agents*](https://www.anthropic.com/engineering/demystifying-evals-for-ai-agents)).

O modelo não mudou. O avaliador mudou. Ele reprovava a resposta "96.12" porque esperava "96.124991…". Algumas tarefas eram ambíguas. Outras não davam o mesmo resultado duas vezes nem com a solução certa.

Gosto desse caso porque ele resume o assunto inteiro. Avaliar um LLM é medir um sistema que não responde igual duas vezes, com uma régua que também pode estar torta.

### Teste unitário para quem não repete a resposta

Algo determinístico implica que a mesma entrada produz a mesma saída. O teste é passa ou falha, e roda igual mil vezes. Com LLM, trocar uma letra no prompt pode mudar a resposta, e o teste manual com as perguntas favoritas só diz se a saída "parece" melhor ([Matt Pocock, *Your App Is Only As Good As Its Evals*](https://www.aihero.dev/what-are-evals)).

Eval é o teste automatizado desse tipo de sistema. Ele roda um conjunto fixo de casos, dá nota a cada resposta e devolve um número que você compara entre versões. Um *benchmark* mede o modelo numa prova pública. O eval mede a sua aplicação nos seus casos. O artigo da Anthropic dá nome às peças. Vale guardar os nomes, porque toda ferramenta usa algum deles:

- **_Task_**, a tarefa: um caso de teste, com entrada e critério de sucesso definidos;
- **_Trial_**, a tentativa: uma execução da tarefa. Como a saída varia, a mesma tarefa roda várias vezes;
- **_Grader_**, o avaliador: a lógica que dá nota a algum aspecto da resposta. Uma tarefa pode ter vários;
- **_Transcript_**: o registro completo da tentativa, com saída, chamadas de ferramenta e passos intermediários;
- **_Outcome_**: o estado final do mundo depois da tentativa. O agente disse que reservou o voo, mas a reserva existe no banco?;
- **_Harness_**: a infraestrutura que roda tudo de ponta a ponta. Entrega a tarefa, executa as tentativas em paralelo, grava os *transcripts*, chama os *graders* e soma as notas.

A diferença entre *transcript* e *outcome* parece detalhe e não é. O *transcript* é o que o agente contou. O *outcome* é o que de fato aconteceu.

### Três juízes, nenhum perfeito

A peça que mais pesa no desenho é o *grader*. Existem três famílias ([Anthropic](https://www.anthropic.com/engineering/demystifying-evals-for-ai-agents); [LangChain, *LLM Evals*](https://www.langchain.com/resources/llm-evals); [Matt Pocock, *The Three Types Of Evals*](https://www.aihero.dev/three-types-of-evals)):

- **Código**: comparação de texto, validação de JSON, teste de unidade sobre o código que o modelo escreveu, consulta ao banco para conferir o *outcome*. É rápido, barato e reproduzível. Quebra quando a resposta certa vem num formato que você não previu, como o "96.12" do CORE-Bench;
- **Modelo**: outro LLM lê a resposta com uma rubrica e dá a nota. É o chamado LLM como juiz. Cobre critério aberto, como tom, fidelidade à fonte e utilidade. Custa e demora mais;
- **Humano**: um especialista revisa uma amostra. É o padrão de qualidade e é o que calibra os outros dois. É lento, caro e não escala.

A regra prática que tiro disso: use código para tudo que dá para verificar com código.

O Matt Pocock repete um caso contado por Ian Webster, o do Clyde, um bot do Discord, cujas respostas eram checadas para começar sempre com letra minúscula, imitando um usuário jovem. É o *grader* mais bobo que existe, uma linha de código. E pega uma regressão real de estilo antes de o usuário ver.

O LLM como juiz merece um cuidado a mais. A documentação da Anthropic recomenda usar como juiz um modelo diferente do avaliado ([Anthropic, *Create strong empirical evaluations*](https://platform.claude.com/docs/en/test-and-evaluate/develop-tests)). O viés tem medição: um LLM avaliador tende a dar nota mais alta ao texto que ele mesmo gerou, enquanto anotadores humanos consideram os textos equivalentes ([Panickssery, Bowman e Feng, *LLM Evaluators Recognize and Favor Their Own Generations*](https://arxiv.org/abs/2404.13076)).

O juiz precisa ser medido. Se o mesmo *transcript* recebe nota diferente a cada rodada, a nota não serve para detectar regressão. E consistência não é acerto: um juiz pode errar sempre do mesmo jeito. É a comparação com a nota humana que mostra se ele acerta.

Só que a taxa de concordância sozinha engana. Suponha 100 respostas, das quais 57 são boas. Um juiz que aprova tudo concorda com o humano em 57% dos casos e não pega nenhuma das 43 ruins. Por isso eu olharia dois números separados: quantas respostas ruins o juiz reprovou e quantas boas ele reprovou sem motivo.

Na minha leitura, vale conferir ainda o que o juiz recebe para ler. Um juiz que lê só o *transcript* julga o que o agente contou. Para julgar o que aconteceu, ele precisa receber também o *outcome*.

A Confident AI, que mantém o DeepEval, organiza dezenas de métricas prontas desse tipo, como *faithfulness*, que confere se cada afirmação da resposta está na fonte recuperada, e *answer relevancy*, que mede quanto da resposta responde de fato à pergunta ([Confident AI, *LLM Evaluation Metrics*](https://www.confident-ai.com/blog/llm-evaluation-metrics-everything-you-need-for-llm-evaluation)).

Métricas usadas anteriormente para tradução e resumo, como BLEU e ROUGE, medem apenas palavras em comum com uma resposta de referência. Não percebem uma resposta certa escrita com outras palavras, nem uma errada que repete as palavras certas. Eu ainda as usaria como alarme barato de que a saída mudou entre duas versões. Como nota de qualidade, não servem.

### Capacidade e regressão não são a mesma nota

O artigo da Anthropic separa as suítes pela pergunta que cada uma responde:

- **Capacidade**: o que o sistema ainda não faz bem? A suíte começa com taxa de acerto baixa, de propósito. Se ela já nasce com 100%, não tem nada para ensinar;
- **Regressão**: o que o sistema já fazia continua funcionando? Qualquer queda é sinal para investigar.

Quando a capacidade satura perto do teto, ela vira caso de regressão e passa a rodar a cada mudança.

A LangChain acrescenta outro eixo, o momento em que o eval roda ([LangChain](https://www.langchain.com/resources/llm-evals)). O eval **_offline_** roda antes do *deploy*, sobre um conjunto curado em que você conhece a resposta boa. O eval **_online_** roda sobre o tráfego de produção, em que não existe resposta de referência, e serve para pegar a deriva lenta do comportamento e problema de segurança. O *offline* diz se a versão nova pode subir. O *online* diz se a que subiu continua boa.

### Acertar uma vez não é acertar sempre

Como a saída de um LLM varia, uma tarefa roda várias vezes. Aí surge a pergunta: o que conta como sucesso? A Anthropic usa duas métricas, e elas andam em sentidos opostos:

- **_pass@k_**: a probabilidade de pelo menos uma das *k* tentativas acertar. Sobe quando *k* cresce;
- **_pass^k_**: a probabilidade de todas as *k* tentativas acertarem. Cai quando *k* cresce.

Um agente que acerta 75% das vezes, avaliado em três tentativas, tem *pass^3* de 0,75 × 0,75 × 0,75, cerca de 42%. A conta supõe tentativas independentes, e num teste real algumas tarefas são sempre fáceis e outras sempre difíceis.

Na minha leitura, a escolha depende da aplicação, não do gosto. Um assistente que gera cinco sugestões de código para o desenvolvedor escolher precisa de no mínimo uma ótima entre as cinco, e *pass@k* mede isso. Um agente de atendimento que cancela um pedido precisa acertar toda vez, e *pass^k* mede isso. Se não há verificação nem confirmação humana antes da ação, reportar *pass@k* para esse agente é escolher a métrica que faz o número parecer bonito.

### Vinte linhas para aposentar o olhômetro

O DeepEval é uma biblioteca em Python que transforma isso em algo parecido com o `pytest`. A unidade é o `LLMTestCase`, com a pergunta, a resposta da sua aplicação e, se a aplicação usa RAG (busca documentos antes de responder), os documentos que ela recuperou. No exemplo o contexto está fixo para caber na tela. No teste de verdade, ele vem do seu recuperador. Cada métrica devolve uma nota de 0 a 1 com uma justificativa, e o `threshold` define a nota mínima para o caso passar ([DeepEval, *Evaluation Introduction*](https://deepeval.com/docs/evaluation-introduction)).

```python
import pytest
from deepeval import assert_test
from deepeval.test_case import LLMTestCase
from deepeval.metrics import AnswerRelevancyMetric, FaithfulnessMetric

casos = [
    LLMTestCase(
        input="Qual o prazo para cancelar sem multa?",
        actual_output=minha_app("Qual o prazo para cancelar sem multa?"),
        retrieval_context=["O cancelamento sem multa vale até 7 dias após a compra."],
    ),
]

@pytest.mark.parametrize("caso", casos)
def test_atendimento(caso):
    assert_test(caso, [
        AnswerRelevancyMetric(threshold=0.7),
        FaithfulnessMetric(threshold=0.8),
    ])
```

A função `minha_app` é a sua aplicação. As duas métricas são LLM como juiz por baixo: chamam um modelo para julgar. O caso só passa se as duas passarem. E o arquivo roda com um comando, o que permite colocá-lo na CI, a esteira que testa cada mudança antes do *merge*:

```bash
deepeval test run test_atendimento.py
```

A métrica pronta serve para começar. O `threshold` de 0,7 é arbitrário até você comparar a nota com o seu próprio julgamento em casos reais. Hamel Husain e Shreya Shankar são diretos nesse ponto: métrica genérica usada como medida de qualidade cria falsa confiança, e nota boa nela não quer dizer que o sistema funciona ([Husain e Shankar, *AI Evals: Everything You Need to Know*](https://hamel.dev/blog/posts/evals-faq/)). Eles defendem o juiz binário, com veredito de passa ou falha, no lugar da escala de 1 a 5, em que a diferença entre um 3 e um 4 muda de um anotador para outro.

O OpenAI Evals segue a mesma ideia com outro formato. Os casos ficam num arquivo JSON e os parâmetros num YAML. Um registro aberto reúne evals prontos, inclusive com juiz por modelo ([openai/evals no GitHub](https://github.com/openai/evals)). Para mim, a ferramenta importa menos que o conjunto de casos. Um bom conjunto costuma migrar de uma ferramenta para outra sem drama. Um conjunto ruim continua ruim em qualquer uma.

### Quando quem reprova é o avaliador

O caso do CORE-Bench não é isolado. O mesmo artigo da Anthropic conta mais dois:

- **Terminal-Bench**, um *benchmark* de tarefas no terminal: tarefas pediam ao agente que escrevesse um *script* sem dizer em que pasta salvar. O *grader* esperava uma pasta específica. O agente reprovava por ambiguidade da tarefa, não por incapacidade;
- **METR**, uma organização que mede o horizonte de tarefas dos agentes: algumas tarefas pediam ao agente que passasse de uma nota mínima. O *grader* penalizava o modelo que parava ao atingir a nota pedida e premiava o que ignorava a instrução.

Nos três casos, o número dizia uma coisa sobre o modelo e a verdade estava no avaliador. As recomendações do artigo para evitar isso são diretas:

- **Leia os _transcripts_**: sem ler as tentativas e as notas de muitas rodadas, você não sabe se a falha é erro do agente ou solução válida rejeitada pelo *grader*;
- **Avalie o _outcome_, não o caminho**: agentes acham caminhos que quem escreveu o eval não previu. Um *grader* que exige a sequência exata de passos reprova solução certa;
- **Escreva tarefas sem ambiguidade**: dois especialistas, sozinhos, devem chegar ao mesmo veredito de passa ou falha. Para cada tarefa, tenha uma solução de referência que prove que ela tem solução;
- **Desconfie do 0%**: um modelo de fronteira com 0% em 100 tentativas quase sempre indica tarefa quebrada, não agente incapaz;
- **Equilibre os casos**: teste onde o comportamento deve acontecer e onde ele não deve. Uma suíte só com pedidos que o agente deve recusar premia o agente que recusa tudo;
- **Isole cada tentativa**: cada *trial* começa de um ambiente limpo. Estado compartilhado entre tentativas produz falhas em série que são da infraestrutura, não do modelo.

Faço uma ressalva a "avalie o *outcome*, não o caminho". Não exigir a sequência de passos é diferente de ignorar o caminho. Um agente pode chegar ao *outcome* certo depois de emitir o mesmo reembolso duas vezes e desfazer um deles. Eu manteria um *grader* de código para a ação que nunca pode acontecer.

A LangChain dá o conselho que mais uso para avaliar: nomeie a falha antes de dar nota a ela ([LangChain](https://www.langchain.com/resources/llm-evals)). "Utilidade" é vago demais para virar *grader*. "Omitiu o aviso legal obrigatório" e "citou documento desatualizado" viram. Uma falha com nome vira caso de teste.

Os nomes saem da leitura, antes de qualquer automação. Husain e Shankar recomendam revisar pelo menos 100 *transcripts*, anotar em texto livre o que saiu errado em cada um e depois agrupar as anotações em categorias de falha ([Husain e Shankar](https://hamel.dev/blog/posts/evals-faq/)). Husain conta as ocorrências de cada categoria numa tabela dinâmica ([Husain, *A Field Guide to Rapidly Improving AI Products*](https://hamel.dev/blog/posts/field-guide/)). Eu usaria essa contagem para escolher qual *grader* escrever primeiro.

### Sua pior reclamação já é um caso de teste

Você não precisa de quinhentos casos para começar. A [Anthropic](https://www.anthropic.com/engineering/demystifying-evals-for-ai-agents) sugere de 20 a 50 tarefas simples, tiradas de falhas reais. No começo, cada mudança tem efeito grande, e uma amostra pequena basta para enxergá-lo. Para separar duas versões parecidas, vinte casos não bastam: a diferença some dentro do ruído, e a suíte precisa crescer.

Dá para pôr número nesse ruído. Evan Miller propõe tratar o eval como experimento e reportar a nota com barra de erro ([Miller, *Adding Error Bars to Evals*](https://arxiv.org/abs/2411.00640)). Numa suíte de 60 casos com 70% de acerto, a conta usual do intervalo de confiança de 95% para uma proporção dá de 58% a 82%, a faixa de taxas compatíveis com o que a amostra mostrou. Uma versão nova que marca 75% acertou 3 casos a mais em 60. Eu não comemoraria esses cinco pontos.

Duas práticas baratas ajudam. Rode as duas versões nos mesmos casos e conte quantos pioraram, porque a mesma taxa de acerto pode esconder casos que trocaram de lado. E separe os casos em dois grupos: um para ajustar o *prompt* e o juiz, outro só para reportar a nota. A nota do grupo em que você ajustou tende a sair melhor do que o sistema é.

O que eu faria amanhã numa aplicação sem eval nenhum:

- Ler pelo menos cem respostas da aplicação, anotar o que saiu errado em cada uma e agrupar as anotações em falhas com nome;
- Juntar as últimas reclamações de usuário ou respostas ruins e transformar cada uma num caso com critério escrito;
- Dar a cada critério o *grader* mais barato que o verifica: código primeiro, LLM como juiz depois;
- Ler à mão as notas do juiz em uma amostra e conferir se eu daria a mesma nota, contando quantas respostas ruins ele deixou passar;
- Rodar a suíte a cada troca de modelo ou de *prompt*, comparar as versões nos mesmos casos e reportar *pass^k* se o produto não pode errar.

E quando a nota cair, abrir o *transcript* antes de abrir o *prompt*. Às vezes o modelo errou. Às vezes, como no CORE-Bench, quem errou foi a régua.
