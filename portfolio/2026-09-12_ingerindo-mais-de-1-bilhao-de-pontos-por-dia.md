---
tipo: portfolio
data: '2026-09-12'
status: draft
titulo: Ingerindo mais de 1 bilhão de pontos por dia
site:
  categories: [data engineering, data science]
  lang: pt-BR
episodio: episodio/conducao-do-projeto-geobehavior
referencias: []
fatos:
- fato/pontos-diarios-ingeridos-no-projeto-geobehavior
- fato/dispositivos-abrangidos-pelo-projeto-geobehavior
- fato/volume-diario-recebido-no-projeto-geobehavior
- fato/variacao-de-frequencia-dos-registros-por-dispositivo-no-geobehavior
fontes: [FONTE-263, FONTE-264]
---

## Brief

Projeto conduzido pelo autor da arquitetura à ciência de dados, com execução pela equipe. O caso articula ingestão em alto volume, heterogeneidade temporal dos registros e construção de indicadores de comportamento a partir de localização. Não atribuir execução direta ao autor, identidade pessoal conhecida, cobertura uniforme ou anonimização comprovada.

## Portfolio

Mais de 1 bilhão de pontos por dia. A estrutura do dado era simples: data e hora, um identificador do dispositivo e uma coordenada. O trabalho para transformar aquilo em informação útil era bem menos simples.

Conduzi de ponta a ponta um projeto que lidava com dados de mais de 100 milhões de celulares. Minha atuação atravessou arquitetura, engenharia e ciência de dados, com a execução realizada pela equipe.

Recebíamos cerca de 30 a 50 gigabytes (GB) por dia, em lotes que chegavam a cada hora. Precisávamos organizar e processar esse volume, controlar o custo e, depois, construir algo útil a partir dos registros de localização.

### Receber era a parte fácil

A ingestão, a entrada dos dados no nosso ambiente, colocava um problema de armazenamento e processamento. Precisávamos de formatos compactos, que ocupassem pouco espaço, e otimizados para leitura, para que trabalhar com os dados não exigisse um custo desproporcional.

O pré-processamento também era importante. Antes de desenvolver os indicadores, havia trabalho de preparação dos dados, especialmente sobre sua dimensão espacial: a localização de cada ponto.

A Uber foi uma referência nessa etapa. A eficácia com que o aplicativo processava os pontos ao solicitar um motorista nos levou a estudar sua abordagem geral de processamento e adaptá-la à nossa necessidade. Meu papel era conduzir essas decisões e manter a conexão entre a estrutura de engenharia e o que a ciência de dados precisaria construir depois.

Resolvemos o desafio do volume com uma estrutura de baixo custo. Mas ter os pontos armazenados e prontos para processamento ainda deixava uma pergunta: o que conseguiríamos entender a partir deles?

### Um bilhão de pontos não significava a mesma informação sobre cada celular

O identificador não me dizia quem era o dono do aparelho. Ele permitia acompanhar registros associados a um dispositivo, mas oferecia apenas uma parte de seu comportamento.

A frequência desses registros variava muito. Alguns celulares geravam **um registro a cada dez dias**. Outros chegavam a **400 registros por dia**. Tratar esses dois casos da mesma forma desconsideraria uma diferença central do dado.

Essa irregularidade mudou a forma como precisávamos trabalhar:

- **O volume total não descrevia a cobertura de cada dispositivo.** Era necessário examinar os registros disponíveis para definir o que podia entrar em cada análise.
- **O período de observação precisava variar.** Alguns dispositivos ofereciam informação útil em poucos dias; outros exigiam períodos mais longos.
- **Disponibilidade não bastava para uso.** Critérios de elegibilidade, as condições para aceitar um registro na análise, faziam parte dos métodos.

Trabalhamos com diferentes janelas temporais, os intervalos de tempo considerados na análise, para ampliar a cobertura dos dispositivos. A irregularidade dos dados precisava ser tratada junto da construção dos indicadores.

### O Geobehavior

Com critérios definidos, construímos o Geobehavior: indicadores para caracterizar padrões associados aos dispositivos a partir de seus registros de localização.

Conseguíamos inferir locais associados à moradia, ao trabalho e à frequência de visitas. Ao combinar esses padrões com os dados do entorno, passávamos dos pontos isolados para uma caracterização do dispositivo.

O identificador continuava sem me informar o nome de seu dono. O que tínhamos era uma leitura de parte do comportamento observado, construída a partir dos registros disponíveis e dos critérios adotados.

Essa distinção era importante para entender o resultado. Um perfil dependia da informação que aquele dispositivo havia produzido. A quantidade de dados recebida pelo sistema não tornava igualmente completa a observação de todos os aparelhos.

### Aprendizados, lições, erros e principais impactos

Conduzi o projeto desde a estrutura para receber e processar os dados até a construção dos indicadores. A equipe resolveu primeiro o desafio do alto volume com baixo custo, tratou a irregularidade dos registros e desenvolveu o Geobehavior.

Dessa experiência, destaco:

- **A arquitetura precisava preparar o uso analítico.** Formatos compactos, leitura eficiente e pré-processamento espacial faziam parte da preparação dos dados para os indicadores.
- **Usar o volume agregado como medida de informação seria uma armadilha.** A diferença entre registros esparsos e frequentes exigia critérios de elegibilidade e períodos de observação distintos.
- **As janelas temporais faziam parte do método.** Adaptar os intervalos analisados era o caminho adotado para ampliar a cobertura diante de registros irregulares.
- **O resultado foi transformar pontos em indicadores de comportamento.** O Geobehavior combinou padrões de localização com dados do entorno, criando uma caracterização que ia além do registro cru.

Minha contribuição foi conduzir essa sequência de ponta a ponta, articulando as decisões de arquitetura, engenharia e ciência de dados para que o volume recebido pudesse se tornar informação utilizável.

## Evidências

- raw:FONTE-263: relato do autor em 2026-09-12. Sustenta escala diária, universo de dispositivos, volume em GB, recebimento horário, campos, objetivos de custo e velocidade, formatos, pré-processamento, Uber como referência, variabilidade, critérios e janelas, Geobehavior e papel de condução.
- Os quatro fatos declarados preservam pontos por dia, universo de dispositivos, volume diário e exemplos de frequência como recortes distintos. Não se divide o total de pontos pelo universo para inferir frequência média de dispositivos ativos.
- raw:FONTE-264: o autor esclarece inspiração na abordagem geral de processamento da Uber, sem tecnologia específica. Não afirmar H3, formato colunar específico, biblioteca, provedor de nuvem ou processamento em tempo real no projeto.
- **Não documentado.** Medida de custo que fundamenta a descrição de baixo custo, Q-065. A qualificação vem do autor; não se inventam valores ou percentuais.
- **Não documentado.** Validação dos perfis de comportamento produzidos, Q-066. Não se afirma acurácia, abrangência individual uniforme ou observação completa de rotinas.
- Origem dos dados e período do projeto não identificados no relato. Não associar automaticamente ao caso de fluxo de celulares mencionado no LinkedIn de 16/07/2026.
- Entrega: `posts/portfolio/0039-geobehavior/index.qmd`; destino no site: `posts/0039-geobehavior/index.qmd`. Numeração local anterior até 0038; clone do site até 0031. Sem capa ou assets.

## Notes

- Título fornecido pelo autor preservado.
- A execução não é atribuída diretamente ao autor. Sua contribuição é condução da arquitetura à ciência de dados; a equipe realiza a execução.
- Moradia, trabalho e frequência são apresentados como inferências sobre observações parciais. Não saber o nome do dono não é convertido em garantia de anonimato, impossibilidade de identificação ou propriedade jurídica do dado.
- Os 100 milhões de dispositivos não são apresentados como pessoas únicas ou dispositivos ativos todos os dias.
- O relato sobre Uber sustenta inspiração e adaptação, sem números de escala da empresa ou alegação de ter reproduzido sua arquitetura.
- Fechamento explicita entregas, aprendizados e armadilhas, sem inventar falha pessoal, impacto financeiro ou uso comercial dos indicadores.
