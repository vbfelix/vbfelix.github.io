# Sombras e nuvens, o maior inimigo para se chegar no verdor de plantas

Fonte: https://vbfelix.github.io/portfolio/0034-zonas-de-manejo-satelites/index.html

![Satélite, nuvem, observações irregulares com uma curva suavizada e terreno dividido em zonas. Esquema conceitual, sem dados reais do estudo.](https://vbfelix.github.io/portfolio/0034-zonas-de-manejo-satelites/thumbnail.svg)

Criei uma metodologia para identificar zonas de manejo na agricultura usando dados de satélites. O objetivo era reconhecer partes de uma área com comportamentos semelhantes, criando uma base para pensar o manejo de forma diferenciada.

Parece um problema de pegar imagens, calcular algumas variáveis e agrupar. Mas o trabalho começava bem antes: entender o cultivo e conseguir acompanhar seu desenvolvimento com dados que não chegavam de forma regular.

Acompanhar o ciclo de desenvolvimento das plantas foi essencial. E as nuvens e sombras deram bastante trabalho.

# Entre os especialistas, a literatura e os dados

Neste trabalho, tive o prazer de contar com o apoio de dois especialistas. Um em sensoriamento remoto, que envolve obter informações sobre uma superfície à distância, como nas imagens de satélite. Outro em agricultura de precisão, que considera as diferenças dentro de uma área para orientar o manejo.

Meu papel era abstrair os conhecimentos dos dois: entender as ideias, organizar o que poderia ser representado e medido e juntar isso com os dados. Essa troca fazia parte da construção da metodologia.

Precisávamos combinar modelos empíricos, construídos a partir dos padrões observados nos dados, com o entendimento da literatura científica. O conhecimento dos especialistas ajudava a fazer essa ligação entre o que estudávamos, o que medíamos e o cultivo que queríamos compreender.

# Entender o cultivo para entender o dado

Uma curva fenológica é um gráfico que acompanha o desenvolvimento da vegetação ao longo do tempo, mostrando mudanças durante o crescimento e o envelhecimento das plantas. No projeto, eu construía essas curvas com o **Índice de Vegetação por Diferença Normalizada**, conhecido pela sigla inglesa **NDVI**, de *Normalized Difference Vegetation Index*.

O [NDVI combina a luz vermelha e a luz infravermelha próxima refletidas pela superfície](https://www.usgs.gov/landsat-missions/landsat-normalized-difference-vegetation-index), registradas pelos sensores do satélite. O infravermelho próximo é uma faixa de luz que nossos olhos não enxergam. O cálculo divide a diferença entre essas duas medidas pela soma delas, produzindo um indicador do verdor da vegetação, útil para acompanhar suas mudanças.

Era essa evolução que me interessava: quando o índice começava a subir, quando atingia o máximo e como diminuía até o fim da estação de crescimento, o período de desenvolvimento da vegetação que eu queria analisar.

Isso permitia olhar para o desenvolvimento ao longo do tempo. Um valor isolado de NDVI não conta essa história inteira. Para interpretar a curva, eu precisava saber qual era a cultura e delimitar as safras com informações de plantio e colheita.

A metodologia começava justamente por aí: identificar a área, a cultura e os períodos de cultivo. Só depois vinha o tratamento das imagens e o cálculo das métricas.

# O satélite não entregava uma série pronta

A irregularidade na obtenção das imagens era um dos maiores desafios. Ter várias imagens disponíveis não significava ter boas observações distribuídas ao longo de todo o ciclo.

Além das lacunas, havia nuvens e sombras. Uma queda no sinal exigia cuidado: interpretar diretamente cada oscilação como uma mudança na lavoura comprometeria tudo o que viesse depois.

Estruturei o ajuste por safra e por pixel: cada pixel é uma pequena unidade da imagem que representa uma porção do terreno. Para cada uma dessas porções, organizei as observações ao longo do tempo, formando uma série temporal.

Para suavizar a série, usei um método que [ajusta pequenas curvas usando observações próximas entre si](https://itl.nist.gov/div898/handbook/pmd/section1/pmd144.htm), dando mais influência às mais próximas. Assim, estima o comportamento da série sem exigir uma única forma de curva para todo o ciclo.

No meu procedimento, o primeiro ajuste também usava pesos associados à probabilidade de nuvens. O peso definia quanto cada observação influenciava o resultado: quanto maior a probabilidade de nuvens, menor essa influência.

O procedimento também identificava observações com alta probabilidade de nuvens ou quedas bruscas de NDVI, substituía seus valores pelo ajuste inicial e fazia uma segunda suavização. A curva tratada passava a ser a base das métricas fenológicas.

Havia um limite importante: intervalos muito longos entre imagens podiam inviabilizar o método. Suavizar uma série não resolve qualquer falta de informação.

# As principais decisões da metodologia

- **A cultura e a safra definiam o recorte.** Plantio, colheita e identificação do cultivo entravam antes da análise. A comparação precisava respeitar o ciclo que eu estava tentando medir.
- **A qualidade da observação entrava no ajuste.** A probabilidade de nuvens influenciava o peso de cada ponto; valores suspeitos não seguiam diretamente para o cálculo das métricas.
- **O comportamento ao longo do tempo virava informação.** Início e fim da estação, pico de NDVI, taxas de crescimento e senescência (o envelhecimento da vegetação, acompanhado pela redução do sinal após o pico) permitiam descrever aspectos diferentes da curva.
- **O histórico fazia parte do zoneamento.** Estabeleci um requisito mínimo de três safras por cultura, em vez de definir o procedimento a partir de uma única safra.
- **O relevo entrava junto com a vegetação.** A elevação, que descreve a altura do terreno, e a declividade, que expressa sua inclinação, compunham o agrupamento com uma medida derivada da curva de NDVI.

# Da curva às zonas de manejo

Uma das medidas que usei foi a área sob a curva de NDVI durante a estação: uma soma contínua dos valores do índice ao longo do tempo. Descontava dessa área uma base definida pela linha que ligava os pontos de início e fim da estação. Ela reunia informação sobre a intensidade e a duração do sinal da vegetação. Era uma medida derivada do NDVI, sem unidade de sacas por hectare.

Para o zoneamento, combinei essa medida com elevação e declividade. Coloquei as variáveis numa escala comum por safra, entre zero e um, para comparar medidas originalmente expressas em escalas diferentes. Essa transformação é a padronização. Depois, calculei suas médias por pixel ao longo do histórico e dei mais influência à medida derivada da vegetação no agrupamento.

A etapa de agrupamento usava o **método das k médias**. O k representa a quantidade de grupos a formar. O método reúne observações em torno de centros calculados pela média das variáveis de cada grupo, buscando aproximar as observações semelhantes. No caso, essas observações correspondiam às porções do terreno descritas pelas medidas escolhidas. Depois, as zonas eram qualificadas em função da medida derivada do NDVI.

O significado do mapa dependia dessas escolhas. O agrupamento recebia o resultado de todo o trabalho anterior: recorte das safras, tratamento das observações, cálculo das métricas, padronização e ponderação.

# Uma metodologia que virou funcionalidade de produto

Construí a metodologia para que pudesse ser generalizada e aplicada a qualquer propriedade rural do Brasil. Isso exigia organizar o conhecimento em um procedimento que pudesse ser repetido com os dados de cada propriedade, considerando sua área, suas culturas e seu histórico de safras.

A metodologia foi acoplada como uma funcionalidade de um produto. As etapas de seleção das imagens, tratamento das séries e identificação das zonas passaram a compor essa entrega. Os critérios de quantidade de safras e qualidade das observações continuavam fazendo parte das condições de aplicação.

# Aprendizados, cuidados e principais impactos

Minha principal entrega foi transformar essa metodologia de zoneamento em uma funcionalidade de produto, estruturada para aplicação em propriedades rurais de todo o Brasil, com etapas e critérios explícitos. O projeto reforçou alguns cuidados que considero centrais no trabalho com dados:

- **Dados e conhecimento precisam trabalhar juntos.** Combinar modelos empíricos com literatura e com o apoio dos especialistas foi parte central da metodologia. Meu papel era traduzir esses conhecimentos em algo que pudesse ser representado e analisado com dados.
- **Entender o fenômeno orienta o que medir.** As curvas fenológicas deram sentido agronômico à análise temporal e ajudaram a definir as características usadas no processo.
- **A qualidade do dado faz parte do método.** Irregularidade, nuvens e sombras precisavam entrar no raciocínio desde o início, porque afetavam as medidas que sustentavam as zonas.
- **Preencher uma curva não elimina suas limitações.** O procedimento precisava reconhecer quando o histórico disponível não era suficiente.
- **Um indicador precisa conservar seu significado.** Uma medida obtida do NDVI não deve ser apresentada diretamente como produtividade colhida.
- **Reproduzir o resultado também exige cuidado.** O procedimento incluía uma semente: um valor que fixa o ponto de partida do gerador de números usado nas escolhas aleatórias do agrupamento. Mantendo os mesmos dados e configurações, isso permite repetir essas escolhas ao executar o método novamente.

O mapa era a forma de apresentar o resultado. O trabalho estatístico estava em definir quais dados poderiam sustentá-lo e como transformar o ciclo do cultivo em informação para o zoneamento.
