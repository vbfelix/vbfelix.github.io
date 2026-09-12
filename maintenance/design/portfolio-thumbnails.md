# Thumbnails do Portfólio

Este repositório cria e mantém as thumbnails. O ghost-writer fornece os textos; imagens de capa e metadados locais de apresentação não são importados. O sincronizador reserva `thumbnail.*` e `_metadata.yml` para o site, preservando imagens do corpo do artigo vindas da origem.

## Processo para cada artigo

1. Leia o artigo completo e identifique o objeto concreto, a pergunta investigada e o método utilizado.
2. Escolha uma composição própria com elementos reconhecíveis daquele texto. Não reutilize uma capa genérica trocando apenas o título, nem crie gráficos com resultados inventados.
3. Crie a imagem em `portfolio/<slug>/thumbnail.svg`, preferindo SVG para ilustrações e diagramas. Use outro formato apenas quando a composição exigir. A criação é editorial, feita ao incorporar ou revisar o artigo; o sincronizador não gera imagens por palavras-chave.
4. Use a lousa verde `#173f35`, traços de giz em branco quente `#f2efe0`, detalhes em azul suave `#b8d9df` e amarelo `#efd58e`. Trabalhe em 1200 × 675, com margens generosas, poucos rótulos e leitura clara no card pequeno. Grades e fórmulas são detalhes de apoio, relacionados ao assunto.
5. Associe a capa e uma descrição acessível em `_metadata.yml` local, usando `image` e `image-alt`. Confira se o frontmatter do artigo define outra imagem, pois ele tem precedência no Quarto; resolva a referência editorial na origem antes de apresentar o card como concluído. Não altere o corpo importado para decorar o site.
6. Confira o card renderizado, a legibilidade, o recorte e a descrição alternativa. Gere o preview atualizado e verifique o arquivo servido contra a imagem local.

Quando chegar um artigo sem thumbnail, a integração visual ainda está pendente: crie a composição antes de concluir a entrega do artigo. Se a revisão mudar o assunto ou o método, revise também a capa. Não é necessário redesenhar a imagem por correções ortográficas.

## Primeiro artigo: UX com produtos físicos

O artigo sobre pesquisa com mochilas usa três mochilas desenhadas a giz, círculos de observação e a sequência observar, comparar e investigar. Esses elementos representam o produto estudado e o delineamento da pesquisa. A ilustração é conceitual; não representa amostras, proporções ou resultados numéricos do estudo.
