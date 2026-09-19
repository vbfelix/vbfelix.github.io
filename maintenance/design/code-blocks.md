# Blocos de código

## Regra

Bloco de código tem fundo preto e texto de alto contraste. O fundo é `#000000`, definido por `$code-block-bg` em `custom_theme.scss`. Toda cor de texto sobre ele precisa de razão de contraste mínima de 4,5:1, o mesmo `$min-contrast-ratio` que o tema já declara para o resto do site.

A regra vale para o bloco (`div.sourceCode`, `pre`). Código inline segue `$code-bg` e `$code-color`, que vivem sobre a lousa verde e têm contraste próprio.

## Por que preto, e não a lousa verde

O realce de sintaxe do Quarto neste site é o tema escuro, construído para fundo quase preto: texto base `#f8f8f2` e tokens claros como `#ffd700`, `#abe338` e `#00e0e0`. Preto é o fundo para o qual essas cores foram desenhadas, e todas passam de 7:1 sobre ele. Escurecer para um verde da identidade em vez de preto reduz a margem sem ganho de leitura, porque o bloco já é uma superfície separada da lousa.

## O defeito que originou a regra

O tema definia `$code-bg`, que só atinge código inline, e não `$code-block-bg`. Sem ele, o `div.sourceCode` ficava com o padrão do Quarto, `rgba(233, 236, 239, 0.65)`, que composto sobre a lousa `#173F35` resulta em `#a0afae` — cinza claro. O texto quase branco do tema escuro caía para 2,13:1 sobre esse cinza, e os tokens coloridos para entre 1,14:1 e 1,62:1. O bloco ficava ilegível, como no artigo `posts/0034-seu-dashboard-passa-em-code-review`.

Definir só `$code-bg` não corrige: são variáveis distintas, e a do bloco é `$code-block-bg`.

## Como conferir

Ao mexer no tema ou no realce de sintaxe, abra no preview um artigo com bloco de código e meça o contraste real, sem confiar no valor declarado no SCSS — o fundo efetivo pode vir de uma camada semitransparente. Leia `backgroundColor` do `div.sourceCode` e `color` do `pre` já computados pelo navegador, componha a transparência sobre o fundo de trás e calcule a razão pela fórmula de luminância relativa da WCAG. Confira o texto base e pelo menos um token colorido.
