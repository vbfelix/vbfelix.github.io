---
version: "alpha"
name: "Caderno estatístico em lousa"
description: "Identidade editorial de Vinícius Félix: investigação, método e clareza sobre uma superfície de lousa verde."
colors:
  primary: "#173F35"
  on-primary: "#F2EFE0"
  surface: "#204A3E"
  surface-deep: "#12372E"
  text: "#F2EFE0"
  text-muted: "#C0CEC0"
  line: "#648477"
  link: "#B8D9DF"
  accent: "#EFD58E"
  selection: "#46685B"
typography:
  display:
    fontFamily: "Georgia, Times New Roman, serif"
    fontSize: "clamp(2.5rem, 5vw, 4rem)"
    fontWeight: 400
    lineHeight: 1.1
    letterSpacing: "-0.04em"
  heading:
    fontFamily: "Georgia, Times New Roman, serif"
    fontSize: "1.75rem"
    fontWeight: 400
    lineHeight: 1.2
    letterSpacing: "-0.04em"
  body:
    fontFamily: "Segoe UI, Arial, sans-serif"
    fontSize: "1rem"
    fontWeight: 400
    lineHeight: 1.7
  label:
    fontFamily: "Consolas, Courier New, monospace"
    fontSize: "0.69rem"
    fontWeight: 400
    lineHeight: 1.5
    letterSpacing: "0.11em"
rounded:
  none: 0px
  card: 6px
  logo: 4px
spacing:
  xs: 0.45rem
  sm: 0.7rem
  md: 1rem
  lg: 2rem
  xl: 3rem
components:
  card:
    backgroundColor: "{colors.surface}"
    textColor: "{colors.text}"
    rounded: "{rounded.card}"
    padding: "{spacing.md}"
  link:
    textColor: "{colors.link}"
  link-hover:
    textColor: "{colors.accent}"
  utility-link:
    backgroundColor: "transparent"
    textColor: "{colors.text}"
    rounded: "{rounded.none}"
    padding: "0.65rem 1rem"
---

## Overview

O site deve parecer um caderno de campo de estatística: sóbrio, investigativo e humano. A base é uma lousa verde profunda, com poeira de giz e diagramas estatísticos discretos. O conteúdo tem prioridade sobre a decoração. Cada elemento visual deve ajudar a leitura, revelar método ou orientar a navegação.

## Colors

`primary` é a lousa e domina a página. `surface` separa cartões e faixas sem romper o campo visual. `text` é um branco quente, nunca branco puro. `link` é azul de giz para referências e navegação. `accent` é amarelo de giz e fica reservado para hover, foco e ênfase de interação. `line` estrutura tabelas, cartões e divisões com baixo contraste.

Mantenha contraste AA em textos e controles. Use a textura de giz apenas como fundo de baixa opacidade. Nunca a aplique sobre blocos de código, tabelas ou imagens informativas.

## Typography

Títulos usam a serif editorial, com peso normal e leve compressão de letras. O corpo usa uma sans serif de sistema para leitura contínua. Metadados, rótulos e referências curtas usam a mono, em caixa alta somente quando forem rótulos de seção. Não use fontes decorativas, pesos pesados ou títulos em caixa alta.

## Layout

Em páginas de leitura, a coluna de conteúdo tem no máximo 900px. Páginas de apresentação e catálogo podem chegar a 1200px. Use a escala de espaçamento e separadores de uma linha para formar ritmo, em vez de caixas empilhadas. Em telas estreitas, uma coluna é a regra para blocos editoriais e para a apresentação pessoal.

## Elevation & Depth

Não use sombras. A profundidade vem de tons de verde, bordas `line` e da textura de fundo. Cartões permanecem planos e só revelam interação por mudança de borda, cor de link ou foco visível.

## Shapes

As formas são essencialmente retangulares. A borda arredondada de 6px é exclusiva de cartões e mídia de portfólio. Links utilitários, tabelas e divisores não recebem cantos arredondados.

## Components

Cartões de portfólio usam `surface`, borda `line` e título serif. Imagens de capa usam proporção 16:9 e preservam sua composição, sem corte automático. Links têm sublinhado fino e afastado do texto. O foco usa contorno de 2px em `accent` ou `link`, com espaço externo suficiente. Controles de navegação devem continuar legíveis sobre a lousa e respeitar o mesmo estado de foco.

## Do's and Don'ts

Faça:

* Use gráficos, eixos, distribuições e textura de giz como vocabulário visual quando forem relevantes ao conteúdo.
* Preserve bastante espaço para a prosa e para a evidência.
* Trate acessibilidade como parte da identidade: contraste, foco visível e texto alternativo são obrigatórios.

Não faça:

* Não introduza gradientes chamativos, sombras, vidro fosco ou cores saturadas fora da paleta.
* Não use azul ou amarelo como grandes superfícies decorativas.
* Não transforme o fundo em ilustração concorrente do conteúdo.
