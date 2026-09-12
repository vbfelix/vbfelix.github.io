# Statistical notebook design

The site presents Vinícius Félix’s work through questions, methods, evidence, and limitations. The reference portfolio informed content hierarchy; the visual language comes from scientific figures and editorial notebooks.

## Visual system

- Paper: #F4F1E9; ink: #202D40; mineral blue: #355C80; terracotta: #A34E35.
- Rules: #D6D1C5; muted text: #60665F; figure surface: #E9E8E0.
- Georgia for editorial headings, Segoe UI / Arial for prose, Consolas / Courier New for metadata. No remote font dependency.
- Restrained color, fine rules, figure captions, readable prose, and generous margins.
- Mobile layouts stack figures and text. Focus remains visible and reduced-motion preferences are respected.

## Homepage

The introduction pairs a professional statement with an educational regression figure. The small portrait anchors authorship. Selected work covers coauthored time-series research, the relper R package, and the documented Datlo data architecture. Writing links to the original articles. The background section records the end of the Datlo role in September 2026.

## Statistical figure

images/notebook-regression.svg contains 48 simulated observations. The deterministic generator starts with seed 20260905, uses a 32-bit LCG (1664525, 1013904223), and Box–Muller Gaussian noise. x spans 0.3 to 9.7; y = 1.5 + 0.64x + Normal(0, 0.95²).

The displayed fit is ordinary least squares. The shaded band is a pointwise 95% confidence interval for the conditional mean, with residual standard error, 46 degrees of freedom, and t critical value 2.012896. It is not a prediction interval. Both axes use arbitrary units. The bilingual captions label the simulation and model assumptions. Decorative SVGs in work rows are hidden from assistive technology and do not encode project outcomes.

## Content and maintenance

- Root pages use en-US; pt-br pages use pt-BR. Update both for new prose.
- Existing articles retain their original language and addresses; the archive says so explicitly.
- Run scripts/build-writing.py when article metadata changes to refresh both archive pages.
- Quarto renders all 45 current pages into docs/, using the existing frozen execution results for computational articles.
- Public biography claims are checked against the ghost-writer canonical profile and current site sources.
- A small language script localizes navigation and footer links. On original-language articles, the Portuguese link opens the Portuguese archive rather than implying a translated article exists.

## Chalkboard revision

The user subsequently chose a university green chalkboard as the visual direction. Current tokens are board green #173F35, chalk white #F2EFE0, muted chalk #C0CEC0, blue chalk #B8D9DF, and yellow chalk #EFD58E. The navbar and footer use a deeper green. A low-opacity SVG noise texture suggests chalk dust without affecting text sharpness.

The homepage displays expectation, variance, Bayes proportionality, a linear model, and the ordinary least-squares estimator as decorative mathematical notation. These decorations are hidden from assistive technology; the explanatory regression figure retains its bilingual alternative text and simulation caption. Graphs and project illustrations use the chalk palette, and the figure has a small wooden tray with two chalk sticks. Existing article figures keep a paper background for legibility.

## Portuguese-only navigation

At the user’s request, Portuguese is now the default and only interface language. Root institutional pages contain the existing Portuguese content; /pt-br/ addresses remain valid for compatibility. The language selector and its JavaScript include have been removed. Archive regeneration writes Portuguese to both address sets. Existing articles retain their original language.

## Homepage focused on reasoning

At the user’s request, the homepage no longer features work notes, project/tool showcases, or the metrics strip. It foregrounds three documented working principles: question the premise, expose uncertainty, and follow through in practice. The archive remains accessible through Escritos. Contact is a navigation menu with direct LinkedIn and GitHub links; the page ends with the same social links, without a contact pitch or form.

## Graph-only homepage

The latest user direction replaces the homepage prose and sections with the existing statistical figure and a short simulation caption. Subtle distribution notation and sketches appear in the background. The foreground figure remains the only main visual. Escritos is removed from navigation, and the archive and articles are excluded from site search; existing URLs remain valid. The remaining navigation provides Sobre and direct social destinations under Contato.

## Homepage profile and bivariate portrait

The homepage now presents the full existing about content, keeping the old about URLs valid. An AI-stylized version of the original portrait forms an artistic point cloud within responsive SVG axes; it does not represent measured observations. The original photo is preserved. Professional experience dates use MM/YY - MM/YY as requested.

## Header removal and monochrome portrait

Removed the global navbar. Home and social links remain available in the footer. Enlarged the portrait plot, with a white-and-gray chalk portrait and a denser cloud surrounding the face. The layout stacks below 900px to give the plot adequate space.
