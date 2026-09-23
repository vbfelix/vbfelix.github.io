// Quarto's bundled Deno runs this after full and incremental renders.
// Public QMD content remains the source of truth; no separate biography to edit.
import { articleLanguage, findArticleSources, firstProseParagraph, markdownLinks, readContent, readFrontMatter, validatePublishedLink } from './content-source.ts';
const config = await Deno.readTextFile('_quarto.yml');
const base = config.match(/site-url:\s*(\S+)/)?.[1].replace(/\/$/, '');
if (!base) throw new Error('Missing website.site-url');
const output = Deno.env.get('QUARTO_PROJECT_OUTPUT_DIR') || 'docs';
const check = Deno.args.includes('--check');
const names: Record<string, string> = {
  index: 'Sobre Vinícius Félix',
  'header-about': 'Sobre mim',
  'header-experience': 'Experiência profissional e acadêmica',
  'header-publications': 'Publicações',
  'header-courses': 'Cursos',
  'header-certifications': 'Certificações',
  'header-participations': 'Participações',
  'header-awards': 'Prêmios',
  agents: 'Para agentes',
};
const absolute = (url: string) => {
  const target = new URL(url, base + '/');
  if (target.origin === new URL(base).origin) target.pathname = target.pathname.replace(/\.qmd$/, '.html');
  return target.href;
};
function markdown(source: string, sourceUrl = '/'): string {
  // This transforms text only. It never evaluates Quarto, R, JavaScript, or
  // any other executable block found in a public document.
  const resolve = (url: string) => {
    if (/^(?:https?:|mailto:|#)/.test(url)) return url;
    const target = new URL(url, absolute(sourceUrl));
    return absolute(target.pathname) + target.search + target.hash;
  };
  const prose = (text: string) => text
    .replace(/^:::.*$/gm, '')
    .replace(/!\[([^\]]*)\]\(([^ )]+)(?:\s+"[^"]*")?\)(?:\{[^}]*\})?/g, (_, alt, url) => `![${alt}](${resolve(url)})`)
    .replace(/\]\(([^ )]+)(?:\s+"[^"]*")?\)/g, (_, url) => `](${resolve(url)})`)
    .replace(/<[^>]+>/g, '');
  const body = source.replace(/^---\n[\s\S]*?\n---(?:\n|$)/, '');
  return body.replace(/\r\n/g, '\n').split(/(```[\s\S]*?```)/g)
    .map((part, index) => index % 2 ? part : prose(part))
    .join('').replace(/\n{3,}/g, '\n\n').trim();
}

type Article = { title: string; type: 'post' | 'portfolio'; language: string; topics: string[]; url: string; markdownUrl: string; content: string; revision: string };
async function digest(value: string): Promise<string> {
  return Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256', new TextEncoder().encode(value))))
    .map(x => x.toString(16).padStart(2, '0')).join('');
}
async function articles(): Promise<Article[]> {
  const result: Article[] = [];
  for (const type of ['post', 'portfolio'] as const) for (const path of await findArticleSources(type === 'post' ? 'posts' : 'portfolio')) {
    const raw = await Deno.readTextFile(path);
    const fields = readFrontMatter(raw), relative = path.replace(/\\/g, '/').replace(/\/index\.qmd$/, '/index.html');
    const title = String(fields.title || '').trim();
    if (!title) throw new Error(`Missing title: ${path}`);
    const topics = Array.isArray(fields.categories) ? fields.categories : String(fields.categories || '').split(',').map(x => x.trim()).filter(Boolean);
    const url = absolute('/' + relative);
    let content = markdown(raw, '/' + relative);
    let revisionSource = raw;
    if (type === 'portfolio') {
      const directory = path.replace(/\/index\.qmd$/, '');
      const metadata = await Deno.readTextFile(`${directory}/_metadata.yml`);
      const artwork = readFrontMatter(`---\n${metadata.trim()}\n---\n`);
      const image = artwork.image, alt = artwork['image-alt'];
      if (typeof image !== 'string' || typeof alt !== 'string' || !/^[\w.-]+$/.test(image) || !alt.trim()) throw new Error(`Invalid portfolio artwork metadata: ${directory}`);
      await Deno.stat(`${directory}/${image}`);
      content = `![${alt}](${absolute(`/${directory}/${image}`)})\n\n${content}`;
      revisionSource += metadata;
    }
    result.push({title, type, language: articleLanguage(type, fields.lang), topics, url, markdownUrl: `${url}.md`, content, revision: await digest(revisionSource)});
  }
  return result;
}
const plain = (s: string) => s.replace(/!\[[^\]]*\]\([^)]+\)/g, '').replace(/\[([^\]]+)\]\([^)]+\)/g, '$1').replace(/\*\*/g, '').trim();
const source: Record<string, string> = {};
const texts: Record<string, string> = {};
for (const name of Object.keys(names)) {
  source[name] = await readContent(`${name}.qmd`);
  texts[name] = markdown(source[name]);
}
const introduction = texts.index.split(/^# [^\n]+\n/m)[1]?.trimStart();
const summary = introduction?.split('\n\n').find(p => p.trim() && !/^[#|\[!]/.test(p.trim()));
if (!summary) throw new Error('Profile introduction not found');
function rows(heading: string): string[][] {
  const section = texts['header-experience'].split(`## ${heading}\n`)[1]?.split('\n## ')[0];
  if (!section) throw new Error(`Missing section: ${heading}`);
  const lines = section.split('\n').filter(line => line.startsWith('|'));
  if (lines.length < 3) throw new Error(`Missing table: ${heading}`);
  return lines.slice(2).map(line => line.split('|').slice(1, -1).map(plain));
}
function dates(period: string) {
  const match = period.match(/^(\d{2})\/(\d{2}) - (\d{2})\/(\d{2})$/);
  if (!match) throw new Error(`Unsupported date range: ${period}`);
  const [, sm, sy, em, ey] = match;
  if (+sm < 1 || +sm > 12 || +em < 1 || +em > 12) throw new Error(`Invalid month: ${period}`);
  const startDate = `20${sy}-${sm}`, endDate = `20${ey}-${em}`;
  if (startDate > endDate) throw new Error(`Reversed period: ${period}`);
  return {startDate, endDate};
}
const work = rows('Experiência profissional').map(([period, role, summary]) => {
  const comma = role.indexOf(',');
  if (comma < 0 || !summary) throw new Error(`Invalid work row: ${role}`);
  const lines = summary.split(/(?<=\.)\s+(?=[A-ZÀ-Ý])/);
  if (!Array.isArray(lines) || lines.length < 1 || lines.length > 3 || lines.some(line => typeof line !== 'string' || !line.trim() || /[\r\n]/.test(line))) {
    throw new Error(`Expected one to three curriculum summary lines: ${period}`);
  }
  return {organization: role.slice(0, comma), position: role.slice(comma + 1).trim(),
    ...dates(period), summary: lines.join('\n'), source: absolute('/header-experience.html#experiência-profissional')};
});
const education = rows('Experiência acadêmica').map(([institution, period, qualification, thesis]) => ({
  institution, qualification, ...dates(period), thesis,
  source: absolute('/header-experience.html#experiência-acadêmica'),
}));
const sameAs = [
  'https://github.com/vbfelix',
  'https://www.linkedin.com/in/vin%C3%ADcius-f%C3%A9lix-962010140/',
  'http://lattes.cnpq.br/6820390470508877',
];
const revision = await digest(JSON.stringify(source));
const documents = Object.keys(names).filter(n => n !== 'header-about').map(name => ({
  title: names[name], url: absolute(`/${name}.html`), markdownUrl: absolute(`/${name}.html.md`),
  content: texts[name],
}));
const articleDocuments = await articles();
const cv = {
  schemaVersion: '1.0', language: 'pt-BR', sourceRevision: revision,
  datePrecision: 'month',
  scope: 'Dados públicos declarados no site. Datas têm precisão mensal. Campos ausentes não foram informados. Cargos com endDate são históricos.',
  name: 'Vinícius Félix', url: base + '/', summary: summary.split('. ')[0] + '.', sameAs, work, education,
  sources: [absolute('/index.html'), absolute('/header-experience.html')],
};
const person = {
  '@type': 'Person', '@id': base + '/#person', name: cv.name, url: cv.url,
  description: summary, sameAs,
  alumniOf: [...new Set(education.map(e => e.institution))].map(name => ({'@type': 'CollegeOrUniversity', name})),
};
const assets: Record<string, string> = {};
for (const name of Object.keys(names)) {
  assets[`${name}.html.md`] = `# ${names[name]}\n\nFonte: ${absolute(`/${name}.html`)}\n\n${texts[name]}\n`;
}
for (const article of articleDocuments) {
  assets[new URL(article.markdownUrl).pathname.replace(/^\//, '')] = `# ${article.title}\n\nFonte: ${article.url}\n\n${article.content}\n`;
}
assets['curriculo.json'] = JSON.stringify(cv, null, 2) + '\n';
assets['curriculo.md'] = `# Currículo de Vinícius Félix\n\nFonte: ${base}/\n\n${cv.scope}\n\n` +
  `${cv.summary}\n\n## Formação acadêmica\n\n` +
  education.map(e => `### ${e.qualification}\n\n${e.institution} | ${e.startDate} a ${e.endDate}\n\nTrabalho final: ${e.thesis}`).join('\n\n') +
  '\n\n## Experiência profissional\n\n' +
  work.map(w => `### ${w.organization}: ${w.position}\n\n${w.startDate} a ${w.endDate}\n\n${w.summary}`).join('\n\n') +
  `\n\n[Trajetória em detalhe](${base}/header-experience.html) · [LinkedIn](${sameAs[1]}) · [GitHub](${sameAs[0]})\n`;
assets['llms.txt'] = `# Vinícius Félix\n\n> Estatístico e mestre em Bioestatística. Perfil público, trajetória profissional, formação e produção acadêmica.\n\n` +
  `As páginas institucionais estão em português; os artigos preservam seus idiomas originais. Figuras produzidas durante a execução de código estão no HTML canônico. O currículo é gerado das mesmas fontes das páginas públicas. Datas no JSON usam YYYY-MM; não implicam um dia específico. Consulte as fontes citadas para contexto e resultados. Cargos encerrados não são vínculos atuais.\n\n` +
  `## Currículo\n\n- [Currículo em Markdown](${base}/curriculo.md): resumo da experiência profissional e formação acadêmica.\n- [Currículo em JSON](${base}/curriculo.json): experiência e formação em campos estruturados, com fontes.\n\n## Perfil e fontes\n\n` +
  documents.map(d => `- [${d.title}](${d.markdownUrl}): versão textual da página pública.`).join('\n') +
  `\n\n## Artigos e projetos\n\n` + articleDocuments.map(article => `- [${article.title}](${article.markdownUrl}): ${article.type === 'post' ? 'artigo' : 'projeto'} em ${article.language}${article.topics.length ? `, temas: ${article.topics.join(', ')}` : ''}.`).join('\n') +
  `\n\n## Dados\n\n- [Índice estruturado do acervo](${base}/agent-index.json): títulos, tipos, idiomas, temas, URLs canônicas, Markdown e revisões.\n- [Conteúdo completo para agentes](${base}/llms-full.txt): perfil, trajetória e acervo na íntegra.\n`;
assets['llm.txt'] = assets['llms.txt']; // Compatibility with the user's singular spelling.
assets['llms-full.txt'] = '# Vinícius Félix: perfil e documentos completos\n\n' +
  documents.map(d => `## ${d.title}\n\nFonte: ${d.url}\n\n${d.content.replace(/^#/gm, '##')}`).join('\n\n') +
  '\n\n# Acervo\n\n' + articleDocuments.map(article => `## ${article.title}\n\nFonte: ${article.url}\n\n${article.content.replace(/^#/gm, '##')}`).join('\n\n') + '\n';
assets['agent-index.json'] = JSON.stringify({
  schemaVersion: '1.0', sourceRevision: await digest(JSON.stringify(articleDocuments.map(({content, ...article}) => article))),
  items: articleDocuments.map(({content, ...article}) => article),
}, null, 2) + '\n';
for (const [path, content] of Object.entries(assets)) {
  // Static servers may omit charset for Markdown/text. A UTF-8 BOM makes
  // browser decoding explicit; JSON stays BOM-free for strict consumers.
  const encoded = new TextEncoder().encode(/\.(md|txt)$/.test(path) ? '\uFEFF' + content : content);
  let actual: Uint8Array | undefined;
  try { actual = await Deno.readFile(`${output}/${path}`); }
  catch (error) { if (!(error instanceof Deno.errors.NotFound) || check) throw error; }
  const unchanged = actual?.length === encoded.length && actual.every((byte, i) => byte === encoded[i]);
  if (check && !unchanged) throw new Error(`Outdated agent asset: ${path}`);
  if (!check && !unchanged) await Deno.writeFile(`${output}/${path}`, encoded);
}
// The marker makes repeated and partial renders idempotent.
const metadataPages = [
  ...Object.keys(names).map(name => ({name, title: names[name], url: absolute(`/${name}.html`), markdownUrl: absolute(`/${name}.html.md`), language: 'pt-BR' as const})),
  ...articleDocuments,
];
for (const page of metadataPages) {
  const path = `${output}/${new URL(page.url).pathname.replace(/^\//, '')}`;
  let html: string;
  try { html = await Deno.readTextFile(path); } catch (error) {
    if (!check && error instanceof Deno.errors.NotFound) continue;
    throw error;
  }
  const isProfile = 'name' in page && ['index', 'header-about'].includes(page.name);
  const isArticle = 'type' in page;
  const articleType = isArticle ? (page.type === 'post' ? ['Article', 'BlogPosting'] : 'CreativeWork') : undefined;
  const graph = {'@context': 'https://schema.org', '@graph': [person, {
    '@type': articleType || (isProfile ? 'ProfilePage' : 'WebPage'),
    '@id': page.url + '#page', url: page.url, name: page.title, inLanguage: page.language,
    ...(isArticle ? {author: {'@id': person['@id']}, mainEntityOfPage: page.url, ...(page.type === 'post' ? {articleSection: page.topics} : {keywords: page.topics})} :
      isProfile ? {mainEntity: {'@id': person['@id']}} : {about: {'@id': person['@id']}}),
  }]};
  const clean = html.replace(/<!-- agent-metadata:start -->[\s\S]*?<!-- agent-metadata:end -->\n?/g, '');
  const canonical = /rel=["']canonical["']/.test(clean) ? '' : `<link rel="canonical" href="${page.url}">\n`;
  const description = isProfile
    ? summary
    : 'content' in page ? firstProseParagraph(page.content) || page.title : `${page.title} de Vinícius Félix. Registros e fontes publicados no site pessoal.`;
  const escapedDescription = description.replace(/&/g, '&amp;').replace(/"/g, '&quot;').replace(/</g, '&lt;');
  const meta = /name=["']description["']/.test(clean) ? '' : `<meta name="description" content="${escapedDescription}">\n`;
  const block = `<!-- agent-metadata:start -->\n${canonical}${meta}<link rel="describedby" href="${base}/llms.txt" type="text/plain">\n` +
    `<link rel="alternate" type="text/markdown" href="${page.markdownUrl}">\n<link rel="alternate" type="application/json" href="${base}/agent-index.json" title="Índice estruturado">\n` +
    `<script type="application/ld+json">${JSON.stringify(graph).replace(/</g, '\\u003c')}</script>\n<!-- agent-metadata:end -->\n`;
  const result = clean.replace('</head>', block + '</head>');
  if (check) {
    if (result !== html) throw new Error(`Outdated agent metadata: ${page.url}`);
  } else if (result !== html) await Deno.writeTextFile(path, result);
}
console.log(`Agent assets ${check ? 'verified' : 'generated'}: ${work.length} roles, ${education.length} qualifications, ${documents.length} source documents.`);
// Verify the published route and anchor behind every internal exported link.
const links = [cv.url, ...work.map(w => w.source), ...education.map(e => e.source),
  ...documents.flatMap(document => [document.url, document.markdownUrl]),
  ...articleDocuments.flatMap(article => [article.url, article.markdownUrl]),
  ...Object.values(assets).flatMap(markdownLinks)];
for (const link of new Set(links)) {
  // Incremental Quarto renders may not have emitted every public page yet.
  await validatePublishedLink(link, base, output, !check);
}
console.log('PASS: exported local destinations and anchors.');
