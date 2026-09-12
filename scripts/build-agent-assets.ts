// Quarto's bundled Deno runs this after full and incremental renders.
// Public QMD content remains the source of truth; no separate biography to edit.
import { readContent } from './content-source.ts';
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
  'header-certifications': 'Certificações',
  'header-participations': 'Participações',
  'header-awards': 'Prêmios',
};
const absolute = (url: string) => {
  const target = new URL(url, base + '/');
  if (target.origin === new URL(base).origin) target.pathname = target.pathname.replace(/\.qmd$/, '.html');
  return target.href;
};
function markdown(source: string): string {
  return source.replace(/\r\n/g, '\n')
    .replace(/```\{=html\}[\s\S]*?```/g, '')
    .replace(/^:::.*$/gm, '')
    .replace(/!\[[^\]]*\]\([^)]*\)(?:\{[^}]*\})?/g, '')
    .replace(/<[^>]+>/g, '')
    .replace(/\]\((\/[^)]+)\)/g, (_, url) => '](' + absolute(url) + ')')
    .replace(/\n{3,}/g, '\n\n').trim();
}
const plain = (s: string) => s.replace(/\[([^\]]+)\]\([^)]+\)/g, '$1').replace(/\*\*/g, '').trim();
const source: Record<string, string> = {};
const texts: Record<string, string> = {};
for (const name of Object.keys(names)) {
  source[name] = await readContent(`${name}.qmd`);
  texts[name] = markdown(source[name]);
}
const summary = texts.index.split('\n\n').find(p => p.trim() && !/^[#|\[!]/.test(p.trim()));
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
const revision = Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256',
  new TextEncoder().encode(JSON.stringify(source))))).map(x => x.toString(16).padStart(2, '0')).join('');
const documents = Object.keys(names).filter(n => n !== 'header-about').map(name => ({
  title: names[name], url: absolute(`/${name}.html`), markdownUrl: absolute(`/${name}.html.md`),
  content: texts[name],
}));
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
assets['curriculo.json'] = JSON.stringify(cv, null, 2) + '\n';
assets['curriculo.md'] = `# Currículo de Vinícius Félix\n\nFonte: ${base}/\n\n${cv.scope}\n\n` +
  `${cv.summary}\n\n## Formação acadêmica\n\n` +
  education.map(e => `### ${e.qualification}\n\n${e.institution} | ${e.startDate} a ${e.endDate}\n\nTrabalho final: ${e.thesis}`).join('\n\n') +
  '\n\n## Experiência profissional\n\n' +
  work.map(w => `### ${w.organization}: ${w.position}\n\n${w.startDate} a ${w.endDate}\n\n${w.summary}`).join('\n\n') +
  `\n\n[Trajetória em detalhe](${base}/header-experience.html) · [LinkedIn](${sameAs[1]}) · [GitHub](${sameAs[0]})\n`;
assets['llms.txt'] = `# Vinícius Félix\n\n> Estatístico e mestre em Bioestatística. Perfil público, trajetória profissional, formação e produção acadêmica.\n\n` +
  `O conteúdo está em português. O currículo é gerado das mesmas fontes das páginas públicas. Datas no JSON usam YYYY-MM; não implicam um dia específico. Consulte as fontes citadas para contexto e resultados. Cargos encerrados não são vínculos atuais.\n\n` +
  `## Currículo\n\n- [Currículo em Markdown](${base}/curriculo.md): resumo da experiência profissional e formação acadêmica.\n- [Currículo em JSON](${base}/curriculo.json): experiência e formação em campos estruturados, com fontes.\n\n## Perfil e fontes\n\n` +
  documents.map(d => `- [${d.title}](${d.markdownUrl}): versão textual da página pública.`).join('\n') +
  `\n\n## Optional\n\n- [Acervo de artigos](${base}/writing.html): textos técnicos, distintos das publicações acadêmicas do currículo.\n- [Conteúdo completo para agentes](${base}/llms-full.txt): perfil, trajetória e documentos complementares na íntegra.\n`;
assets['llm.txt'] = assets['llms.txt']; // Compatibility with the user's singular spelling.
assets['llms-full.txt'] = '# Vinícius Félix: perfil e documentos completos\n\n' +
  documents.map(d => `## ${d.title}\n\nFonte: ${d.url}\n\n${d.content.replace(/^#/gm, '##')}`).join('\n\n') + '\n';
for (const [path, content] of Object.entries(assets)) {
  // Static servers may omit charset for Markdown/text. A UTF-8 BOM makes
  // browser decoding explicit; JSON stays BOM-free for strict consumers.
  const encoded = new TextEncoder().encode(/\.(md|txt)$/.test(path) ? '\uFEFF' + content : content);
  if (check) {
    const actual = await Deno.readFile(`${output}/${path}`);
    if (actual.length !== encoded.length || !actual.every((byte, i) => byte === encoded[i])) throw new Error(`Outdated agent asset: ${path}`);
  } else await Deno.writeFile(`${output}/${path}`, encoded);
}
// The marker makes repeated and partial renders idempotent.
for (const name of Object.keys(names)) {
  const path = `${output}/${name}.html`;
  let html: string;
  try { html = await Deno.readTextFile(path); } catch (error) {
    if (!check && error instanceof Deno.errors.NotFound) continue;
    throw error;
  }
  const url = absolute(`/${name}.html`);
  const graph = {'@context': 'https://schema.org', '@graph': [person, {
    '@type': ['index', 'header-about'].includes(name) ? 'ProfilePage' : 'WebPage',
    '@id': url + '#page', url, name: names[name], inLanguage: 'pt-BR',
    ...(['index', 'header-about'].includes(name) ? {mainEntity: {'@id': person['@id']}} : {about: {'@id': person['@id']}}),
  }]};
  const clean = html.replace(/<!-- agent-metadata:start -->[\s\S]*?<!-- agent-metadata:end -->\n?/g, '');
  const canonical = /rel=["']canonical["']/.test(clean) ? '' : `<link rel="canonical" href="${url}">\n`;
  const description = ['index', 'header-about'].includes(name)
    ? summary
    : `${names[name]} de Vinícius Félix. Registros e fontes publicados no site pessoal.`;
  const escapedDescription = description.replace(/&/g, '&amp;').replace(/"/g, '&quot;').replace(/</g, '&lt;');
  const meta = /name=["']description["']/.test(clean) ? '' : `<meta name="description" content="${escapedDescription}">\n`;
  const block = `<!-- agent-metadata:start -->\n${canonical}${meta}<link rel="describedby" href="${base}/llms.txt" type="text/plain">\n` +
    `<link rel="alternate" type="text/markdown" href="${url}.md">\n<link rel="alternate" type="application/json" href="${base}/curriculo.json" title="Currículo estruturado">\n` +
    `<script type="application/ld+json">${JSON.stringify(graph).replace(/</g, '\\u003c')}</script>\n<!-- agent-metadata:end -->\n`;
  const result = clean.replace('</head>', block + '</head>');
  if (check) {
    if (result !== html) throw new Error(`Outdated agent metadata: ${name}`);
  } else await Deno.writeTextFile(path, result);
}
console.log(`Agent assets ${check ? 'verified' : 'generated'}: ${work.length} roles, ${education.length} qualifications, ${documents.length} source documents.`);
// Verify the published route and anchor behind every internal exported link.
const links = [cv.url, ...work.map(w => w.source), ...education.map(e => e.source),
  ...Object.values(assets).flatMap(text => [...text.matchAll(/\]\((https?:\/\/[^\s)]+)/g)].map(m => m[1]))];
for (const link of new Set(links)) {
  const url = new URL(link);
  if (url.origin !== new URL(base).origin) continue;
  let path = decodeURIComponent(url.pathname);
  if (path.endsWith('/')) path += 'index.html';
  const file = `${output}/${path.replace(/^\//, '')}`;
  const info = await Deno.stat(file);
  if (!info.isFile) throw new Error(`Invalid exported target: ${link}`);
  if (url.hash && path.endsWith('.html')) {
    const html = await Deno.readTextFile(file);
    const anchor = decodeURIComponent(url.hash.slice(1));
    if (!html.includes(`id="${anchor}"`)) throw new Error(`Missing exported anchor: ${link}`);
  }
}
console.log('PASS: exported local destinations and anchors.');
