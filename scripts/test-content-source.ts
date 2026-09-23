import { articleLanguage, findArticleSources, firstProseParagraph, markdownLinks, readContent, readFrontMatter, validatePublishedLink } from './content-source.ts';

function assert(value: boolean, message: string) { if (!value) throw new Error(message); }
const temporary = await Deno.makeTempDir();
try {
  await Deno.mkdir(`${temporary}/content`);
  await Deno.writeTextFile(`${temporary}/index.qmd`, '---\ntitle: Home\n---\n{{< include content/about.qmd >}}');
  await Deno.writeTextFile(`${temporary}/about.qmd`, '{{< include content/about.qmd >}}');
  await Deno.writeTextFile(`${temporary}/content/about.qmd`, 'Vinícius\n{{< include content/table.qmd >}}');
  await Deno.writeTextFile(`${temporary}/content/table.qmd`, 'Versão inicial');
  assert((await readContent(`${temporary}/index.qmd`)).includes('Versão inicial'), 'Nested include missing');
  await Deno.writeTextFile(`${temporary}/content/table.qmd`, 'Conteúdo atualizado: Estatística');
  const home = await readContent(`${temporary}/index.qmd`);
  assert(home === await readContent(`${temporary}/about.qmd`), 'Routes diverged');
  assert(home.includes('Conteúdo atualizado: Estatística') && !home.includes('Versão inicial'), 'Stale content');
  assert(!home.includes('title: Home') && !home.includes('{{<'), 'Metadata or include leaked');
  await Deno.writeTextFile(`${temporary}/content/table.qmd`, '{{< include content/about.qmd >}}');
  let cyclic = false;
  try { await readContent(`${temporary}/index.qmd`); } catch (error) { cyclic = String(error).includes('Cyclic'); }
  assert(cyclic, 'Cycle must fail explicitly');
  await Deno.writeTextFile(`${temporary}/content/table.qmd`, '{{< include missing.qmd >}}');
  let missing = false;
  try { await readContent(`${temporary}/index.qmd`); } catch (error) { missing = error instanceof Deno.errors.NotFound; }
  assert(missing, 'Missing include must fail explicitly');
  await Deno.mkdir(`${temporary}/posts/example`, {recursive:true});
  await Deno.writeTextFile(`${temporary}/posts/example/index.qmd`, '---\ntitle: "Teste"\ncategories: [AI, data]\nlang: pt-BR\n---\nTexto');
  await Deno.writeTextFile(`${temporary}/posts/index.qmd`, '---\ntitle: Raiz\n---');
  const articleSources = await findArticleSources(`${temporary}/posts`);
  assert(articleSources.length === 2, 'Article discovery must include every index.qmd');
  const metadata = await Promise.all(articleSources.map(async path => readFrontMatter(await Deno.readTextFile(path))));
  const rootMetadata = metadata.find(item => item.title === 'Raiz');
  const nestedMetadata = metadata.find(item => item.title === 'Teste');
  assert(rootMetadata?.title === 'Raiz', 'Front matter title missing');
  assert(Array.isArray(nestedMetadata?.categories) && nestedMetadata.categories.join(',') === 'AI,data', 'Inline categories missing');
  assert(articleLanguage('post') === 'en' && articleLanguage('portfolio') === 'pt-BR', 'Article default language mismatch');
  assert(articleLanguage('post', 'pt-BR') === 'pt-BR', 'Declared article language ignored');
  assert(firstProseParagraph('```{r}\nprint("code")\n```\n\nPrimeira frase.\n\nOutra.') === 'Primeira frase.', 'Code chunk leaked into article description');
  assert(markdownLinks('[Broken](https://vbfelix.github.io/missing.html)').includes('https://vbfelix.github.io/missing.html'), 'Exported links missing from validation');
  await Deno.mkdir(`${temporary}/published`);
  await Deno.writeTextFile(`${temporary}/published/index.html`, '<h1 id="present">Present</h1>');
  await validatePublishedLink('https://vbfelix.github.io/#present', 'https://vbfelix.github.io', `${temporary}/published`);
  let brokenAbsolute = false;
  try { await validatePublishedLink('https://vbfelix.github.io/missing.html', 'https://vbfelix.github.io', `${temporary}/published`); }
  catch (error) { brokenAbsolute = error instanceof Deno.errors.NotFound; }
  assert(brokenAbsolute, 'Missing same-origin absolute target must fail');
  await validatePublishedLink('https://vbfelix.github.io/relper/index.html', 'https://vbfelix.github.io', `${temporary}/published`);
} finally {
  // This directory is created by this test, never derived from a user path.
  await Deno.remove(temporary, {recursive:true});
}
const home = await readContent('index.qmd');
const about = await readContent('header-about.qmd');
assert(home.trim() === about.trim(), 'Home and about must share one body');
const table = (await readContent('_content/experience-table.qmd')).trim();
assert(home.includes(table), 'Home must include the canonical work table');
assert((await readContent('header-experience.qmd')).includes(table), 'Experience must include the canonical work table');
assert(!home.includes('{{<'), 'Unresolved source shortcode');
let workTables = 0, biographies = 0;
for await (const entry of Deno.readDir('_content')) {
  if (!entry.isFile || !entry.name.endsWith('.qmd')) continue;
  const text = await Deno.readTextFile(`_content/${entry.name}`);
  if (text.includes('| Período | Organização e função | Trabalho |')) workTables++;
  if (text.includes('::: {.about-hero}')) biographies++;
}
assert(workTables === 1, 'Keep exactly one editable professional table');
assert(biographies === 1, 'Keep exactly one editable About body');
console.log('PASS: shared routes, fresh nested includes, UTF-8, cycle and missing-file rejection.');
