// Resolve the same relative include graph used by Quarto, from the public QMD.
export async function readContent(path: string, parents: string[] = [], baseDirectory?: string): Promise<string> {
  const file = await Deno.realPath(path);
  if (parents.includes(file)) throw new Error(`Cyclic content include: ${path}`);
  const text = (await Deno.readTextFile(file)).replace(/\r\n/g, '\n');
  const body = text.replace(/^---\n[\s\S]*?\n---(?:\n|$)/, '');
  // Quarto resolves nested includes against the public document's directory.
  const directory = baseDirectory ?? file.slice(0, Math.max(file.lastIndexOf('/'), file.lastIndexOf('\\')) + 1);
  const pattern = /\{\{<\s*include\s+([^>]+?)\s*>\}\}/g;
  let result = '', offset = 0;
  for (const match of body.matchAll(pattern)) {
    const relative = match[1].trim().replace(/^["']|["']$/g, '');
    result += body.slice(offset, match.index) + await readContent(directory + relative, [...parents, file], directory);
    offset = match.index! + match[0].length;
  }
  return result + body.slice(offset);
}

export async function findArticleSources(directory: string): Promise<string[]> {
  const found: string[] = [];
  for await (const entry of Deno.readDir(directory)) {
    const path = `${directory}/${entry.name}`;
    if (entry.isDirectory) found.push(...await findArticleSources(path));
    else if (entry.isFile && entry.name === 'index.qmd') found.push(path);
  }
  return found.sort();
}

export function readFrontMatter(source: string): Record<string, string | string[]> {
  const block = source.match(/^---\n([\s\S]*?)\n---(?:\n|$)/)?.[1] ?? '';
  const fields: Record<string, string | string[]> = {};
  let current = '';
  for (const line of block.split('\n')) {
    const inline = line.match(/^([\w-]+):\s*(.*)$/);
    if (inline) {
      current = inline[1];
      const value = inline[2].trim().replace(/^['"]|['"]$/g, '');
      fields[current] = /^\[.*\]$/.test(value) ? value.slice(1, -1).split(',').map(x => x.trim().replace(/^['"]|['"]$/g, '')).filter(Boolean) : value;
    } else if (current && /^-\s+/.test(line)) {
      const value = line.replace(/^-\s+/, '').trim().replace(/^['"]|['"]$/g, '');
      fields[current] = [...(Array.isArray(fields[current]) ? fields[current] : []), value];
    }
  }
  return fields;
}

export function articleLanguage(type: 'post' | 'portfolio', declared?: string | string[]): string {
  if (Array.isArray(declared)) throw new Error('Article language must be a scalar');
  return declared?.trim() || (type === 'post' ? 'en' : 'pt-BR');
}

export function firstProseParagraph(content: string): string {
  const withoutCode = content.replace(/^```[^\n]*\n[\s\S]*?^```[ \t]*$/gm, '');
  return withoutCode.split(/\n\s*\n/)
    .map(block => block.replace(/\s+/g, ' ').trim())
    .find(block => block && !/^(?:#|!\[|\||>|\$\$|:::)/.test(block)) || '';
}

export function markdownLinks(content: string): string[] {
  return [...content.matchAll(/\]\((https?:\/\/[^\s)]+)/g)].map(match => match[1]);
}

export async function validatePublishedLink(link: string, base: string, output: string, allowMissing = false): Promise<void> {
  const url = new URL(link);
  if (url.origin !== new URL(base).origin || url.pathname.startsWith('/relper/')) return;
  let path = decodeURIComponent(url.pathname);
  if (path.includes('\\') || path.split('/').includes('..')) throw new Error(`Invalid exported target: ${link}`);
  if (path.endsWith('/')) path += 'index.html';
  const file = `${output}/${path.replace(/^\//, '')}`;
  let info: Deno.FileInfo;
  try { info = await Deno.stat(file); }
  catch (error) {
    if (allowMissing && error instanceof Deno.errors.NotFound) return;
    throw error;
  }
  if (!info.isFile) throw new Error(`Invalid exported target: ${link}`);
  if (url.hash && path.endsWith('.html')) {
    const html = await Deno.readTextFile(file);
    const anchor = decodeURIComponent(url.hash.slice(1));
    if (!html.includes(`id="${anchor}"`)) throw new Error(`Missing exported anchor: ${link}`);
  }
}
