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
