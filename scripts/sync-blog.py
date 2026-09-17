"""Import public blog articles from ghost-writer/main."""
import argparse
import hashlib
import json
from pathlib import Path, PurePosixPath
import re
import subprocess

ROOT = Path(__file__).resolve().parents[1]
REMOTE = 'https://github.com/vbfelix/ghost-writer.git'
PREFIX = 'posts/blog/'


def git(cache, *args):
    return subprocess.run(['git', '-c', f'safe.directory={cache.as_posix()}',
                           f'--git-dir={cache}', *args], check=True,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE).stdout


def digest(data):
    return hashlib.sha256(data).hexdigest()


def source_path(name):
    path = PurePosixPath(name)
    if path.is_absolute() or any(part in ('.', '..') for part in path.parts) or '\\' in name or ':' in name:
        raise ValueError(f'Unsafe source path: {name}')
    return path


def parse_source(data):
    text = data.decode('utf-8')
    parts = re.split(r'^---\s*$', text, maxsplit=2, flags=re.M)
    if len(parts) != 3 or parts[0].strip():
        raise ValueError('Missing YAML front matter.')
    metadata, document = parts[1], parts[2]

    def required(field):
        match = re.search(rf'^{field}:\s*["\']?(.+?)["\']?\s*$', metadata, re.M)
        if not match:
            raise ValueError(f'Missing {field} metadata.')
        return match.group(1).strip('"\'')

    title = required('titulo')
    published = required('data')
    categories = re.search(r'^\s+categories:\s*\[(.*?)\]\s*$', metadata, re.M)
    language = re.search(r'^\s+lang:\s*(.+?)\s*$', metadata, re.M)
    if not categories or not language:
        raise ValueError('Missing site categories or language metadata.')
    body = document.split('\n## Blog\n', 1)
    if len(body) != 2:
        raise ValueError('Missing public Blog section.')
    public = re.split(r'\n## (?:Evidências|Notes)\n', body[1], maxsplit=1)[0].strip()
    public = re.sub(r'^### Resumo\n\n', '', public)
    if not public:
        raise ValueError('Public Blog section is empty.')
    categories_value = ', '.join(item.strip().strip('"\'') for item in categories.group(1).split(','))
    return title, published, categories_value, language.group(1).strip('"\''), public


def render_article(data):
    title, published, categories, language, public = parse_source(data)
    return ('---\n'
            f'title: {json.dumps(title, ensure_ascii=False)}\n'
            'author: "Vinícius Félix"\n'
            f'date: "{published}"\n'
            f'categories: [{categories}]\n'
            f'lang: {language}\n'
            '---\n\n'
            f'{public}\n').encode('utf-8')


def allocate_targets(root, incoming, old):
    targets = {name: record['target'] for name, record in old['files'].items() if name in incoming}
    occupied = {Path(target).parts[1] for target in targets.values()}
    for directory in (root / 'posts').glob('[0-9][0-9][0-9][0-9]-*'):
        occupied.add(directory.name)
    indexes = [int(name[:4]) for name in occupied if re.match(r'^\d{4}-', name)]
    next_index = max(indexes, default=31) + 1
    for name in sorted(set(incoming) - set(targets)):
        slug = source_path(name).stem[11:]
        while f'{next_index:04d}-{slug}' in occupied:
            next_index += 1
        directory = f'{next_index:04d}-{slug}'
        targets[name] = f'posts/{directory}/index.qmd'
        occupied.add(directory)
        next_index += 1
    return targets


def synchronize(root=ROOT, remote=REMOTE, check=False):
    root = Path(root).resolve()
    manifest = root / 'blog-source.json'
    old = json.loads(manifest.read_text(encoding='utf-8')) if manifest.exists() else {'files': {}}
    if check:
        if not manifest.exists():
            raise ValueError('Run sync-blog.py before checking the imported snapshot.')
        for name, record in old['files'].items():
            path = root / record['target']
            if not path.is_file() or digest(path.read_bytes()) != record['rendered']:
                raise ValueError(f'Imported file changed locally: {name}. Edit ghost-writer/main instead.')
        print(f"Blog snapshot verified: {old['commit']} ({len(old['files'])} files). No network check.")
        return
    cache = root / '.cache' / 'portfolio-source.git'
    if not cache.exists():
        cache.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run(['git', 'init', '--bare', str(cache)], check=True, stdout=subprocess.PIPE)
    git(cache, 'fetch', '--depth=1', remote, 'refs/heads/main')
    commit = git(cache, 'rev-parse', 'FETCH_HEAD').decode().strip()
    records = git(cache, 'ls-tree', '-rz', commit, '--', PREFIX).split(b'\0')
    incoming = {}
    for record in filter(None, records):
        meta, raw_path = record.split(b'\t', 1)
        mode, kind, blob = meta.split()
        name = raw_path.decode('utf-8')[len(PREFIX):]
        if name == '.gitkeep':
            continue
        if mode not in (b'100644', b'100755') or kind != b'blob' or not name.endswith('.md'):
            raise ValueError(f'Unsupported source entry: {name}')
        source_path(name)
        incoming[name] = git(cache, 'cat-file', 'blob', blob.decode())
    for name, record in old['files'].items():
        path = root / record['target']
        if not path.is_file() or digest(path.read_bytes()) != record['rendered']:
            raise ValueError(f'Local edit to imported file: {name}; reconcile before syncing.')
    rendered = {name: render_article(data) for name, data in incoming.items()}
    targets = allocate_targets(root, incoming, old)
    for name, output in rendered.items():
        path = root / targets[name]
        if name not in old['files'] and path.exists() and path.read_bytes() != output:
            raise ValueError(f'Unmanaged local file conflicts with main: {targets[name]}; reconcile before syncing.')
    for name, output in rendered.items():
        path = root / targets[name]
        path.parent.mkdir(parents=True, exist_ok=True)
        if not path.exists() or path.read_bytes() != output:
            path.write_bytes(output)
    for name, record in old['files'].items():
        if name not in incoming:
            path = root / record['target']
            if path.is_file():
                path.unlink()
            output = root / 'docs' / Path(record['target']).with_suffix('.html')
            if output.is_file():
                output.unlink()
    state = {'repository': remote, 'branch': 'main', 'directory': PREFIX.rstrip('/'), 'commit': commit,
             'files': {name: {'source': digest(data), 'rendered': digest(rendered[name]), 'target': targets[name]}
                       for name, data in sorted(incoming.items())}}
    manifest.write_text(json.dumps(state, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')
    print(f'Blog synchronized from main@{commit[:12]}: {len(incoming)} articles.')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--check', action='store_true', help='Validate the imported snapshot offline.')
    args = parser.parse_args()
    try:
        synchronize(check=args.check)
    except (ValueError, OSError, subprocess.CalledProcessError) as exc:
        parser.exit(1, f'Blog sync failed: {exc}\n')
