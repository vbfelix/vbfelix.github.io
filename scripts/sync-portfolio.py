"""Import portfolio files from a pinned snapshot of ghost-writer/main (stdlib only)."""
import argparse
import hashlib
import json
from pathlib import Path, PurePosixPath
import subprocess

ROOT = Path(__file__).resolve().parents[1]
REMOTE = 'https://github.com/vbfelix/ghost-writer.git'
PREFIX = 'posts/portfolio/'


def local_presentation(name):
    """The site owns thumbnail artwork and per-article display metadata."""
    path = PurePosixPath(name)
    return path.name == '_metadata.yml' or path.stem.casefold() == 'thumbnail'


def git(cache, *args):
    return subprocess.run(['git', '-c', f'safe.directory={cache.as_posix()}',
                           f'--git-dir={cache}', *args], check=True,
                          stdout=subprocess.PIPE, stderr=subprocess.PIPE).stdout


def digest(data):
    return hashlib.sha256(data).hexdigest()


def target(root, name):
    parts = PurePosixPath(name)
    if parts.is_absolute() or any(p in ('..', '.') for p in parts.parts) or '\\' in name or ':' in name:
        raise ValueError(f'Unsafe source path: {name}')
    result = root / 'portfolio' / name
    if not result.resolve().is_relative_to((root / 'portfolio').resolve()):
        raise ValueError(f'Path escapes portfolio: {name}')
    return result


def synchronize(root=ROOT, remote=REMOTE, check=False):
    root = Path(root).resolve()
    manifest = root / 'portfolio-source.json'
    old = json.loads(manifest.read_text(encoding='utf-8')) if manifest.exists() else {'files': {}}
    if check:
        if not manifest.exists():
            raise ValueError('Run sync-portfolio.py before checking the imported snapshot.')
        for name, sha in old['files'].items():
            path = target(root, name)
            if not path.is_file() or digest(path.read_bytes()) != sha:
                raise ValueError(f'Imported file changed locally: {name}. Edit ghost-writer/main instead.')
        print(f"Portfolio snapshot verified: {old['commit']} ({len(old['files'])} files). No network check.")
        return
    cache = root / '.cache' / 'portfolio-source.git'
    if not cache.exists():
        cache.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run(['git', 'init', '--bare', str(cache)], check=True, stdout=subprocess.PIPE)
    # Fetch exactly remote main; never read a sibling repository's checkout or local branch.
    git(cache, 'fetch', '--depth=1', remote, 'refs/heads/main')
    commit = git(cache, 'rev-parse', 'FETCH_HEAD').decode().strip()
    records = git(cache, 'ls-tree', '-rz', commit, '--', PREFIX).split(b'\0')
    incoming = {}
    for record in filter(None, records):
        meta, raw_path = record.split(b'\t', 1)
        mode, kind, blob = meta.split()
        name = raw_path.decode('utf-8')[len(PREFIX):]
        if name == '.gitkeep' or local_presentation(name):
            continue
        if mode not in (b'100644', b'100755') or kind != b'blob':
            raise ValueError(f'Unsupported source entry: {name}')
        target(root, name)
        incoming[name] = git(cache, 'cat-file', 'blob', blob.decode())
    # Validate every change before writing; never erase edits or unrelated local assets.
    for name, sha in old['files'].items():
        if local_presentation(name):
            continue
        path = target(root, name)
        if not path.is_file() or digest(path.read_bytes()) != sha:
            raise ValueError(f'Local edit to imported file: {name}; reconcile it before syncing.')
    for name, data in incoming.items():
        path = target(root, name)
        adopted = old.get('pendingMigration', {}).get(name)
        if name not in old['files'] and path.exists() and path.read_bytes() != data and digest(path.read_bytes()) != adopted:
            raise ValueError(f'Unmanaged local file conflicts with main: {name}; reconcile before syncing.')
    for name, data in incoming.items():
        path = target(root, name)
        path.parent.mkdir(parents=True, exist_ok=True)
        if not path.exists() or path.read_bytes() != data:
            path.write_bytes(data)
    for name in old['files'].keys() - incoming.keys():
        if local_presentation(name):
            continue
        target(root, name).unlink()
        output = root / 'docs' / 'portfolio' / name
        if output.suffix == '.qmd':
            output = output.with_suffix('.html')
        if output.resolve().is_relative_to((root / 'docs' / 'portfolio').resolve()) and output.is_file():
            output.unlink()
    state = {'repository': remote, 'branch': 'main', 'directory': PREFIX.rstrip('/'),
             'commit': commit, 'files': {name: digest(data) for name, data in sorted(incoming.items())}}
    print(f'Portfolio synchronized from main@{commit[:12]}: {len(incoming)} files.')
    unmanaged = [p.relative_to(root).as_posix() for p in (root / 'portfolio').glob('*/index.qmd')
                 if p.relative_to(root / 'portfolio').as_posix() not in incoming]
    if unmanaged:
        print('Pending migration (preserved locally, absent from upstream): ' + ', '.join(unmanaged))
    state['pendingMigration'] = {p.removeprefix('portfolio/'): digest((root / p).read_bytes()) for p in unmanaged}
    manifest.write_text(json.dumps(state, ensure_ascii=False, indent=2) + '\n', encoding='utf-8')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--check', action='store_true', help='Validate committed snapshot offline; does not fetch.')
    args = parser.parse_args()
    try:
        synchronize(check=args.check)
    except (ValueError, OSError, subprocess.CalledProcessError) as exc:
        parser.exit(1, f'Portfolio sync failed: {exc}\n')
