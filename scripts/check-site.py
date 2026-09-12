"""Check generated local navigation/assets and homepage structure after render."""
from pathlib import Path
from html.parser import HTMLParser
from urllib.parse import urlsplit, unquote

ROOT = Path(__file__).resolve().parents[1] / 'docs'

class Page(HTMLParser):
    def __init__(self, path):
        super().__init__()
        self.path, self.links, self.ids, self.images = path, [], set(), []
        self.h1 = 0
        self.feed(path.read_text(encoding='utf-8'))

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if 'id' in attrs:
            self.ids.add(attrs['id'])
        if tag == 'h1':
            self.h1 += 1
        if tag in ('a', 'img', 'script', 'link'):
            url = attrs.get('href') if tag in ('a', 'link') else attrs.get('src')
            if url:
                self.links.append(url)
        if tag == 'img':
            self.images.append(attrs)

def main():
    pages = {p.resolve(): Page(p) for p in ROOT.rglob('*.html') if 'site_libs' not in p.parts}
    errors = []
    targets = sorted(pages)
    if not targets or not (ROOT / 'index.html').is_file():
        raise SystemExit('No rendered homepage. Run quarto render first.')
    repository = ROOT.parent
    sources = [*repository.glob('*.qmd'), *repository.glob('pt-br/*.html'), *repository.glob('posts/*/index.qmd'), *repository.glob('portfolio/*/index.qmd')]
    expected = {(ROOT / source.relative_to(repository).with_suffix('.html')).resolve() for source in sources}
    for missing in sorted(expected - pages.keys()):
        errors.append(f'Missing rendered page: {missing.relative_to(ROOT)}')
    for stale in sorted(pages.keys() - expected):
        errors.append(f'Rendered page without source: {stale.relative_to(ROOT)}')
    for source in repository.glob('pt-br/*.html'):
        output = ROOT / 'pt-br' / source.name
        if output.exists() and output.read_bytes() != source.read_bytes():
            errors.append(f'Outdated redirect: pt-br/{source.name}')
        if not (ROOT / source.name).is_file():
            errors.append(f'Missing redirect destination: {source.name}')
    for path in targets:
        page = pages[path.resolve()]
        for raw in page.links:
            url = urlsplit(raw)
            if url.scheme or url.netloc:
                continue
            target = ((ROOT / unquote(url.path.lstrip('/'))) if url.path.startswith('/') else (path.parent / unquote(url.path))).resolve() if url.path else path.resolve()
            if not target.is_relative_to(ROOT.resolve()):
                errors.append(f'{path}: link escapes output directory: {raw}')
                continue
            if target.is_dir():
                target /= 'index.html'
            if not target.exists():
                errors.append(f'{path.relative_to(ROOT)}: missing {raw}')
            elif url.fragment and target in pages and unquote(url.fragment) not in pages[target].ids:
                errors.append(f'{path.relative_to(ROOT)}: missing anchor {raw}')
        if path == (ROOT / 'index.html').resolve():
            if page.h1 != 1:
                errors.append(f'{path}: expected one h1, found {page.h1}')
            if any('alt' not in img for img in page.images):
                errors.append(f'{path}: image without alt')
    print(f'{len(pages)} HTML pages; all local links, anchors and assets checked.')
    if errors:
        print('\n'.join(errors))
        raise SystemExit(1)
    print('PASS: local links, anchors, assets, home headings, and image descriptions.')

if __name__ == '__main__':
    main()
