"""Build the shared archive from the repository's single-line YAML metadata.

No third-party dependencies. Unsupported metadata fails explicitly instead of
silently generating an incomplete archive. Use --check in validation workflows.
"""
from argparse import ArgumentParser
from datetime import date
from pathlib import Path
import html
import json
import re

ROOT = Path(__file__).resolve().parents[1]
TARGET = ROOT / '_content/writing.qmd'
RECENT_TARGET = ROOT / '_content/recent-writing.qmd'


def scalar(text):
    text = text.strip()
    if text.startswith('"'):
        return json.loads(text)
    if text.startswith("'") and text.endswith("'"):
        return text[1:-1].replace("''", "'")
    if text in ('|', '>') or not text:
        raise ValueError('Expected a nonempty single-line metadata value')
    return text


def read_post(path):
    parts = re.split(r'^---\s*$', path.read_text(encoding='utf-8-sig'), maxsplit=2, flags=re.M)
    if len(parts) != 3 or parts[0].strip():
        raise ValueError(f'{path}: missing YAML front matter')
    fields = dict(re.findall(r'^(title|date|categories|lang):[ \t]*(.+)$', parts[1], re.M))
    try:
        title = scalar(fields['title'])
        published = scalar(fields['date'])
        date.fromisoformat(published)
        categories = fields.get('categories', '[]').strip()
        if not (categories.startswith('[') and categories.endswith(']')):
            raise ValueError('categories must use an inline list')
        categories = ', '.join(scalar(item) for item in categories[1:-1].split(',') if item.strip())
        language = scalar(fields.get('lang', 'en')).lower()
        flags = {
            'en': ('EN', 'Em inglês', 'en'),
            'en-us': ('EN', 'Em inglês', 'en'),
            'pt': ('PT', 'Em português', 'pt'),
            'pt-br': ('PT', 'Em português', 'pt'),
        }
        if language not in flags:
            raise ValueError(f'unsupported language: {language}')
        return published, title, categories, path.parent.name, flags[language]
    except (KeyError, ValueError) as error:
        raise ValueError(f'{path}: invalid metadata: {error}') from error


def render(posts, limit=None, heading=2):
    rows = []
    ordered = sorted(posts, reverse=True)
    if limit is not None:
        ordered = ordered[:limit]
    for published, title, categories, slug, (label, language, code) in ordered:
        rows.append(f'<article class="archive-entry" data-language="{code}" data-categories="{html.escape(categories, quote=True)}"><time datetime="{published}">{published}</time><div><h{heading}><span class="post-language" role="img" aria-label="{language}">{label}</span><a href="/posts/{slug}/index.html">{html.escape(title)}</a></h{heading}><p>{html.escape(categories)}</p></div></article>')
    return ('Estatística, matemática e engenharia de dados. Textos do acervo, preservados no idioma original.\n\n'
            '```{=html}\n' + '\n'.join(rows) + '\n```\n')


def main():
    parser = ArgumentParser(description=__doc__)
    parser.add_argument('--check', action='store_true', help='Fail if the shared archive is outdated; do not write')
    args = parser.parse_args()
    posts = [read_post(path) for path in sorted(ROOT.glob('posts/*/index.qmd'))]
    if not posts:
        raise SystemExit('No posts found; archive was not changed.')
    outputs = ((TARGET, render(posts)), (RECENT_TARGET, render(posts, limit=3, heading=3)))
    if args.check:
        if any(not path.exists() or path.read_text(encoding='utf-8') != content for path, content in outputs):
            raise SystemExit('Archive or recent writing is outdated. Run python scripts/build-writing.py')
    else:
        for path, content in outputs:
            if not path.exists() or path.read_text(encoding='utf-8') != content:
                path.write_text(content, encoding='utf-8')
    print(f'Portuguese archive: {len(posts)} articles.')


if __name__ == '__main__':
    main()
