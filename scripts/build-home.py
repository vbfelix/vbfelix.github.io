"""Build selected home cards from the portfolio's canonical article metadata."""

from argparse import ArgumentParser
from pathlib import Path
from html import escape
import json
import re

ROOT = Path(__file__).resolve().parents[1]
TARGET = ROOT / '_content/home-selected.qmd'
SELECTED = (
    '0040-criando-meu-clone-com-ia',
    '0038-arquitetura-fontes-externas',
    '0032-ux-produtos-fisicos',
)


def value(frontmatter, name, path):
    match = re.search(rf'^{re.escape(name)}:[ \t]*(.+)$', frontmatter, re.M)
    if not match:
        raise ValueError(f'{path}: missing {name}')
    scalar = match.group(1).strip()
    if scalar.startswith('"'):
        return json.loads(scalar)
    if scalar.startswith("'") and scalar.endswith("'"):
        return scalar[1:-1].replace("''", "'")
    return scalar


def card(slug, root=ROOT):
    folder = root / 'portfolio' / slug
    source = folder / 'index.qmd'
    metadata = folder / '_metadata.yml'
    parts = re.split(r'^---\s*$', source.read_text(encoding='utf-8-sig'), maxsplit=2, flags=re.M)
    if len(parts) != 3 or parts[0].strip():
        raise ValueError(f'{source}: missing front matter')
    title = value(parts[1], 'title', source)
    image = value(metadata.read_text(encoding='utf-8'), 'image', metadata)
    alt = value(metadata.read_text(encoding='utf-8'), 'image-alt', metadata)
    if Path(image).name != image or not (folder / image).is_file():
        raise ValueError(f'{metadata}: invalid image {image}')
    paragraphs = [block.strip().replace('\n', ' ') for block in re.split(r'\n\s*\n', parts[2])]
    opening = next((block for block in paragraphs if block and not block.startswith(('#', '!', ':::','```'))), '')
    if not opening:
        raise ValueError(f'{source}: missing opening paragraph')
    sentences = re.split(r'(?<=[.!?])\s+(?=[A-ZÀ-Ý])', opening)
    summary = sentences[0]
    if len(sentences) > 1 and len(summary) + len(sentences[1]) + 1 <= 180:
        summary += ' ' + sentences[1]
    url = f'portfolio/{slug}/index.html'
    image_url = f'portfolio/{slug}/{image}'
    return '\n'.join((
        '::: {.about-project-card}',
        f'<a href="{escape(url, quote=True)}" class="about-project-card__media"><img src="{escape(image_url, quote=True)}" alt="{escape(alt, quote=True)}"></a>',
        '',
        f'### [{title}]({url})',
        '',
        summary,
        ':::',
    ))


def main():
    parser = ArgumentParser(description=__doc__)
    parser.add_argument('--check', action='store_true')
    args = parser.parse_args()
    content = '\n\n'.join(card(slug) for slug in SELECTED) + '\n'
    if args.check:
        if not TARGET.exists() or TARGET.read_text(encoding='utf-8') != content:
            raise SystemExit('Home cards are outdated. Run python scripts/build-home.py')
    elif not TARGET.exists() or TARGET.read_text(encoding='utf-8') != content:
        TARGET.write_text(content, encoding='utf-8')
    print(f'Home cards: {len(SELECTED)} portfolio articles.')


if __name__ == '__main__':
    main()
