"""Shared helpers for the tests that read the rendered site in docs/.

A small element tree built on html.parser, so the tests need no third-party dependency.
"""
from functools import lru_cache
from html.parser import HTMLParser
from pathlib import Path

REPOSITORY = Path(__file__).resolve().parents[1]
DOCS = REPOSITORY / 'docs'
VOID = {'area', 'base', 'br', 'col', 'embed', 'hr', 'img', 'input', 'link', 'meta', 'source', 'track', 'wbr'}


class Element:
    def __init__(self, tag, attrs, parent):
        self.tag, self.attrs, self.parent = tag, attrs, parent
        self.children, self.own_text = [], []

    @property
    def classes(self):
        return (self.attrs.get('class') or '').split()

    def has_class(self, name):
        return name in self.classes

    def descendants(self):
        for child in self.children:
            yield child
            yield from child.descendants()

    def find_all(self, tag=None, cls=None):
        return [element for element in self.descendants()
                if (tag is None or element.tag == tag) and (cls is None or element.has_class(cls))]

    def find(self, tag=None, cls=None):
        return next(iter(self.find_all(tag, cls)), None)

    def ancestors(self):
        element = self.parent
        while element is not None:
            yield element
            element = element.parent

    @property
    def text(self):
        """Visible text of the element and its descendants, with whitespace collapsed."""
        parts = []

        def collect(element):
            for item in element.own_text:
                if isinstance(item, Element):
                    collect(item)
                else:
                    parts.append(item)
        collect(self)
        return ' '.join(''.join(parts).split())


class Document(HTMLParser):
    def __init__(self, text):
        super().__init__()
        self.root = Element('#document', {}, None)
        self._stack = [self.root]
        self.feed(text)

    def handle_starttag(self, tag, attrs):
        parent = self._stack[-1]
        element = Element(tag, dict(attrs), parent)
        parent.children.append(element)
        parent.own_text.append(element)
        if tag not in VOID:
            self._stack.append(element)

    def handle_startendtag(self, tag, attrs):
        self.handle_starttag(tag, attrs)
        if tag not in VOID:
            self._stack.pop()

    def handle_endtag(self, tag):
        for index in range(len(self._stack) - 1, 0, -1):
            if self._stack[index].tag == tag:
                del self._stack[index:]
                return

    def handle_data(self, data):
        self._stack[-1].own_text.append(data)

    def find_all(self, tag=None, cls=None):
        return self.root.find_all(tag, cls)

    def find(self, tag=None, cls=None):
        return self.root.find(tag, cls)

    def by_id(self, identifier):
        return next((element for element in self.root.descendants() if element.attrs.get('id') == identifier), None)


@lru_cache(maxsize=None)
def page(relative):
    """Parse one rendered page, given its path relative to docs/."""
    return Document((DOCS / relative).read_text(encoding='utf-8'))


def rendered_pages():
    """Every published page except Quarto's libraries and the static pt-br redirect stubs."""
    return sorted(path.relative_to(DOCS).as_posix() for path in DOCS.rglob('*.html')
                  if 'site_libs' not in path.parts and 'pt-br' not in path.parts)


def articles(section):
    """Rendered article pages of one section: 'posts' or 'portfolio'."""
    return [relative for relative in rendered_pages() if relative.startswith(section + '/')]


def accessible_name(element):
    """Name a screen reader announces for a link or button: its label, its text or the alt of an image inside."""
    name = element.attrs.get('aria-label') or element.text or element.attrs.get('title') or ''
    if name.strip():
        return name.strip()
    inner = [image.attrs.get('alt', '') for image in element.find_all('img')]
    inner += [icon.attrs.get('aria-label', '') for icon in element.descendants() if icon.attrs.get('role') == 'img']
    return ' '.join(inner).strip()
