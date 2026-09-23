"""Regression checks for archive metadata and side-effect-free imports."""
import importlib.util
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location('archive', Path(__file__).with_name('build-writing.py'))
archive = importlib.util.module_from_spec(spec)
spec.loader.exec_module(archive)
home_spec = importlib.util.spec_from_file_location('home', Path(__file__).with_name('build-home.py'))
home = importlib.util.module_from_spec(home_spec)
home_spec.loader.exec_module(home)
site_spec = importlib.util.spec_from_file_location('site', Path(__file__).with_name('check-site.py'))
site = importlib.util.module_from_spec(site_spec)
site_spec.loader.exec_module(site)


class ArchiveTests(unittest.TestCase):
    def read(self, metadata):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / 'index.qmd'
            path.write_text('---\n' + metadata + '\n---\nBody\n', encoding='utf-8')
            return archive.read_post(path)

    def test_delimiter_inside_title_and_escaped_html(self):
        post = self.read('title: "A --- B & <C>"\ndate: "2025-01-02"\ncategories: [math, statistics]')
        self.assertEqual(post[:3], ('2025-01-02', 'A --- B & <C>', 'math, statistics'))
        self.assertIn('A --- B &amp; &lt;C&gt;', archive.render([post]))

    def test_invalid_date_fails(self):
        with self.assertRaisesRegex(ValueError, 'invalid metadata'):
            self.read('title: Example\ndate: 2025-02-31')

    def test_multiline_title_fails_instead_of_silent_corruption(self):
        with self.assertRaises(ValueError):
            self.read('title: |\n  Example\ndate: 2025-01-01')

    def test_single_quoted_title(self):
        self.assertEqual(archive.scalar("'Benford''s Law'"), "Benford's Law")

    def test_language_label_and_filter_metadata_use_post_metadata(self):
        brazilian = self.read('title: Exemplo\ndate: 2025-01-01\nlang: pt-BR')
        american = self.read('title: Example\ndate: 2025-01-02\nlang: en')
        rendered = archive.render([brazilian, american])
        self.assertIn('data-language="pt" data-categories=""', rendered)
        self.assertIn('aria-label="Em português">PT', rendered)
        self.assertIn('data-language="en" data-categories=""', rendered)
        self.assertIn('aria-label="Em inglês">EN', rendered)

    def test_unsupported_language_fails(self):
        with self.assertRaisesRegex(ValueError, 'unsupported language'):
            self.read('title: Exemple\ndate: 2025-01-01\nlang: fr')

    def test_missing_language_defaults_to_english_label(self):
        post = self.read('title: Example\ndate: 2025-01-01')
        self.assertIn('aria-label="Em inglês">EN', archive.render([post]))

    def test_recent_writing_limits_entries_and_uses_child_headings(self):
        posts = [
            ('2025-01-01', 'Older', '', 'older', ('EN', 'Em inglês', 'en')),
            ('2025-01-02', 'Newer', '', 'newer', ('EN', 'Em inglês', 'en')),
        ]
        rendered = archive.render(posts, limit=1, heading=3)
        self.assertIn('<h3>', rendered)
        self.assertIn('Newer', rendered)
        self.assertNotIn('Older', rendered)


class HomeCardTests(unittest.TestCase):
    def test_card_tracks_article_title_opening_and_local_artwork(self):
        with tempfile.TemporaryDirectory() as directory:
            folder = Path(directory) / 'portfolio' / 'case'
            folder.mkdir(parents=True)
            (folder / 'index.qmd').write_text('---\ntitle: Primeiro título\n---\n\nPrimeira frase. Segunda frase.\n', encoding='utf-8')
            (folder / '_metadata.yml').write_text('image: thumbnail.svg\nimage-alt: "Diagrama do caso"\n', encoding='utf-8')
            (folder / 'thumbnail.svg').write_text('<svg/>', encoding='utf-8')
            first = home.card('case', Path(directory))
            self.assertIn('### [Primeiro título](portfolio/case/index.html)', first)
            self.assertIn('Primeira frase. Segunda frase.', first)
            self.assertIn('<img src="portfolio/case/thumbnail.svg" alt="Diagrama do caso">', first)
            (folder / 'index.qmd').write_text('---\ntitle: Título revisado\n---\n\nAbertura revisada.\n', encoding='utf-8')
            revised = home.card('case', Path(directory))
            self.assertIn('### [Título revisado](portfolio/case/index.html)', revised)
            self.assertIn('Abertura revisada.', revised)
            self.assertNotIn('Primeiro título', revised)


class SelectedCardOutputTests(unittest.TestCase):
    def test_source_and_rendered_cards_share_the_same_order(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / 'home-selected.qmd'
            source.write_text(
                '<a href="portfolio/first/index.html" class="about-project-card__media"></a>\n'
                '<a href="portfolio/second/index.html" class="about-project-card__media"></a>\n',
                encoding='utf-8',
            )
            page = root / 'index.html'
            page.write_text(
                '<a class="about-project-card__media" href="portfolio/first/index.html"></a>\n'
                '<a class="about-project-card__media" href="portfolio/second/index.html"></a>\n',
                encoding='utf-8',
            )

            expected = site.selected_cards(source)
            rendered = site.Page(page).project_cards

            self.assertEqual(rendered, expected)

    def test_source_without_cards_fails(self):
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory) / 'home-selected.qmd'
            source.write_text('Nenhum trabalho selecionado.\n', encoding='utf-8')

            with self.assertRaisesRegex(ValueError, 'no selected portfolio cards'):
                site.selected_cards(source)

    def test_different_rendered_order_reports_the_page_and_source(self):
        error = site.selected_card_error(
            'header-about.html',
            ['portfolio/first/index.html', 'portfolio/second/index.html'],
            ['portfolio/second/index.html', 'portfolio/first/index.html'],
        )

        self.assertIn('header-about.html', error)
        self.assertIn('_content/home-selected.qmd', error)


if __name__ == '__main__':
    unittest.main()
