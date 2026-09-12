"""Regression checks for archive metadata and side-effect-free imports."""
import importlib.util
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location('archive', Path(__file__).with_name('build-writing.py'))
archive = importlib.util.module_from_spec(spec)
spec.loader.exec_module(archive)


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


if __name__ == '__main__':
    unittest.main()
