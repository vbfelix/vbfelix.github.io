import importlib.util
from pathlib import Path
import unittest

spec = importlib.util.spec_from_file_location('sync_blog', Path(__file__).with_name('sync-blog.py'))
sync = importlib.util.module_from_spec(spec)
spec.loader.exec_module(sync)


class BlogSyncTests(unittest.TestCase):
    def test_render_keeps_only_public_blog_content(self):
        source = ('---\n'
                  'tipo: blog\n'
                  'data: "2026-09-17"\n'
                  'titulo: "Título público"\n'
                  'site:\n'
                  '  categories: [AI, "engenharia de software"]\n'
                  '  lang: pt-BR\n'
                  '---\n\n'
                  '## Brief\n\nInterno.\n\n'
                  '## Blog\n\n### Resumo\n\nTexto público.\n\n'
                  '## Evidências\n\nInterno também.\n').encode('utf-8')
        article = sync.render_article(source).decode('utf-8')
        self.assertIn('title: "Título público"', article)
        self.assertIn('categories: [AI, engenharia de software]', article)
        self.assertIn('Texto público.', article)
        self.assertNotIn('Interno', article)
        self.assertNotIn('### Resumo', article)

    def test_invalid_source_path_is_rejected(self):
        for name in ('../outside.md', '/absolute.md', 'C:/outside.md', 'a\\outside.md'):
            with self.assertRaises(ValueError):
                sync.source_path(name)

    def test_existing_articles_keep_their_public_paths(self):
        incoming = {'2026-09-01_antigo.md': b'', '2026-09-17_novo.md': b''}
        old = {'files': {'2026-09-17_novo.md': {'target': 'posts/0033-novo/index.qmd'}}}
        with self.subTest('existing path'):
            self.assertEqual(sync.allocate_targets(Path('.'), incoming, old)['2026-09-17_novo.md'],
                             'posts/0033-novo/index.qmd')


if __name__ == '__main__':
    unittest.main()
