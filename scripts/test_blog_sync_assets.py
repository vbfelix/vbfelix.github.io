import importlib.util
import json
from pathlib import Path
import subprocess
import tempfile
import unittest

spec = importlib.util.spec_from_file_location('sync_blog', Path(__file__).with_name('sync-blog.py'))
sync = importlib.util.module_from_spec(spec)
spec.loader.exec_module(sync)

ARTICLE = ('---\n'
           'tipo: blog\n'
           'data: "2026-10-03"\n'
           'titulo: "Com figura"\n'
           'site:\n'
           '  categories: [Estatística]\n'
           '  lang: pt-BR\n'
           '---\n\n'
           '## Brief\n\nInterno.\n\n'
           '## Blog\n\nTexto.\n\n{body}\n')
NAME = '2026-10-03_com-figura.md'
CITED = '![Legenda (a) e (b).](figura-2.png)\n\n![Externa](https://example.com/x.png)'


class BlogSyncAssetTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.source = Path(self.temp.name) / 'source'
        self.site = Path(self.temp.name) / 'site'
        (self.source / 'conteudo/blog').mkdir(parents=True)
        (self.site / 'posts').mkdir(parents=True)
        self.run_git('init', '-q', '-b', 'main')

    def run_git(self, *args):
        subprocess.run(['git', '-c', 'user.name=test', '-c', 'user.email=test@example.com', *args],
                       cwd=self.source, check=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    def publish(self, files):
        folder = self.source / 'conteudo/blog'
        for path in folder.iterdir():
            path.unlink()
        for name, data in files.items():
            (folder / name).write_bytes(data)
        self.run_git('add', '-A')
        self.run_git('commit', '-q', '-m', 'conteudo')
        sync.synchronize(root=self.site, remote=self.source.as_uri())

    def article(self, body=CITED):
        return ARTICLE.format(body=body).encode('utf-8')

    def test_cited_image_lands_beside_the_article(self):
        self.publish({NAME: self.article(), 'figura-2.png': b'cited', 'sobra.jpg': b'not cited'})
        folder = self.site / 'posts/0032-com-figura'
        self.assertEqual((folder / 'figura-2.png').read_bytes(), b'cited')
        self.assertFalse((folder / 'sobra.jpg').exists())
        record = json.loads((self.site / 'blog-source.json').read_text(encoding='utf-8'))['files'][NAME]
        self.assertEqual(record['assets'], {'figura-2.png': sync.digest(b'cited')})
        sync.synchronize(root=self.site, check=True)

    def test_article_without_image_keeps_the_manifest_shape(self):
        self.publish({NAME: self.article(body='Sem figura.')})
        record = json.loads((self.site / 'blog-source.json').read_text(encoding='utf-8'))['files'][NAME]
        self.assertEqual(sorted(record), ['rendered', 'source', 'target'])

    def test_cited_image_absent_from_source_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'figura-2.png'):
            self.publish({NAME: self.article()})

    def test_local_edit_to_imported_image_is_detected(self):
        self.publish({NAME: self.article(), 'figura-2.png': b'cited'})
        (self.site / 'posts/0032-com-figura/figura-2.png').write_bytes(b'edited')
        with self.assertRaises(ValueError):
            sync.synchronize(root=self.site, check=True)

    def test_image_no_longer_cited_is_removed(self):
        self.publish({NAME: self.article(), 'figura-2.png': b'cited'})
        built = self.site / 'docs/posts/0032-com-figura/figura-2.png'
        built.parent.mkdir(parents=True)
        built.write_bytes(b'cited')
        self.publish({NAME: self.article(body='Sem figura.'), 'figura-2.png': b'cited'})
        self.assertFalse((self.site / 'posts/0032-com-figura/figura-2.png').exists())
        self.assertFalse(built.exists())
        self.assertTrue((self.site / 'posts/0032-com-figura/index.qmd').is_file())

    def test_other_files_in_the_source_folder_are_still_rejected(self):
        with self.assertRaisesRegex(ValueError, 'Unsupported source entry'):
            self.publish({NAME: self.article(body='Sem figura.'), 'notas.txt': b'x'})


if __name__ == '__main__':
    unittest.main()
