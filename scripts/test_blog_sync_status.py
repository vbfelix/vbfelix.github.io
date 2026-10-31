"""Only an article with status published leaves ghost-writer; drafts never reach the site."""
import importlib.util
import json
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

spec = importlib.util.spec_from_file_location('sync_blog', Path(__file__).with_name('sync-blog.py'))
sync = importlib.util.module_from_spec(spec)
spec.loader.exec_module(sync)

ARTICLE = ('---\n'
           'tipo: blog\n'
           'data: "2026-10-31"\n'
           'status: {status}\n'
           'titulo: "{title}"\n'
           'site:\n'
           '  categories: [Outro]\n'
           '  lang: pt-BR\n'
           '---\n\n'
           '## Blog\n\nTexto.\n')
PUBLISHED = '2026-10-31_publicado'
DRAFT = '2026-10-30_rascunho'


def article(status='published', title='Publicado'):
    return ARTICLE.format(status=status, title=title).encode('utf-8')


class BlogSyncStatusTests(unittest.TestCase):
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
        shutil.rmtree(folder)
        for name, data in files.items():
            path = folder / name
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(data)
        self.run_git('add', '-A')
        self.run_git('commit', '-q', '-m', 'conteudo')
        sync.synchronize(root=self.site, remote=self.source.as_uri())

    def manifest(self):
        return json.loads((self.site / 'blog-source.json').read_text(encoding='utf-8'))['files']

    def site_files(self):
        return sorted(path.relative_to(self.site).as_posix()
                      for path in (self.site / 'posts').rglob('*') if path.is_file())

    def test_draft_is_not_imported(self):
        self.publish({f'{DRAFT}/post.md': article(status='draft', title='Rascunho')})
        self.assertEqual(self.manifest(), {})
        self.assertEqual(self.site_files(), [])

    def test_only_the_published_article_crosses(self):
        self.publish({f'{PUBLISHED}/post.md': article(),
                      f'{DRAFT}/post.md': article(status='draft', title='Rascunho')})
        self.assertEqual(list(self.manifest()), [f'{PUBLISHED}.md'])
        self.assertEqual(self.site_files(), ['posts/0032-publicado/index.qmd'])

    def test_draft_does_not_reserve_an_index(self):
        self.publish({f'{DRAFT}/post.md': article(status='draft', title='Rascunho'),
                      f'{PUBLISHED}/post.md': article()})
        self.assertEqual(self.manifest()[f'{PUBLISHED}.md']['target'], 'posts/0032-publicado/index.qmd')

    def test_article_back_to_draft_leaves_the_site(self):
        self.publish({f'{PUBLISHED}/post.md': article()})
        self.assertEqual(self.site_files(), ['posts/0032-publicado/index.qmd'])
        self.publish({f'{PUBLISHED}/post.md': article(status='draft')})
        self.assertEqual(self.manifest(), {})
        self.assertEqual(self.site_files(), [])

    def test_draft_with_two_sources_is_still_rejected(self):
        with self.assertRaisesRegex(ValueError, 'two sources'):
            self.publish({f'{DRAFT}.md': article(status='draft', title='Rascunho'),
                          f'{DRAFT}/post.md': article(status='draft', title='Rascunho')})

    def test_article_without_status_is_imported(self):
        headless = article().decode('utf-8').replace('status: published\n', '').encode('utf-8')
        self.publish({f'{PUBLISHED}/post.md': headless})
        self.assertEqual(list(self.manifest()), [f'{PUBLISHED}.md'])


if __name__ == '__main__':
    unittest.main()
