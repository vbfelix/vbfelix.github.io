"""ghost-writer keeps one folder per article: post.md, its images and, when planned, plano.md."""
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
           'data: "2026-10-03"\n'
           'status: published\n'
           'titulo: "{title}"\n'
           'site:\n'
           '  categories: [Estatística]\n'
           '  lang: pt-BR\n'
           '---\n\n'
           '## Blog\n\nTexto.\n\n{body}\n')
PLAN = ('---\n'
        'tipo: blog\n'
        'data: "2026-10-03"\n'
        'status: aprovado\n'
        'titulo: "Com figura"\n'
        '---\n\n'
        '## Contexto\n\nInterno.\n').encode('utf-8')
FOLDER = '2026-10-03_com-figura'
KEY = FOLDER + '.md'
OTHER = '2026-10-04_outro'
CITED = '![Legenda.](figura-2.png)'


def article(title='Com figura', body=CITED):
    return ARTICLE.format(title=title, body=body).encode('utf-8')


class BlogSyncFolderTests(unittest.TestCase):
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
        return {path.relative_to(self.site).as_posix(): path.read_bytes()
                for path in (self.site / 'posts').rglob('*') if path.is_file()}

    def test_article_moved_into_its_folder_keeps_key_path_and_bytes(self):
        self.publish({KEY: article(), 'figura-2.png': b'cited'})
        before_files, before_manifest = self.site_files(), self.manifest()
        self.publish({f'{FOLDER}/post.md': article(), f'{FOLDER}/figura-2.png': b'cited', f'{FOLDER}/plano.md': PLAN})
        self.assertEqual(self.site_files(), before_files)
        self.assertEqual(self.manifest(), before_manifest)
        self.assertEqual(before_manifest[KEY]['target'], 'posts/0032-com-figura/index.qmd')
        sync.synchronize(root=self.site, check=True)

    def test_migration_and_new_article_arrive_together(self):
        self.publish({KEY: article(), 'figura-2.png': b'cited'})
        before_files, before_record = self.site_files(), self.manifest()[KEY]
        new = '2026-10-13_subq-snap'
        self.publish({f'{FOLDER}/post.md': article(), f'{FOLDER}/figura-2.png': b'cited',
                      f'{new}/post.md': article(title='Novo', body='Sem figura.'), f'{new}/plano.md': PLAN})
        self.assertEqual(self.manifest()[KEY], before_record)
        self.assertEqual(self.manifest()[f'{new}.md']['target'], 'posts/0033-subq-snap/index.qmd')
        after_files = self.site_files()
        self.assertEqual(sorted(set(after_files) - set(before_files)), ['posts/0033-subq-snap/index.qmd'])
        self.assertEqual({name: after_files[name] for name in before_files}, before_files)

    def test_plan_is_never_published(self):
        self.publish({f'{FOLDER}/post.md': article(body='Sem figura.'), f'{FOLDER}/plano.md': PLAN})
        self.assertEqual(list(self.manifest()), [KEY])
        self.assertEqual(list(self.site_files()), ['posts/0032-com-figura/index.qmd'])

    def test_folder_with_only_a_plan_publishes_nothing(self):
        self.publish({f'{FOLDER}/plano.md': PLAN})
        self.assertEqual(self.manifest(), {})
        self.assertEqual(self.site_files(), {})

    def test_new_article_takes_its_slug_from_the_folder(self):
        self.publish({'2026-10-13_subq-snap/post.md': article(title='Novo', body='Sem figura.')})
        self.assertEqual(self.manifest()['2026-10-13_subq-snap.md']['target'], 'posts/0032-subq-snap/index.qmd')

    def test_name_without_the_date_prefix_is_rejected(self):
        for name in ('teste/post.md', 'post.md', 'plano.md'):
            with self.subTest(name=name), self.assertRaisesRegex(ValueError, 'Unsupported source entry'):
                self.publish({name: article(body='Sem figura.')})

    def test_image_is_resolved_in_the_folder_of_the_article(self):
        self.publish({f'{FOLDER}/post.md': article(), f'{FOLDER}/figura-2.png': b'first',
                      f'{OTHER}/post.md': article(title='Outro'), f'{OTHER}/figura-2.png': b'second'})
        self.assertEqual((self.site / 'posts/0032-com-figura/figura-2.png').read_bytes(), b'first')
        self.assertEqual((self.site / 'posts/0033-outro/figura-2.png').read_bytes(), b'second')

    def test_image_outside_the_article_folder_does_not_satisfy_the_citation(self):
        missing = f'Image missing from main: figura-2.png \\(cited by {FOLDER}/post.md\\)'
        for elsewhere in ('figura-2.png', f'{OTHER}/figura-2.png'):
            with self.subTest(elsewhere=elsewhere), self.assertRaisesRegex(ValueError, missing):
                self.publish({f'{FOLDER}/post.md': article(), elsewhere: b'cited'})

    def test_loose_article_does_not_take_an_image_from_a_folder(self):
        with self.assertRaisesRegex(ValueError, 'Image missing from main: figura-2.png'):
            self.publish({KEY: article(), f'{OTHER}/figura-2.png': b'cited'})

    def test_other_markdown_in_the_article_folder_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'Unsupported source entry'):
            self.publish({f'{FOLDER}/post.md': article(body='Sem figura.'), f'{FOLDER}/notas.md': b'x'})

    def test_article_as_loose_file_and_as_folder_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'two sources'):
            self.publish({KEY: article(body='Sem figura.'), f'{FOLDER}/post.md': article(body='Sem figura.')})


if __name__ == '__main__':
    unittest.main()
