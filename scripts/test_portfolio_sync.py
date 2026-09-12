import importlib.util
from pathlib import Path
import subprocess
import tempfile
import unittest

spec = importlib.util.spec_from_file_location('sync', Path(__file__).with_name('sync-portfolio.py'))
sync = importlib.util.module_from_spec(spec)
spec.loader.exec_module(sync)


class PortfolioSyncTests(unittest.TestCase):
    def test_remote_main_updates_preserves_assets_and_rejects_local_edits(self):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            remote, site = base / 'remote', base / 'site'
            remote.mkdir()
            site.mkdir()

            def git(*args):
                subprocess.run(['git', '-C', str(remote), *args], check=True, capture_output=True)

            git('init', '-b', 'main')
            git('config', 'user.name', 'Test')
            git('config', 'user.email', 'test@example.invalid')
            article = remote / 'posts/portfolio/test/index.qmd'
            article.parent.mkdir(parents=True)
            article.write_text('Texto com acentuação.', encoding='utf-8')
            article.with_name('thumbnail.svg').write_text('<svg>upstream image</svg>')
            article.with_name('_metadata.yml').write_text('image: upstream.png')
            git('add', '.')
            git('commit', '-m', 'first')
            git('checkout', '-b', 'draft')
            article.write_text('Draft, never import', encoding='utf-8')
            local = site / 'portfolio/test/index.qmd'
            local.parent.mkdir(parents=True)
            asset = local.with_name('thumbnail.svg')
            asset.write_text('<svg/>')
            sync.synchronize(site, str(remote))
            self.assertEqual(local.read_text(encoding='utf-8'), 'Texto com acentuação.')
            self.assertEqual(asset.read_text(), '<svg/>')
            self.assertFalse(local.with_name('_metadata.yml').exists())
            sync.synchronize(site, str(remote), check=True)
            git('checkout', '--', '.')
            git('checkout', 'main')
            article.write_text('Updated main', encoding='utf-8')
            git('add', '.')
            git('commit', '-m', 'update')
            sync.synchronize(site, str(remote))
            self.assertEqual(local.read_text(), 'Updated main')
            self.assertEqual(asset.read_text(), '<svg/>')
            local.write_text('Local user edit')
            with self.assertRaises(ValueError):
                sync.synchronize(site, str(remote))
            self.assertEqual(local.read_text(), 'Local user edit')
            with self.assertRaises(subprocess.CalledProcessError):
                sync.synchronize(site, str(base / 'missing'))
            self.assertEqual(local.read_text(), 'Local user edit')

    def test_paths_cannot_escape(self):
        for name in ('../outside', '/absolute', 'C:/outside', 'a\\outside'):
            with self.assertRaises(ValueError):
                sync.target(Path.cwd(), name)

    def test_empty_source_adopts_existing_article_then_removes_only_managed_files(self):
        with tempfile.TemporaryDirectory() as tmp:
            base = Path(tmp)
            remote, site = base / 'remote', base / 'site'
            remote.mkdir()
            site.mkdir()
            def git(*args):
                subprocess.run(['git', '-C', str(remote), *args], check=True, capture_output=True)
            git('init', '-b', 'main')
            git('config', 'user.name', 'Test')
            git('config', 'user.email', 'test@example.invalid')
            upstream = remote / 'posts/portfolio/test/index.qmd'
            upstream.parent.mkdir(parents=True)
            (upstream.parent.parent / '.gitkeep').touch()
            git('add', '.')
            git('commit', '-m', 'empty catalog')
            local = site / 'portfolio/test/index.qmd'
            local.parent.mkdir(parents=True)
            local.write_text('Legacy article')
            asset = local.with_name('thumbnail.svg')
            asset.write_text('<svg/>')
            sync.synchronize(site, str(remote))
            self.assertEqual(local.read_text(), 'Legacy article')
            upstream.write_text('Canonical article')
            git('add', '.')
            git('commit', '-m', 'publish')
            sync.synchronize(site, str(remote))
            self.assertEqual(local.read_text(), 'Canonical article')
            output = site / 'docs/portfolio/test/index.html'
            output.parent.mkdir(parents=True)
            output.write_text('Rendered article')
            upstream.unlink()
            git('add', '.')
            git('commit', '-m', 'remove')
            sync.synchronize(site, str(remote))
            self.assertFalse(local.exists())
            self.assertFalse(output.exists())
            self.assertTrue(asset.exists())


if __name__ == '__main__':
    unittest.main()
