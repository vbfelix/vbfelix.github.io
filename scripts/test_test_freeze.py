"""The guard that keeps existing test code out of feature commits."""
from pathlib import Path
import importlib.util
import unittest

spec = importlib.util.spec_from_file_location('freeze', Path(__file__).with_name('check-test-freeze.py'))
freeze = importlib.util.module_from_spec(spec)
spec.loader.exec_module(freeze)


class TestCodePaths(unittest.TestCase):
    def test_python_and_typescript_tests_are_test_code(self):
        for path in ('scripts/test_tools.py', 'scripts/test_usability.py', 'scripts/test-content-source.ts',
                     'scripts\\test_design.py'):
            with self.subTest(path=path):
                self.assertTrue(freeze.is_test_code(path))

    def test_sources_checks_and_content_are_not_test_code(self):
        for path in ('scripts/check-site.py', 'scripts/build-writing.py', 'scripts/check-test-freeze.py',
                     'styles.css', 'posts/0001-chi-square-test/index.qmd', 'docs/index.html',
                     'scripts/filters/records.lua', 'scripts/testing.md', 'test_notes.py'):
            with self.subTest(path=path):
                self.assertFalse(freeze.is_test_code(path))


class Violations(unittest.TestCase):
    def test_new_test_files_are_welcome(self):
        self.assertEqual(freeze.violations([('A', ['scripts/test_new_feature.py']), ('M', ['styles.css'])]), [])

    def test_feature_commit_without_tests_passes(self):
        self.assertEqual(freeze.violations([('M', ['styles.css']), ('A', ['scripts/filters/new.lua']),
                                            ('D', ['docs/search.json'])]), [])

    def test_modifying_a_test_is_refused(self):
        self.assertEqual(freeze.violations([('M', ['styles.css']), ('M', ['scripts/test_tools.py'])]),
                         [('M', 'scripts/test_tools.py')])

    def test_modifying_only_tests_is_refused_too(self):
        self.assertEqual(freeze.violations([('M', ['scripts/test_usability.py'])]), [('M', 'scripts/test_usability.py')])

    def test_deleting_a_test_is_refused(self):
        self.assertEqual(freeze.violations([('D', ['scripts/test-content-source.ts'])]),
                         [('D', 'scripts/test-content-source.ts')])

    def test_renaming_a_test_away_is_refused(self):
        self.assertEqual(freeze.violations([('R100', ['scripts/test_tools.py', 'scripts/tools_old.py'])]),
                         [('R', 'scripts/test_tools.py')])

    def test_renaming_a_file_onto_a_test_name_is_refused(self):
        self.assertEqual(freeze.violations([('R087', ['scripts/helpers.py', 'scripts/test_helpers.py'])]),
                         [('R', 'scripts/test_helpers.py')])

    def test_copying_a_test_into_a_new_test_file_is_an_addition(self):
        self.assertEqual(freeze.violations([('C075', ['scripts/test_tools.py', 'scripts/test_more_tools.py'])]), [])

    def test_every_touched_test_is_reported(self):
        found = freeze.violations([('M', ['scripts/test_tools.py']), ('D', ['scripts/test_design.py']),
                                   ('M', ['_quarto.yml'])])
        self.assertEqual(found, [('M', 'scripts/test_tools.py'), ('D', 'scripts/test_design.py')])


if __name__ == '__main__':
    unittest.main()
