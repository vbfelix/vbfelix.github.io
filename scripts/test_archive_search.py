"""The blog archive search is a progressive enhancement served from the rendered output."""
from pathlib import Path
import unittest

from test_support import DOCS, REPOSITORY, page

SCRIPT = Path('assets/js/archive-filters.js')


class ArchiveSearch(unittest.TestCase):
    def test_published_filter_script_matches_its_source(self):
        # The archive page loads the script from docs/; a stale copy would ship the old filters.
        source = (REPOSITORY / SCRIPT).read_text(encoding='utf-8').replace('\r\n', '\n')
        published = (DOCS / SCRIPT).read_text(encoding='utf-8').replace('\r\n', '\n')
        self.assertEqual(published, source)

    def test_search_box_is_added_by_the_script_not_the_markup(self):
        # Without JavaScript the list stays a plain reading list, with nothing to type into.
        document = page('writing.html')
        self.assertEqual(document.find('main').find_all('input'), [])
        self.assertEqual(document.find_all(cls='archive-filter-search'), [])


if __name__ == '__main__':
    unittest.main()
