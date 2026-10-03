"""The Lua filter that turns course and certification paragraphs into rows, run through Pandoc itself."""
from pathlib import Path
import os
import shutil
import subprocess
import tempfile
import unittest

from test_support import Document, REPOSITORY

FILTER = REPOSITORY / 'scripts' / 'filters' / 'records.lua'


def quarto():
    fallback = Path(os.environ.get('ProgramFiles', '')) / 'RStudio/resources/app/bin/quarto/bin/quarto.exe'
    return shutil.which('quarto') or (str(fallback) if fallback.is_file() else None)


def convert(markdown, target='html'):
    # `quarto pandoc` does not hand the document back on stdout, so it is written to a file.
    with tempfile.TemporaryDirectory() as directory:
        output = Path(directory) / 'output'
        subprocess.run([quarto(), 'pandoc', '--from', 'markdown', '--to', target, '--lua-filter', str(FILTER),
                        '--output', str(output)],
                       input=markdown, capture_output=True, text=True, encoding='utf-8', check=True)
        return output.read_text(encoding='utf-8')


RECORD = '![{alt}](/images/logos/{logo}){{width="32"}} \\[{date}\\] {title}\n\n'


@unittest.skipUnless(quarto(), 'Quarto is not installed')
class RecordsFilter(unittest.TestCase):
    def html(self, markdown):
        return Document(convert(markdown))

    def test_consecutive_records_become_one_list(self):
        document = self.html(RECORD.format(alt='AWS', logo='aws.svg', date='09/26', title='First course') +
                             RECORD.format(alt='AWS', logo='aws.svg', date='08/26', title='Second course'))
        lists = document.find_all('div', 'record-list')
        self.assertEqual(len(lists), 1)
        rows = lists[0].find('ul').children
        self.assertEqual([row.find('span', 'record-title').text for row in rows], ['First course', 'Second course'])
        self.assertEqual(document.find_all('p'), [])

    def test_row_keeps_logo_date_and_title_in_that_order(self):
        row = self.html(RECORD.format(alt='AWS', logo='aws.svg', date='09/26', title='A course')).find('li')
        self.assertEqual([child.tag for child in row.children], ['img', 'span', 'span'])
        self.assertEqual(row.find('img').attrs['alt'], 'AWS')
        self.assertEqual(row.find('img').attrs['width'], '32')

    def test_date_loses_its_brackets(self):
        row = self.html(RECORD.format(alt='AWS', logo='aws.svg', date='09/26', title='A course')).find('li')
        self.assertEqual(row.find('span', 'record-date').text, '09/26')
        self.assertNotIn('[', row.text)

    def test_title_keeps_its_link_and_formatting(self):
        row = self.html(RECORD.format(alt='X', logo='x.svg', date='01/19',
                                      title='[Data *Analyst* with R](https://example.com/c)')).find('li')
        title = row.find('span', 'record-title')
        self.assertEqual(title.find('a').attrs['href'], 'https://example.com/c')
        self.assertEqual(title.find('em').text, 'Analyst')
        self.assertEqual(title.text, 'Data Analyst with R')

    def test_headings_separate_the_lists(self):
        document = self.html('## 2026\n\n' + RECORD.format(alt='A', logo='a.svg', date='09/26', title='One') +
                             '## 2025\n\n' + RECORD.format(alt='A', logo='a.svg', date='03/25', title='Two'))
        self.assertEqual(len(document.find_all('div', 'record-list')), 2)
        self.assertEqual([heading.text for heading in document.find_all('h2')], ['2026', '2025'])

    def test_sub_items_stay_inside_their_record(self):
        document = self.html(RECORD.format(alt='H', logo='h.png', date='05/20', title='Problem solving') +
                             '-   [Basic](https://example.com/basic)\n-   Intermediate\n\n' +
                             RECORD.format(alt='H', logo='h.png', date='04/20', title='SQL'))
        lists = document.find_all('div', 'record-list')
        self.assertEqual(len(lists), 1)
        rows = lists[0].find('ul').children
        self.assertEqual(len(rows), 2)
        self.assertEqual([item.text for item in rows[0].find('ul').children], ['Basic', 'Intermediate'])
        self.assertIsNone(rows[1].find('ul'))

    def test_a_list_that_follows_no_record_is_left_alone(self):
        document = self.html('-   one\n-   two\n')
        self.assertEqual(document.find_all('div', 'record-list'), [])
        self.assertEqual(len(document.find_all('li')), 2)

    def test_ordinary_paragraphs_are_left_alone(self):
        cases = ['Plain text with \\[09/26\\] inside.\n',
                 '![Chart](chart.png) A figure followed by a caption.\n',
                 '![Logo](logo.png) \\[2026\\] A year is not a month and year.\n',
                 '![Logo](logo.png) \\[09/26\\]\n']
        for markdown in cases:
            with self.subTest(markdown=markdown):
                document = self.html(markdown)
                self.assertEqual(document.find_all('div', 'record-list'), [])
                self.assertEqual(len(document.find_all('p')), 1)

    def test_a_paragraph_between_records_splits_the_list(self):
        document = self.html(RECORD.format(alt='A', logo='a.svg', date='09/26', title='One') + 'A note.\n\n' +
                             RECORD.format(alt='A', logo='a.svg', date='08/26', title='Two'))
        self.assertEqual(len(document.find_all('div', 'record-list')), 2)
        self.assertEqual([paragraph.text for paragraph in document.find_all('p')], ['A note.'])

    def test_other_formats_keep_the_source_shape(self):
        markdown = RECORD.format(alt='AWS', logo='aws.svg', date='09/26', title='A course')
        plain = convert(markdown, 'plain')
        self.assertIn('[09/26] A course', plain)
        self.assertNotIn('record', convert(markdown, 'markdown'))


if __name__ == '__main__':
    unittest.main()
