"""Usability and accessibility of the rendered site.

These tests read the pages in docs/, the same files GitHub Pages serves, so they describe what a
visitor gets. Run `scripts/site.ps1 -Action render` first when sources changed.
"""
from collections import Counter
from datetime import date
from pathlib import PurePosixPath
from urllib.parse import parse_qs, urlsplit
import re
import unittest

from test_support import DOCS, REPOSITORY, accessible_name, articles, page, rendered_pages

PAGES = rendered_pages()
HOME = 'index.html'
# The tests assert structure and destinations, not wording, so that copy can be edited freely.


def destination(relative, href):
    """File in docs/ that a local href points to, from the page that holds it."""
    path = urlsplit(href).path
    if path.startswith('/'):
        target = PurePosixPath(path.lstrip('/'))
    else:
        parts = []
        for part in (PurePosixPath(relative).parent / path).parts:
            if part == '..':
                parts.pop()
            elif part != '.':
                parts.append(part)
        target = PurePosixPath(*parts) if parts else PurePosixPath('index.html')
    return target.as_posix()


class EveryPage(unittest.TestCase):
    def test_site_has_the_expected_number_of_pages(self):
        sources = [*REPOSITORY.glob('*.qmd'), *REPOSITORY.glob('posts/*/index.qmd'), *REPOSITORY.glob('portfolio/*/index.qmd')]
        self.assertEqual(len(PAGES), len(sources))

    def test_declares_a_language(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                self.assertRegex(document.find('html').attrs.get('lang', ''), r'^(pt|pt-BR|en|en-US)$')

    def test_has_a_title_that_names_the_site(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                self.assertIn(document.find('a', 'navbar-brand').text, document.find('title').text)

    def test_titles_are_unique(self):
        titles = Counter(page(relative).find('title').text for relative in PAGES if relative != 'header-about.html')
        self.assertEqual([title for title, count in titles.items() if count > 1], [])

    def test_scales_on_phones(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                viewport = [meta for meta in document.find_all('meta') if meta.attrs.get('name') == 'viewport']
                self.assertEqual(len(viewport), 1)
                self.assertIn('width=device-width', viewport[0].attrs['content'])
                self.assertNotIn('user-scalable=no', viewport[0].attrs['content'])
                self.assertNotIn('maximum-scale', viewport[0].attrs['content'])

    def test_has_one_main_landmark_a_header_and_a_footer(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                self.assertEqual(len(document.find_all('main')), 1)
                self.assertIsNotNone(document.by_id('quarto-header'))
                self.assertEqual(len(document.find_all('footer')), 1)

    def test_skip_link_leads_to_the_content(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                links = document.find_all('a', 'skip-link')
                self.assertEqual(len(links), 1)
                self.assertTrue(links[0].text)
                target = links[0].attrs['href'].lstrip('#')
                self.assertEqual(document.by_id(target).tag, 'main')

    def test_has_no_search_box(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                self.assertIsNone(document.by_id('quarto-search'))
                self.assertEqual(document.find_all('input', 'aa-Input'), [])

    def test_ids_are_unique(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                ids = Counter(element.attrs['id'] for element in document.root.descendants() if element.attrs.get('id'))
                self.assertEqual([identifier for identifier, count in ids.items() if count > 1], [])

    def test_every_link_has_a_name(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                unnamed = [link.attrs.get('href') for link in document.find_all('a')
                           if link.attrs.get('aria-hidden') != 'true' and not accessible_name(link)]
                if relative.startswith('posts/'):
                    # Two legacy posts carry an empty heading, which leaves an empty entry in their own contents list.
                    unnamed = [href for href in unnamed if not href.startswith('#')]
                self.assertEqual(unnamed, [])

    def test_every_button_has_a_name(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                unnamed = [button.attrs for button in document.find_all('button') if not accessible_name(button)]
                self.assertEqual(unnamed, [])

    def test_links_do_not_use_vague_text(self):
        vague = {'clique aqui', 'aqui', 'click here', 'here', 'saiba mais', 'leia mais', 'link'}
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                found = [link.text for link in document.find_all('a') if link.text.lower() in vague]
                self.assertEqual(found, [])

    def test_links_that_open_a_new_tab_are_isolated(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                unsafe = [link.attrs['href'] for link in document.find_all('a')
                          if link.attrs.get('target') == '_blank' and 'noopener' not in (link.attrs.get('rel') or '')]
                self.assertEqual(unsafe, [])

    def test_embedded_frames_are_titled(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                untitled = [frame.attrs.get('src') for frame in document.find_all('iframe') if not frame.attrs.get('title')]
                self.assertEqual(untitled, [])

    def test_does_not_trap_or_reorder_keyboard_focus(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                positive = [element.tag for element in document.root.descendants()
                            if element.attrs.get('tabindex', '0').lstrip('-').isdigit() and int(element.attrs.get('tabindex', '0')) > 0]
                self.assertEqual(positive, [])

    def test_does_not_autoplay_media_or_refresh_itself(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                self.assertEqual([media.tag for media in document.find_all('video') + document.find_all('audio')
                                  if 'autoplay' in media.attrs], [])
                self.assertEqual([meta for meta in document.find_all('meta') if meta.attrs.get('http-equiv', '').lower() == 'refresh'], [])

    def test_local_images_exist_and_stay_within_the_site(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                document = page(relative)
                for image in document.find_all('img'):
                    source = image.attrs.get('src', '')
                    if urlsplit(source).scheme:
                        continue
                    self.assertTrue((DOCS / destination(relative, source)).is_file(), source)


class ImagesOutsideLegacyPosts(unittest.TestCase):
    """Posts keep figures generated years ago without descriptions; every other page must describe its images."""

    def test_every_image_has_an_alt_attribute(self):
        for relative in PAGES:
            if relative.startswith('posts/'):
                continue
            with self.subTest(page=relative):
                missing = [image.attrs.get('src') for image in page(relative).find_all('img') if 'alt' not in image.attrs]
                self.assertEqual(missing, [])

    def test_alt_text_is_not_a_file_name(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                names = [image.attrs['alt'] for image in page(relative).find_all('img')
                         if re.search(r'\.(png|jpe?g|svg|gif|webp)$', image.attrs.get('alt', ''), re.I)]
                self.assertEqual(names, [])


class Navigation(unittest.TestCase):
    def menu(self, relative):
        """Menu entries as (text, destination), so pages at different depths compare equal."""
        navbar = page(relative).by_id('navbarCollapse')
        return [(accessible_name(link), self.target(relative, link)) for link in navbar.find_all('a')]

    def footer(self, relative):
        return [(link.text, self.target(relative, link)) for link in page(relative).find('footer').find_all('a')]

    @staticmethod
    def target(relative, link):
        href = link.attrs.get('href', '')
        return href if href in ('', '#') or urlsplit(href).scheme else destination(relative, href)

    def test_menu_is_the_same_on_every_page(self):
        expected = self.menu(HOME)
        self.assertGreaterEqual(len(expected), 5)
        for relative in PAGES:
            with self.subTest(page=relative):
                self.assertEqual(self.menu(relative), expected)

    def test_footer_is_the_same_on_every_page(self):
        expected = self.footer(HOME)
        for relative in PAGES:
            with self.subTest(page=relative):
                self.assertEqual(self.footer(relative), expected)

    def test_menu_reaches_the_main_sections(self):
        targets = {target for _, target in self.menu(HOME)}
        self.assertLessEqual({'index.html', 'portfolio.html', 'writing.html', 'header-certifications.html',
                              'header-participations.html', 'header-courses.html', 'header-publications.html',
                              'header-awards.html'}, targets)

    def test_footer_reaches_the_machine_formats(self):
        targets = {target for _, target in self.footer(HOME)}
        self.assertLessEqual({'agents.html', 'curriculo.md', 'curriculo.json'}, targets)

    def test_every_published_section_page_is_reachable_from_the_home_page(self):
        # Through the menu, the footer or a link in the home content.
        reachable = {target for _, target in self.menu(HOME) + self.footer(HOME)}
        reachable |= {destination(HOME, link.attrs['href']) for link in page(HOME).find('main').find_all('a')
                      if link.attrs.get('href') and not urlsplit(link.attrs['href']).scheme}
        orphans = [relative for relative in PAGES
                   if '/' not in relative and relative not in reachable and relative != 'header-about.html']
        self.assertEqual(orphans, [])

    def test_menu_icons_are_named_for_screen_readers(self):
        navbar = page(HOME).by_id('navbarCollapse')
        icons = [link for link in navbar.find_all('a', 'nav-link') if not link.text]
        self.assertGreater(len(icons), 0)
        for link in icons:
            self.assertTrue(link.find('i').attrs.get('aria-label'), link.attrs.get('href'))

    def test_menu_collapses_behind_a_labelled_button(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                toggler = page(relative).find('button', 'navbar-toggler')
                self.assertTrue(toggler.attrs.get('aria-label'))
                self.assertEqual(toggler.attrs.get('aria-controls'), 'navbarCollapse')
                self.assertEqual(toggler.attrs.get('aria-expanded'), 'false')

    def test_submenu_announces_its_state(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                toggle = page(relative).find('a', 'dropdown-toggle')
                self.assertEqual(toggle.attrs.get('aria-expanded'), 'false')
                self.assertGreater(len(toggle.parent.find_all('a', 'dropdown-item')), 0)

    def test_brand_leads_home(self):
        for relative in PAGES:
            with self.subTest(page=relative):
                brand = page(relative).find('a', 'navbar-brand')
                self.assertTrue(brand.text)
                self.assertEqual(destination(relative, brand.attrs['href']), 'index.html')

    def test_menu_marks_the_current_section(self):
        for relative in ('index.html', 'portfolio.html', 'writing.html', 'header-certifications.html'):
            with self.subTest(page=relative):
                navbar = page(relative).by_id('navbarCollapse')
                active = [link for link in navbar.find_all('a', 'nav-link') if link.has_class('active')]
                self.assertEqual([destination(relative, link.attrs['href']) for link in active], [relative])
                self.assertEqual(active[0].attrs.get('aria-current'), 'page')

    def test_menu_and_footer_destinations_exist(self):
        for _, target in self.menu(HOME) + self.footer(HOME):
            if target in ('', '#') or urlsplit(target).scheme:
                continue
            with self.subTest(target=target):
                self.assertTrue((DOCS / target).is_file())

    def test_footer_states_the_licence(self):
        licences = [link for link in page(HOME).find('footer').find_all('a')
                    if urlsplit(link.attrs.get('href', '')).netloc == 'creativecommons.org']
        self.assertEqual(len(licences), 1)

    def test_footer_does_not_repeat_the_header_profiles(self):
        hosts = {urlsplit(link.attrs.get('href', '')).netloc for link in page(HOME).find('footer').find_all('a')}
        self.assertFalse(hosts & {'github.com', 'www.linkedin.com', 'stackoverflow.com'})


class Breadcrumbs(unittest.TestCase):
    def trail(self, relative):
        return page(relative).find('nav', 'site-breadcrumbs')

    def test_home_has_no_trail(self):
        self.assertIsNone(self.trail(HOME))

    def test_every_other_page_has_one_labelled_trail(self):
        for relative in PAGES:
            if relative == HOME:
                continue
            with self.subTest(page=relative):
                trails = page(relative).find_all('nav', 'site-breadcrumbs')
                self.assertEqual(len(trails), 1)
                self.assertTrue(trails[0].attrs.get('aria-label'))
                self.assertIsNotNone(trails[0].find('ol'))

    def test_trail_starts_at_home_and_ends_at_the_current_page(self):
        for relative in PAGES:
            if relative == HOME:
                continue
            with self.subTest(page=relative):
                items = self.trail(relative).find_all('li')
                first = items[0].find('a')
                self.assertTrue(first.text)
                self.assertEqual(destination(relative, first.attrs['href']), 'index.html')
                self.assertEqual(items[-1].attrs.get('aria-current'), 'page')
                self.assertIsNone(items[-1].find('a'))
                self.assertEqual([item.attrs.get('aria-current') for item in items[:-1]], [None] * (len(items) - 1))

    def test_trail_ends_with_the_page_title(self):
        for relative in PAGES:
            if relative in (HOME, 'header-about.html'):
                continue
            with self.subTest(page=relative):
                current = self.trail(relative).find_all('li')[-1].text
                self.assertTrue(page(relative).find('title').text.startswith(current))

    def test_section_pages_hang_directly_under_home(self):
        for relative in PAGES:
            if '/' in relative or relative == HOME:
                continue
            with self.subTest(page=relative):
                self.assertEqual(len(self.trail(relative).find_all('li')), 2)

    def test_articles_show_their_section(self):
        for section, target in (('posts', 'writing.html'), ('portfolio', 'portfolio.html')):
            # The trail names the section with the same word the menu uses.
            label = next(link.text for link in page(HOME).by_id('navbarCollapse').find_all('a', 'nav-link')
                         if destination(HOME, link.attrs.get('href', '')) == target)
            for relative in articles(section):
                with self.subTest(page=relative):
                    items = self.trail(relative).find_all('li')
                    self.assertEqual(len(items), 3)
                    link = items[1].find('a')
                    self.assertEqual(link.text, label)
                    self.assertEqual(destination(relative, link.attrs['href']), target)


class Articles(unittest.TestCase):
    def test_there_are_articles_in_both_sections(self):
        self.assertEqual(len(articles('posts')), len(list(REPOSITORY.glob('posts/*/index.qmd'))))
        self.assertEqual(len(articles('portfolio')), len(list(REPOSITORY.glob('portfolio/*/index.qmd'))))

    def test_every_article_ends_with_a_way_back(self):
        for section, target in (('posts', 'writing.html'), ('portfolio', 'portfolio.html')):
            for relative in articles(section):
                with self.subTest(page=relative):
                    endings = page(relative).find_all('nav', 'article-end')
                    self.assertEqual(len(endings), 1)
                    self.assertTrue(endings[0].attrs.get('aria-label'))
                    back = endings[0].find('a', 'utility-link')
                    self.assertTrue(back.text)
                    self.assertEqual(destination(relative, back.attrs['href']), target)

    def test_end_of_article_is_the_last_block_of_the_text(self):
        for section in ('posts', 'portfolio'):
            for relative in articles(section):
                with self.subTest(page=relative):
                    main = page(relative).find('main')
                    ending = main.find('nav', 'article-end')
                    self.assertIs(ending.parent.children[-1], ending)

    def test_post_themes_filter_the_archive(self):
        archive = page('writing.html')
        known = {theme for entry in archive.find_all('article', 'archive-entry')
                 for theme in entry.attrs['data-categories'].split(', ') if theme}
        for relative in articles('posts'):
            with self.subTest(page=relative):
                tags = page(relative).find('nav', 'article-end').find_all('a', 'post-tag')
                self.assertGreater(len(tags), 0)
                for tag in tags:
                    url = urlsplit(tag.attrs['href'])
                    self.assertEqual(destination(relative, tag.attrs['href']), 'writing.html')
                    theme = parse_qs(url.query)['tema'][0]
                    self.assertIn(theme, known)
                    self.assertEqual(tag.text, '#' + '-'.join(theme.split()))

    def test_portfolio_articles_offer_no_theme_filter(self):
        for relative in articles('portfolio'):
            with self.subTest(page=relative):
                self.assertEqual(page(relative).find('nav', 'article-end').find_all('a', 'post-tag'), [])

    def test_article_has_a_title_heading_and_a_date(self):
        for section in ('posts', 'portfolio'):
            for relative in articles(section):
                with self.subTest(page=relative):
                    document = page(relative)
                    self.assertTrue(document.find('h1', 'title').text)
                    self.assertTrue(document.find('p', 'date').text)

    def test_long_articles_offer_a_table_of_contents(self):
        for section in ('posts', 'portfolio'):
            for relative in articles(section):
                document = page(relative)
                headings = [heading for level in ('h2', 'h3') for heading in document.find('main').find_all(level)]
                if len(headings) < 3:
                    continue
                with self.subTest(page=relative):
                    toc = document.by_id('TOC')
                    self.assertIsNotNone(toc)
                    for link in toc.find_all('a'):
                        self.assertIsNotNone(document.by_id(link.attrs['href'].lstrip('#')), link.attrs['href'])


class Home(unittest.TestCase):
    def setUp(self):
        self.document = page(HOME)
        self.main = self.document.find('main')

    def test_has_one_main_heading(self):
        headings = self.document.find_all('h1')
        self.assertEqual(len(headings), 1)
        self.assertTrue(headings[0].text)

    def test_sections_follow_the_reading_order(self):
        order = ['about-hero', 'about-selected-work', 'about-principles', 'about-history', 'about-recent-writing']
        found = [name for element in self.main.descendants() for name in element.classes if name in order]
        self.assertEqual(found, order)

    def test_every_section_has_a_heading(self):
        for name in ('about-selected-work', 'about-principles', 'about-history', 'about-recent-writing'):
            with self.subTest(section=name):
                self.assertTrue(self.main.find(cls=name).find('h2').text)

    def test_headings_do_not_skip_levels(self):
        levels = [int(element.tag[1]) for element in self.main.descendants() if re.fullmatch('h[1-6]', element.tag)]
        self.assertEqual([(a, b) for a, b in zip(levels, levels[1:]) if b > a + 1], [])

    def test_hero_offers_the_two_main_paths(self):
        actions = self.main.find_all('a', 'about-action')
        self.assertEqual([destination(HOME, action.attrs['href']) for action in actions], ['portfolio.html', 'writing.html'])

    def test_portrait_is_described(self):
        portrait = self.main.find('figure', 'portrait-plot') or self.main.find(cls='portrait-plot')
        self.assertTrue(portrait.find('img').attrs.get('alt'))

    def test_selected_work_shows_three_described_cards(self):
        cards = self.main.find_all(cls='about-project-card')
        self.assertEqual(len(cards), 3)
        for card in cards:
            image, title = card.find('img'), card.find('h3').find('a')
            self.assertGreater(len(image.attrs.get('alt', '')), 20)
            self.assertTrue(title.text)
            self.assertTrue(destination(HOME, title.attrs['href']).startswith('portfolio/'))
            self.assertEqual(card.find('a', 'about-project-card__media').attrs['href'], title.attrs['href'])
            self.assertTrue(card.find_all('p')[-1].text)

    def test_recent_writing_shows_the_three_newest_posts(self):
        entries = self.main.find_all('article', 'archive-entry')
        newest = [entry.find('time').attrs['datetime'] for entry in page('writing.html').find_all('article', 'archive-entry')][:3]
        self.assertEqual([entry.find('time').attrs['datetime'] for entry in entries], newest)

    def test_sections_end_with_a_path_to_the_full_content(self):
        links = [destination(HOME, link.attrs['href']) for link in self.main.find_all('a', 'experience-link')]
        self.assertEqual(links, ['header-experience.html', 'writing.html'])

    def test_about_alias_serves_the_same_content(self):
        alias = page('header-about.html').find('main')
        self.assertEqual([heading.text for heading in alias.find_all('h2')], [heading.text for heading in self.main.find_all('h2')])
        self.assertEqual(alias.find('h1').text, self.main.find('h1').text)


class ExperienceTable(unittest.TestCase):
    PAGES = (HOME, 'header-experience.html')

    @staticmethod
    def table(relative):
        return page(relative).find(cls='experience-table-scroll').find('table')

    def test_has_column_headers(self):
        for relative in self.PAGES:
            with self.subTest(page=relative):
                table = self.table(relative)
                headers = [cell.text for cell in table.find('thead').find_all('th')]
                self.assertEqual(len(headers), 3)
                self.assertTrue(all(headers))

    def test_rows_are_complete_and_in_reverse_chronological_order(self):
        for relative in self.PAGES:
            with self.subTest(page=relative):
                table = self.table(relative)
                rows = table.find('tbody').find_all('tr')
                self.assertGreaterEqual(len(rows), 9)
                starts = []
                for row in rows:
                    cells = row.find_all('td')
                    self.assertEqual(len(cells), 3)
                    period = re.fullmatch(r'(\d\d)/(\d\d) - (\d\d)/(\d\d)', cells[0].text)
                    self.assertIsNotNone(period, cells[0].text)
                    starts.append((int(period.group(2)), int(period.group(1))))
                    self.assertTrue(cells[1].text)
                    self.assertGreater(len(cells[2].text), 40)
                self.assertEqual(starts, sorted(starts, reverse=True))

    def test_logos_are_described_and_sized(self):
        for relative in self.PAGES:
            with self.subTest(page=relative):
                table = self.table(relative)
                for logo in table.find_all('img', 'company-logo'):
                    self.assertTrue(logo.attrs.get('alt'))
                    self.assertEqual((logo.attrs.get('width'), logo.attrs.get('height')), ('31', '31'))


class BlogArchive(unittest.TestCase):
    def setUp(self):
        self.document = page('writing.html')
        self.entries = self.document.find_all('article', 'archive-entry')

    def test_page_is_named_like_its_menu_entry(self):
        entry = next(link for link in self.document.by_id('navbarCollapse').find_all('a', 'nav-link')
                     if destination('writing.html', link.attrs.get('href', '')) == 'writing.html')
        self.assertEqual(self.document.find('h1').text, entry.text)

    def test_lists_every_post_once(self):
        targets = [destination('writing.html', entry.find('a').attrs['href']) for entry in self.entries]
        self.assertEqual(sorted(targets), sorted(articles('posts')))

    def test_entries_are_newest_first(self):
        dates = [entry.find('time').attrs['datetime'] for entry in self.entries]
        self.assertEqual(dates, sorted(dates, reverse=True))

    def test_dates_are_machine_readable(self):
        for entry in self.entries:
            moment = entry.find('time')
            self.assertEqual(moment.attrs['datetime'], moment.text)
            date.fromisoformat(moment.attrs['datetime'])

    def test_entry_title_matches_the_post(self):
        for entry in self.entries:
            link = [anchor for anchor in entry.find('h3').find_all('a')][0]
            target = destination('writing.html', link.attrs['href'])
            with self.subTest(page=target):
                # Pandoc curls the apostrophes of the post title; the archive keeps the source text.
                self.assertEqual(link.text.replace("'", '’'), page(target).find('h1', 'title').text.replace("'", '’'))

    def test_language_badge_is_announced_in_words(self):
        for entry in self.entries:
            badge = entry.find('span', 'post-language')
            self.assertEqual(badge.attrs.get('role'), 'img')
            self.assertEqual(badge.text, entry.attrs['data-language'].upper())
            self.assertGreater(len(badge.attrs.get('aria-label', '')), len(badge.text))

    def test_language_badge_matches_the_language_of_the_post(self):
        for entry in self.entries:
            target = destination('writing.html', entry.find('h3').find('a').attrs['href'])
            with self.subTest(page=target):
                self.assertTrue(page(target).find('html').attrs['lang'].lower().startswith(entry.attrs['data-language']))

    def test_months_group_only_their_own_posts(self):
        months = self.document.find_all('section', 'archive-month')
        self.assertEqual(sum(len(month.find_all('article', 'archive-entry')) for month in months), len(self.entries))
        names = ['Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho', 'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro']
        for month in months:
            heading = re.fullmatch(r'(\S+) de (\d{4})', month.find('h2').text)
            self.assertIsNotNone(heading)
            prefix = f'{heading.group(2)}-{names.index(heading.group(1)) + 1:02d}'
            entries = month.find_all('article', 'archive-entry')
            self.assertGreater(len(entries), 0)
            for entry in entries:
                self.assertTrue(entry.find('time').attrs['datetime'].startswith(prefix))

    def test_every_entry_has_themes_that_filter_the_list(self):
        for entry in self.entries:
            themes = [theme for theme in entry.attrs['data-categories'].split(', ') if theme]
            tags = entry.find_all('a', 'post-tag')
            self.assertGreater(len(themes), 0)
            self.assertEqual([tag.attrs['data-category'] for tag in tags], themes)
            for tag in tags:
                self.assertEqual(parse_qs(urlsplit(tag.attrs['href']).query)['tema'], [tag.attrs['data-category']])

    def test_filter_script_is_loaded_without_blocking_the_page(self):
        scripts = [script for script in self.document.find_all('script') if 'archive-filters.js' in script.attrs.get('src', '')]
        self.assertEqual(len(scripts), 1)
        self.assertIn('defer', scripts[0].attrs)

    def test_list_is_readable_without_the_filter_script(self):
        # Entries and months are plain HTML; the script only adds the filter controls.
        self.assertEqual(self.document.find_all(cls='archive-filters'), [])
        self.assertEqual([entry for entry in self.entries if 'hidden' in entry.attrs], [])


class Portfolio(unittest.TestCase):
    def setUp(self):
        self.document = page('portfolio.html')
        self.cards = self.document.find_all(cls='quarto-grid-item')
        self.links = self.document.find_all('a', 'quarto-grid-link')

    def test_lists_every_project_once(self):
        targets = [destination('portfolio.html', link.attrs['href']) for link in self.links]
        self.assertEqual(sorted(targets), sorted(articles('portfolio')))

    def test_the_whole_card_is_one_link(self):
        self.assertEqual(len(self.links), len(self.cards))
        for link in self.links:
            self.assertEqual(len(link.find_all(cls='quarto-grid-item')), 1)

    def test_has_no_filter_box(self):
        self.assertEqual(self.document.find_all(cls='quarto-listing-filter'), [])
        self.assertEqual(self.document.find('main').find_all('input'), [])

    def test_all_projects_fit_one_page(self):
        self.assertEqual(self.document.find_all(cls='listing-pagination'), [])
        self.assertEqual(self.document.find_all(cls='pagination'), [])

    def test_cards_have_a_described_cover_a_title_and_a_summary(self):
        for card in self.cards:
            cover = card.find('img')
            self.assertGreater(len(cover.attrs.get('alt', '')), 20)
            self.assertTrue(card.find(cls='card-title').text)
            self.assertGreater(len(card.find(cls='card-text').text), 20)

    def test_newest_project_comes_first(self):
        targets = [destination('portfolio.html', link.attrs['href']) for link in self.links]
        self.assertEqual(targets, sorted(targets, reverse=True))


class Records(unittest.TestCase):
    """Courses and certifications: one row per record, grouped by year."""

    PAGES = ('header-courses.html', 'header-certifications.html')

    def test_every_year_has_one_list_of_records(self):
        for relative in self.PAGES:
            with self.subTest(page=relative):
                main = page(relative).find('main')
                years = [heading for heading in main.find_all('h2') if re.fullmatch(r'\d{4}', heading.text)]
                self.assertGreater(len(years), 0)
                self.assertEqual(len(main.find_all('div', 'record-list')), len(years))
                self.assertEqual([heading.text for heading in years], sorted((heading.text for heading in years), reverse=True))

    def test_rows_have_a_described_logo_a_date_and_a_title(self):
        for relative in self.PAGES:
            with self.subTest(page=relative):
                rows = [row for group in page(relative).find_all('div', 'record-list') for row in group.find('ul').children]
                self.assertGreater(len(rows), 5)
                for row in rows:
                    self.assertTrue(row.find('img').attrs.get('alt'))
                    self.assertRegex(row.find('span', 'record-date').text, r'^(0[1-9]|1[0-2])/\d\d$')
                    self.assertTrue(row.find('span', 'record-title').text)

    def test_record_dates_belong_to_the_year_they_are_listed_under(self):
        for relative in self.PAGES:
            with self.subTest(page=relative):
                for section in page(relative).find('main').find_all('section', 'level2'):
                    year = section.find('h2').text
                    if not re.fullmatch(r'\d{4}', year):
                        continue
                    for stamp in section.find_all('span', 'record-date'):
                        self.assertEqual(stamp.text[-2:], year[-2:], f'{stamp.text} under {year}')

    def test_no_record_is_left_as_a_loose_paragraph(self):
        for relative in self.PAGES:
            with self.subTest(page=relative):
                loose = [paragraph.text for paragraph in page(relative).find('main').find_all('p')
                         if re.search(r'\[\d\d/\d\d\]', paragraph.text)]
                self.assertEqual(loose, [])


class Embeds(unittest.TestCase):
    def test_talk_embeds_adapt_to_the_column(self):
        frames = page('header-participations.html').find_all('iframe')
        self.assertEqual(len(frames), 2)
        for frame in frames:
            self.assertTrue(frame.has_class('embed'))
            self.assertNotIn('width', frame.attrs)
            self.assertNotIn('style', frame.attrs)
            self.assertEqual(frame.attrs.get('loading'), 'lazy')

    def test_talks_show_the_country_in_words(self):
        flags = [image for image in page('header-participations.html').find('main').find_all('img')]
        self.assertGreater(len(flags), 20)
        self.assertEqual([flag.attrs.get('src') for flag in flags if not flag.attrs.get('alt')], [])


class Redirects(unittest.TestCase):
    def test_old_portuguese_addresses_still_lead_somewhere(self):
        stubs = sorted((DOCS / 'pt-br').glob('*.html'))
        self.assertGreater(len(stubs), 0)
        for stub in stubs:
            with self.subTest(page=stub.name):
                text = stub.read_text(encoding='utf-8')
                target = re.search(r'url=([^"\'>\s]+)', text)
                self.assertIsNotNone(target)
                self.assertTrue((DOCS / 'pt-br' / target.group(1)).resolve().is_file() or
                                (DOCS / target.group(1).lstrip('./')).is_file(), target.group(1))
                self.assertIn('<a ', text)


if __name__ == '__main__':
    unittest.main()
