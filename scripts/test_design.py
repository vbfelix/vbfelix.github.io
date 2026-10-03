"""The stylesheet against the rules in DESIGN.md: contrast, text size, breakpoints, focus and motion."""
import re
import unittest

from test_support import REPOSITORY

CSS = (REPOSITORY / 'styles.css').read_text(encoding='utf-8')
THEME = (REPOSITORY / 'custom_theme.scss').read_text(encoding='utf-8')
DESIGN = (REPOSITORY / 'DESIGN.md').read_text(encoding='utf-8')
BOOTSTRAP_BREAKPOINTS = {'575.98px', '767.98px', '991.98px', '1199.98px', '1399.98px'}


def root_tokens():
    block = re.search(r':root\s*\{(.*?)\}', CSS, re.S).group(1)
    return dict(re.findall(r'(--[\w-]+)\s*:\s*([^;}]+)', block))


def design_colors():
    block = re.search(r'^colors:\n((?:  .+\n)+)', DESIGN, re.M).group(1)
    return {name: value.lower() for name, value in re.findall(r'^  ([\w-]+): "(#[0-9A-Fa-f]{6})"', block, re.M)}


def luminance(color):
    channels = [int(color[index:index + 2], 16) / 255 for index in (1, 3, 5)]
    linear = [value / 12.92 if value <= 0.03928 else ((value + 0.055) / 1.055) ** 2.4 for value in channels]
    return 0.2126 * linear[0] + 0.7152 * linear[1] + 0.0722 * linear[2]


def contrast(foreground, background):
    """WCAG 2 contrast ratio between two #rrggbb colors."""
    lighter, darker = sorted((luminance(foreground), luminance(background)), reverse=True)
    return (lighter + 0.05) / (darker + 0.05)


def rules():
    """Every `selector { declarations }` pair, including those inside media queries."""
    without_comments = re.sub(r'/\*.*?\*/', '', CSS, flags=re.S)
    return [(selector.strip(), body) for selector, body in re.findall(r'([^{}]+)\{([^{}]*)\}', without_comments)]


def font_sizes():
    """Font sizes in rem declared through `font-size` or the `font` shorthand."""
    sizes = []
    for selector, body in rules():
        sizes += [(selector, float(value)) for value in re.findall(r'font-size\s*:\s*(\d*\.?\d+)rem', body)]
        sizes += [(selector, float(value)) for value in re.findall(r'font\s*:[^;]*?(\d*\.?\d+)rem\s*/', body)]
    return sizes


class Tokens(unittest.TestCase):
    def setUp(self):
        self.tokens = root_tokens()
        self.design = design_colors()

    def test_css_colors_are_the_ones_in_the_design_file(self):
        pairs = {'--paper': 'primary', '--ink': 'text', '--muted': 'text-muted', '--line': 'line', '--blue': 'link',
                 '--rust': 'accent', '--wash': 'surface', '--surface-deep': 'surface-deep', '--selection': 'selection'}
        for token, name in pairs.items():
            with self.subTest(token=token):
                self.assertEqual(self.tokens[token].strip().lower(), self.design[name])

    def test_theme_colors_are_the_ones_in_the_design_file(self):
        pairs = {'$body-bg': 'primary', '$body-color': 'text', '$link-color': 'link', '$link-hover-color': 'accent',
                 '$border-color': 'line', '$navbar-bg': 'primary', '$navbar-fg': 'text', '$navbar-hl': 'accent'}
        for variable, name in pairs.items():
            with self.subTest(variable=variable):
                value = re.search(re.escape(variable) + r':\s*(#[0-9A-Fa-f]{6})', THEME).group(1)
                self.assertEqual(value.lower(), self.design[name])

    def test_spacing_scale_is_the_one_in_the_design_file(self):
        block = re.search(r'^spacing:\n((?:  .+\n)+)', DESIGN, re.M).group(1)
        for name, value in re.findall(r'^  (\w+): ([\d.]+rem)', block, re.M):
            with self.subTest(step=name):
                self.assertEqual(float(self.tokens[f'--space-{name}'].replace('rem', '')), float(value.replace('rem', '')))

    def test_reading_and_catalog_widths_follow_the_design_file(self):
        self.assertEqual(self.tokens['--measure-read'].strip(), '900px')
        self.assertEqual(self.tokens['--measure-wide'].strip(), '1200px')
        self.assertIn('900px', DESIGN)
        self.assertIn('1200px', DESIGN)

    def test_every_variable_used_is_defined(self):
        used = set(re.findall(r'var\((--[\w-]+)', CSS))
        local = set(re.findall(r'(--[\w-]+)\s*:', CSS))
        undefined = sorted(name for name in used - local if not name.startswith('--bs-'))
        self.assertEqual(undefined, [])

    def test_colors_live_only_in_the_tokens(self):
        outside = re.sub(r':root\s*\{.*?\}', '', CSS, count=1, flags=re.S)
        outside = re.sub(r'url\("data:[^"]*"\)', '', outside)
        self.assertEqual(re.findall(r'#[0-9a-fA-F]{3,8}\b', outside), ['#000'])  # the portrait mask


class Contrast(unittest.TestCase):
    def setUp(self):
        self.colors = design_colors()

    def ratio(self, foreground, background):
        return contrast(self.colors[foreground], self.colors[background])

    def test_text_reaches_aa_on_every_surface(self):
        for foreground in ('text', 'text-muted', 'link', 'accent'):
            for background in ('primary', 'surface', 'surface-deep'):
                with self.subTest(foreground=foreground, background=background):
                    self.assertGreaterEqual(self.ratio(foreground, background), 4.5)

    def test_menu_item_under_the_cursor_stays_readable(self):
        # The dropdown item uses accent on surface; a near-white bar once made it unreadable.
        self.assertGreaterEqual(self.ratio('accent', 'surface'), 4.5)
        menu = next(body for selector, body in rules() if selector == '.navbar .dropdown-menu')
        self.assertIn('--bs-dropdown-link-hover-bg:var(--wash)', menu)
        self.assertIn('--bs-dropdown-link-hover-color:var(--rust)', menu)
        self.assertIn('--bs-dropdown-link-active-bg:var(--wash)', menu)
        self.assertIn('--bs-dropdown-bg:var(--surface-deep)', menu)

    def test_selected_text_stays_readable(self):
        self.assertGreaterEqual(self.ratio('text', 'selection'), 4.5)

    def test_lines_and_focus_rings_are_visible(self):
        for foreground in ('line', 'accent', 'link'):
            for background in ('primary', 'surface'):
                with self.subTest(foreground=foreground, background=background):
                    self.assertGreaterEqual(self.ratio(foreground, background), 3 if foreground != 'line' else 2)

    def test_code_blocks_are_black_with_contrast_enforced(self):
        self.assertRegex(THEME, r'\$code-block-bg:\s*#000000')
        self.assertRegex(THEME, r'\$min-contrast-ratio:\s*4\.5')
        for name in ('text', 'link', 'accent', 'text-muted'):
            with self.subTest(color=name):
                self.assertGreaterEqual(contrast(self.colors[name], '#000000'), 4.5)


class Typography(unittest.TestCase):
    def test_no_text_is_smaller_than_the_label_size(self):
        small = [(selector, size) for selector, size in font_sizes() if size < 0.75]
        self.assertEqual(small, [])

    def test_body_text_is_relative_to_the_reader_setting(self):
        body = ' '.join(body for selector, body in rules() if selector == 'body')
        self.assertIn('font-size:1rem', body)
        self.assertRegex(body, r'line-height:1\.[5-9]')

    def test_text_sizes_do_not_use_pixels(self):
        fixed = [(selector, value) for selector, body in rules()
                 for value in re.findall(r'font-size\s*:\s*(\d+px)', body)]
        self.assertEqual(fixed, [])

    def test_titles_keep_the_normal_weight(self):
        heavy = [(selector, weight) for selector, body in rules()
                 for weight in re.findall(r'font-weight\s*:\s*(\d+|bold)', body) if weight == 'bold' or int(weight) > 400]
        self.assertEqual(heavy, [])
        banner = next(body for selector, body in rules() if selector == '.quarto-title-banner .quarto-title .title')
        self.assertIn('font-weight:400', banner)

    def test_uppercase_is_reserved_for_mono_labels(self):
        for selector, body in rules():
            if 'text-transform:uppercase' not in body:
                continue
            with self.subTest(selector=selector):
                self.assertIn('var(--mono)', body)

    def test_design_label_size_matches_the_floor(self):
        label = re.search(r'^  label:\n(?:    .+\n)*?    fontSize: "([\d.]+)rem"', DESIGN, re.M).group(1)
        self.assertEqual(float(label), 0.75)


class Layout(unittest.TestCase):
    def test_breakpoints_are_the_ones_bootstrap_uses(self):
        widths = re.findall(r'@media\s*\(\s*max-width\s*:\s*([\d.]+px)\s*\)', CSS)
        self.assertGreater(len(widths), 0)
        self.assertLessEqual(set(widths), BOOTSTRAP_BREAKPOINTS)
        self.assertEqual(re.findall(r'@media\s*\(\s*min-width', CSS), [])

    def test_hero_stacks_when_the_menu_collapses(self):
        block = re.search(r'@media\(max-width:991\.98px\)\s*\{(.*?)\n\}', CSS, re.S).group(1)
        self.assertRegex(block, r'\.about-hero\s*\{[^}]*grid-template-columns:1fr')

    def test_phone_layouts_are_single_column(self):
        block = re.search(r'@media\(max-width:767\.98px\)\s*\{(.*?)\n\}', CSS, re.S).group(1)
        for selector in ('.about-projects', '.about-principle', '.archive-entry'):
            with self.subTest(selector=selector):
                self.assertRegex(block, re.escape(selector) + r'\s*\{[^}]*grid-template-columns:1fr')

    def test_experience_table_does_not_force_sideways_scroll_on_phones(self):
        block = re.search(r'@media\(max-width:575\.98px\)\s*\{(.*?)\n\}', CSS, re.S).group(1)
        self.assertRegex(block, r'\.experience-table-scroll table\s*\{[^}]*min-width:0')
        self.assertRegex(block, r'\.experience-table-scroll td\s*\{[^}]*display:block')

    def test_wide_content_can_scroll_instead_of_breaking_the_page(self):
        scroll = next(body for selector, body in rules() if selector == '.experience-table-scroll')
        self.assertIn('overflow-x:auto', scroll)
        media = next(body for selector, body in rules() if selector == 'img,svg')
        self.assertIn('max-width:100%', media)

    def test_content_widths_use_the_tokens(self):
        reading = next(body for selector, body in rules() if selector == 'main.content')
        self.assertIn('max-width:var(--measure-read)', reading)
        archive = next(body for selector, body in rules() if selector == 'body.writing-page main.content')
        self.assertIn('max-width:var(--measure-read)', archive)
        wide = next(body for selector, body in rules() if 'body.portfolio-page main.content' in selector)
        self.assertIn('max-width:var(--measure-wide)', wide)

    def test_no_layout_width_is_fixed_in_pixels(self):
        allowed = {'.about-hero__portrait', '.company-logo'}
        fixed = [(selector, value) for selector, body in rules()
                 for value in re.findall(r'(?<![-\w])width\s*:\s*(\d{3,}px)', body) if selector not in allowed]
        self.assertEqual(fixed, [])

    def test_embeds_fill_the_column(self):
        embed = next(body for selector, body in rules() if selector == '.embed')
        self.assertIn('width:100%', embed)
        video = next(body for selector, body in rules() if selector == '.embed--video')
        self.assertRegex(video, r'aspect-ratio:16 / 9')

    def test_anchor_targets_clear_the_fixed_header(self):
        html = next(body for selector, body in rules() if selector == 'html')
        self.assertRegex(html, r'scroll-padding-top:\d+rem')

    def test_tags_are_large_enough_to_tap(self):
        tag = next(body for selector, body in rules() if selector == '.post-tag')
        self.assertRegex(tag, r'padding:\.[3-9]\d*rem 0')


class Identity(unittest.TestCase):
    def test_there_are_no_shadows(self):
        self.assertEqual(re.findall(r'(?:box|text)-shadow\s*:\s*(?!none)[^;}]+', CSS), [])

    def test_rounded_corners_stay_within_the_card_radius(self):
        radii = [int(value) for value in re.findall(r'border-radius\s*:\s*(\d+)px', CSS)]
        self.assertTrue(all(radius <= 6 for radius in radii), radii)

    def test_no_blur_or_glass_effects(self):
        self.assertNotIn('backdrop-filter', CSS)
        self.assertEqual(re.findall(r'(?<!-)filter\s*:\s*blur', CSS), [])

    def test_cover_images_are_not_cropped(self):
        for selector, body in rules():
            if 'aspect-ratio:16 / 9' in body and 'img' in selector:
                with self.subTest(selector=selector):
                    self.assertIn('object-fit:contain', body)
        self.assertNotIn('object-fit:cover', CSS)


class FocusAndMotion(unittest.TestCase):
    def test_focus_is_always_visible(self):
        focus = next(body for selector, body in rules() if selector == ':focus-visible')
        self.assertRegex(focus, r'outline:2px solid var\(--focus,var\(--rust\)\)')
        self.assertEqual(re.findall(r'outline\s*:\s*(?:none|0)\b', CSS), [])

    def test_component_focus_colors_come_from_the_palette(self):
        colors = set(re.findall(r'--focus\s*:\s*([^;}]+)', CSS))
        self.assertLessEqual({color.strip() for color in colors}, {'var(--blue)', 'var(--rust)'})

    def test_skip_link_appears_on_focus_above_the_header(self):
        hidden = next(body for selector, body in rules() if selector == '.skip-link')
        shown = next(body for selector, body in rules() if selector == '.skip-link:focus')
        self.assertRegex(hidden, r'top:-\d')
        self.assertRegex(shown, r'top:\.?\d')
        self.assertGreater(int(re.search(r'z-index:(\d+)', hidden).group(1)), 1030)  # Bootstrap's fixed header

    def test_reduced_motion_is_respected(self):
        block = re.search(r'@media\(prefers-reduced-motion:reduce\)\s*\{(.*?)\n\}', CSS, re.S).group(1)
        self.assertIn('scroll-behavior:auto', block)
        self.assertIn('animation:none!important', block)
        self.assertIn('transition:none!important', block)

    def test_hidden_entries_stay_hidden(self):
        # The archive filter hides entries with the `hidden` attribute; a display rule must not undo it.
        for selector in ('.archive-entry[hidden]', '.archive-month[hidden]'):
            with self.subTest(selector=selector):
                self.assertIn('display:none', next(body for name, body in rules() if name == selector))


class Stylesheet(unittest.TestCase):
    def test_braces_are_balanced(self):
        without_comments = re.sub(r'/\*.*?\*/', '', CSS, flags=re.S)
        self.assertEqual(without_comments.count('{'), without_comments.count('}'))

    def test_declarations_are_well_formed(self):
        for selector, body in rules():
            for declaration in filter(None, (part.strip() for part in re.sub(r'url\("[^"]*"\)', 'url()', body).split(';'))):
                with self.subTest(selector=selector, declaration=declaration[:60]):
                    self.assertRegex(declaration, r'^[-\w]+\s*:\s*\S')
                    name, value = declaration.split(':', 1)
                    self.assertNotIn(':', re.sub(r'\([^)]*\)', '', value), 'stray colon in value')

    def test_important_is_used_sparingly(self):
        self.assertLessEqual(CSS.count('!important'), 16)

    def test_published_stylesheet_is_the_source(self):
        self.assertEqual((REPOSITORY / 'docs' / 'styles.css').read_text(encoding='utf-8'), CSS)


if __name__ == '__main__':
    unittest.main()
