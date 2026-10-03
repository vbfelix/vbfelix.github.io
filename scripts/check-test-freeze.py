"""Refuse a commit that modifies, renames or deletes existing test code.

A change or a new feature must be proven by the tests that already exist, so those tests are
frozen: a commit may add new test files, never touch the ones already versioned. Editing a test
is a separate decision that belongs to the user, who enables it for one commit by setting
SITE_ALLOW_TEST_EDIT=1. Reads the staged changes; use it from `site.ps1 -Action verify -Scope staged`.
"""
import os
import re
import subprocess
import sys

TEST_CODE = re.compile(r'^scripts/(test_[^/]+\.py|test-[^/]+\.ts)$')
OVERRIDE = 'SITE_ALLOW_TEST_EDIT'


def is_test_code(path):
    return bool(TEST_CODE.match(path.replace('\\', '/')))


def violations(entries):
    """Staged entries, as (status, paths), that touch test code already in the repository.

    `status` is the letter Git reports: A added, M modified, D deleted, R renamed, C copied, T type changed.
    """
    found = []
    for status, paths in entries:
        kind = status[:1]
        if kind in ('A', 'C'):  # an added file, or a copy that leaves its source untouched
            continue
        # A rename lists the old path first; either side being a test counts as touching it.
        touched = [path for path in paths if is_test_code(path)]
        if touched:
            found.append((kind, touched[0]))
    return found


def staged_entries():
    output = subprocess.run(['git', 'diff', '--cached', '--name-status', '-z'], capture_output=True, check=True).stdout
    fields = [field for field in output.decode('utf-8').split('\0') if field]
    entries, index = [], 0
    while index < len(fields):
        status = fields[index]
        count = 2 if status[:1] in ('R', 'C') else 1
        entries.append((status, fields[index + 1:index + 1 + count]))
        index += 1 + count
    return entries


def main():
    found = violations(staged_entries())
    if not found:
        print('PASS: no existing test code is modified by this commit.')
        return
    if os.environ.get(OVERRIDE) == '1':
        print(f'WARN: {OVERRIDE}=1, committing changes to existing test code: ' + ', '.join(path for _, path in found))
        return
    names = {'M': 'modified', 'D': 'deleted', 'R': 'renamed', 'T': 'changed type', 'C': 'copied over'}
    for kind, path in found:
        print(f'{path}: {names.get(kind, kind)}')
    print('FAIL: existing test code is frozen. A change or a new feature may add new test files, '
          'never edit the tests that already exist.\n'
          f'If the user explicitly asked to change a test, commit it alone with {OVERRIDE}=1.')
    sys.exit(1)


if __name__ == '__main__':
    main()
