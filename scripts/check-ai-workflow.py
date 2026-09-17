"""Validate project-scoped Codex agents, hooks, and skills."""
from __future__ import annotations

import argparse
import json
from pathlib import Path
try:
    import tomllib
except ModuleNotFoundError:  # Python 3.10 compatibility
    tomllib = None

ROOT = Path(__file__).resolve().parents[1]


def check() -> list[str]:
    errors: list[str] = []
    if tomllib is None:
        config_text = (ROOT / '.codex/config.toml').read_text(encoding='utf-8')
        config = {'agents': {
            'enabled': 'enabled = true' in config_text,
            'max_concurrent_threads_per_session': int(next(
                line.split('=', 1)[1].strip() for line in config_text.splitlines()
                if line.strip().startswith('max_concurrent_threads_per_session')
            )),
        }}
    else:
        config = tomllib.loads((ROOT / '.codex/config.toml').read_text(encoding='utf-8'))
    agents_config = config.get('agents', {})
    if not agents_config.get('enabled'):
        errors.append('.codex/config.toml: agents.enabled must be true')
    if agents_config.get('max_concurrent_threads_per_session', 0) > 3:
        errors.append('.codex/config.toml: concurrency exceeds the project limit of 3')

    expected = {'code_reviewer', 'copy_reviewer'}
    found: set[str] = set()
    for path in sorted((ROOT / '.codex/agents').glob('*.toml')):
        agent_text = path.read_text(encoding='utf-8')
        if tomllib is None:
            data = {}
            for key in ('name', 'description', 'sandbox_mode'):
                prefix = f'{key} = '
                line = next((item for item in agent_text.splitlines() if item.startswith(prefix)), '')
                if line:
                    data[key] = line.split('=', 1)[1].strip().strip('"')
            if 'developer_instructions = """' in agent_text:
                data['developer_instructions'] = True
        else:
            data = tomllib.loads(agent_text)
        missing = {'name', 'description', 'developer_instructions'} - data.keys()
        if missing:
            errors.append(f'{path.relative_to(ROOT)}: missing {sorted(missing)}')
        if data.get('sandbox_mode') != 'read-only':
            errors.append(f'{path.relative_to(ROOT)}: reviewer must be read-only')
        found.add(data.get('name', ''))
    if found != expected:
        errors.append(f'.codex/agents: expected {sorted(expected)}, found {sorted(found)}')

    hooks = json.loads((ROOT / '.codex/hooks.json').read_text(encoding='utf-8'))
    if not hooks.get('hooks', {}).get('Stop'):
        errors.append('.codex/hooks.json: Stop hook is missing')

    for path in sorted((ROOT / '.agents/skills').glob('*/SKILL.md')):
        text = path.read_text(encoding='utf-8')
        parts = text.split('---', 2)
        if len(parts) < 3 or 'name:' not in parts[1] or 'description:' not in parts[1]:
            errors.append(f'{path.relative_to(ROOT)}: invalid frontmatter')
        if 'TODO' in text or '[TODO' in text:
            errors.append(f'{path.relative_to(ROOT)}: unfinished placeholder')
    return errors


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--quiet', action='store_true')
    args = parser.parse_args()
    try:
        errors = check()
    except (OSError, ValueError, StopIteration, json.JSONDecodeError) as exc:
        errors = [str(exc)]
    if errors:
        if not args.quiet:
            print('\n'.join(errors))
        return 1
    if not args.quiet:
        print('PASS: Codex agents, hooks, and skills are valid.')
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
