"""Reproduce the educational homepage SVG with deterministic simulated data.

The band is a pointwise 95% t interval for the mean response, not prediction.
No personal or professional observations are used.
"""
from pathlib import Path
from math import sqrt, log, cos, pi

def main():
    seed = 20260905
    def uniform():
        nonlocal seed
        seed = (1664525 * seed + 1013904223) % 2**32
        return (seed + .5) / 2**32

    points = []
    for i in range(48):
        x = .3 + i * 9.4 / 47
        noise = sqrt(-2 * log(uniform())) * cos(2 * pi * uniform()) * .95
        points.append((x, 1.5 + .64 * x + noise))
    mx = sum(x for x, y in points) / 48
    my = sum(y for x, y in points) / 48
    sxx = sum((x - mx)**2 for x, y in points)
    b = sum((x - mx) * (y - my) for x, y in points) / sxx
    a = my - b * mx
    se = sqrt(sum((y - a - b*x)**2 for x, y in points) / 46)
    X = lambda x: 52 + x*42
    Y = lambda y: 354 - y*30
    band = []
    for i in range(51):
        x = i / 5
        half = 2.012896 * se * sqrt(1/48 + (x-mx)**2 / sxx)
        band.append((X(x), Y(a + b*x + half), Y(a + b*x - half)))
    svg = ['<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 510 408">']
    svg.append('<g stroke="#648477" stroke-width=".7">')
    for n in range(0, 11, 2):
        svg.append(f'<path d="M{X(n)} 38V354M52 {Y(n)}H472"/>')
    svg.append('</g><g fill="#c0cec0" font-family="monospace" font-size="11">')
    for n in range(0, 11, 2):
        svg.append(f'<text x="{X(n)}" y="375" text-anchor="middle">{n}</text><text x="36" y="{Y(n)+4}" text-anchor="end">{n}</text>')
    svg.append(f'<text x="473" y="397">x</text><text x="18" y="36">y</text><text x="52" y="20">n = 48</text><text x="472" y="20" text-anchor="end">y = {a:.2f} + {b:.2f}x</text></g>')
    edge = 'L'.join(f'{x},{upper}' for x, upper, lower in band)
    edge += 'L' + 'L'.join(f'{x},{lower}' for x, upper, lower in reversed(band))
    svg.append(f'<path d="M{edge}Z" fill="#54796a"/>')
    svg.append(f'<path d="M{X(0)} {Y(a)}L{X(10)} {Y(a+b*10)}" stroke="#b8d9df" stroke-width="2" fill="none"/>')
    for x, y in points:
        svg.append(f'<circle cx="{X(x)}" cy="{Y(y)}" r="3.2" fill="#efd58e" fill-opacity=".75"/>')
    svg.append('<path d="M52 38V354H472" stroke="#c0cec0" fill="none"/></svg>')
    target = Path(__file__).resolve().parents[1] / 'maintenance/design/notebook-regression.svg'
    target.write_text(''.join(svg) + '\n', encoding='utf-8')
    print(f'OLS: intercept={a:.6f}, slope={b:.6f}, residual SE={se:.6f}')


if __name__ == '__main__':
    main()
