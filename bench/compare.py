#!/usr/bin/env python3
"""Compare two ebb benchmark result files and gate on regressions.

Usage: compare.py BASE_TSV HEAD_TSV [--threshold PERCENT]

Both files hold "case<TAB>ms-per-iter<TAB>MiB/s" lines as written by
bench/run-ebb-bench.sh --out.  Exits non-zero when any case present in
both files is more than PERCENT (default 25) slower in HEAD than in
BASE.  The two runs must come from the same machine (CI runs them
back to back on one runner); absolute numbers across machines are
meaningless.
"""

import argparse
import sys


def load(path):
    results = {}
    with open(path) as fh:
        for line in fh:
            parts = line.rstrip("\n").split("\t")
            if len(parts) == 3:
                results[parts[0]] = float(parts[1])
    return results


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("base")
    parser.add_argument("head")
    parser.add_argument("--threshold", type=float, default=25.0,
                        help="percent slowdown allowed per case")
    args = parser.parse_args()

    base, head = load(args.base), load(args.head)
    common = sorted(base.keys() & head.keys())
    if not common:
        print("error: no common benchmark cases", file=sys.stderr)
        return 2

    failed = False
    print(f"{'case':<20} {'base ms':>10} {'head ms':>10} {'change':>9}")
    for case in common:
        change = (head[case] - base[case]) / base[case] * 100.0
        mark = ""
        if change > args.threshold:
            mark = "  REGRESSION"
            failed = True
        print(f"{case:<20} {base[case]:>10.2f} {head[case]:>10.2f} "
              f"{change:>+8.1f}%{mark}")
    for case in sorted(base.keys() ^ head.keys()):
        print(f"{case:<20} (present in one run only, skipped)")

    if failed:
        print(f"\nFAIL: a case regressed by more than {args.threshold:g}%")
        return 1
    print(f"\nOK: no case regressed by more than {args.threshold:g}%")
    return 0


if __name__ == "__main__":
    sys.exit(main())
