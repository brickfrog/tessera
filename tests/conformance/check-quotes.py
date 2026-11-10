#!/usr/bin/env python3
"""Check that every "quote" in the corpus is in the page its case cites.

A dev tool, not part of the test suite: it fetches the cited pages (the tests never use the network).

    python3 tests/conformance/check-quotes.py [--cache DIR] [--offline] [PATH ...]

Pages are kept in the cache directory (default: a folder under the system temp directory) so a second run
is quick; --offline uses only what is there. PATH limits the check to cases under those folders or files.
Exit code 1 if a quote isn't found or a page can't be fetched. Comparison ignores case, the kind of
quotes and dashes, and all whitespace, because pages wrap and typeset text differently.
"""
import argparse
import hashlib
import html
import json
import os
import re
import sys
import tempfile
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))


def strip_comments(text):
    """JSON with // and /* */ comments and trailing commas, as the harness accepts."""
    out, i, n, in_string = [], 0, len(text), False
    while i < n:
        c = text[i]
        if in_string:
            out.append(c)
            if c == "\\" and i + 1 < n:
                out.append(text[i + 1])
                i += 1
            elif c == '"':
                in_string = False
        elif c == '"':
            in_string = True
            out.append(c)
        elif text.startswith("//", i):
            while i < n and text[i] != "\n":
                i += 1
            continue
        elif text.startswith("/*", i):
            i = text.find("*/", i + 2)
            i = n if i < 0 else i + 2
            continue
        else:
            out.append(c)
        i += 1
    return re.sub(r",(\s*[}\]])", r"\1", "".join(out))


def page_text(raw):
    raw = re.sub(r"(?is)<(script|style)[^>]*>.*?</\1>", " ", raw)
    return html.unescape(re.sub(r"<[^>]+>", " ", raw))


def normalize(text):
    text = text.lower()
    for a, b in (("‘", "'"), ("’", "'"), ("“", '"'), ("”", '"'),
                 ("–", "-"), ("—", "-"), ("−", "-"), (" ", " ")):
        text = text.replace(a, b)
    return re.sub(r"\s+", "", text)


def fetch(url, cache, offline):
    path = os.path.join(cache, hashlib.sha256(url.split("#")[0].encode()).hexdigest() + ".html")
    if os.path.exists(path):
        with open(path, encoding="utf-8", errors="replace") as f:
            return f.read()
    if offline:
        raise RuntimeError("not in the cache")
    request = urllib.request.Request(url.split("#")[0], headers={"User-Agent": "Mozilla/5.0 (corpus quote check)"})
    with urllib.request.urlopen(request, timeout=40) as response:
        raw = response.read().decode("utf-8", errors="replace")
    os.makedirs(cache, exist_ok=True)
    with open(path, "w", encoding="utf-8") as f:
        f.write(raw)
    return raw


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--cache", default=os.path.join(tempfile.gettempdir(), "tessera-conformance-pages"))
    parser.add_argument("--offline", action="store_true")
    parser.add_argument("paths", nargs="*")
    args = parser.parse_args()

    roots = [os.path.abspath(p) for p in args.paths] or [HERE]
    files = []
    for root in roots:
        if os.path.isfile(root):
            files.append(root)
        for folder, _, names in os.walk(root):
            if os.path.basename(folder) == "models" and os.path.dirname(folder) == HERE:
                continue
            files += [os.path.join(folder, n) for n in names if n.endswith(".json")]

    pages, bad, quotes, unquoted = {}, 0, 0, 0
    for path in sorted(set(files)):
        if os.path.dirname(path) == os.path.join(HERE, "models"):
            continue
        name = os.path.relpath(path, HERE)
        try:
            with open(path, encoding="utf-8") as f:
                case = json.loads(strip_comments(f.read()))
        except Exception as e:  # noqa: BLE001
            print(f"{name}: not JSON: {e}")
            bad += 1
            continue
        sources = case.get("source") or []
        if isinstance(sources, dict):
            sources = [sources]
        for source in sources:
            url, quote = source.get("url", ""), source.get("quote")
            if url not in pages:
                try:
                    pages[url] = normalize(page_text(fetch(url, args.cache, args.offline)))
                except Exception as e:  # noqa: BLE001
                    pages[url] = None
                    print(f"{name}: can't fetch {url}: {e}")
                    bad += 1
            if not quote:
                unquoted += 1
                continue
            quotes += 1
            if pages[url] is not None and normalize(quote) not in pages[url]:
                print(f"{name}: quote not found in {url}\n    {quote}")
                bad += 1

    print(f"{quotes} quotes checked in {len(pages)} pages, {unquoted} sources without a quote, {bad} problems")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
