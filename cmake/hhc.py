#!/usr/bin/env python3
# not upstream: HTML Help compiler counterpart; the .chm it writes is a zip of the project's pages, which Orbiter's help viewer reads
# usage: hhc.py project.hhp   (writes the "Compiled file" of the project, default <project>.chm, and always exits 0 like hhc_fix.bat)
import os, re, sys, zipfile

REF = re.compile(r'''(?:href|src|background)\s*=\s*["']?([^"'\s>]+)|url\(\s*["']?([^"')\s]+)|<param\s+name\s*=\s*"Local"\s+value\s*=\s*"([^"]+)"''', re.I)
PAGES = ('.htm', '.html', '.css', '.hhc', '.hhk', '.js')

def find(base, rel):
    # file lookup that ignores case, as Windows did when the projects were written
    rel = rel.replace('\\', '/')
    path = base
    for part in [p for p in rel.split('/') if p not in ('', '.')]:
        if part == '..':
            path = os.path.dirname(path)
            continue
        cand = os.path.join(path, part)
        if not os.path.exists(cand):
            try:
                low = {n.lower(): n for n in os.listdir(path)}
            except OSError:
                return None
            if part.lower() not in low:
                return None
            cand = os.path.join(path, low[part.lower()])
        path = cand
    return path if os.path.isfile(path) else None

def options(hhp):
    opt, files, sect = {}, [], None
    for line in open(hhp, encoding='latin-1'):
        line = line.strip()
        if line.startswith('[') and line.endswith(']'):
            sect = line[1:-1].upper()
        elif sect == 'OPTIONS' and '=' in line:
            k, v = line.split('=', 1)
            opt[k.strip().lower()] = v.strip()
        elif sect == 'FILES' and line:
            files.append(line)
    return opt, files

def main(argv):
    if len(argv) < 2:
        sys.stderr.write('usage: hhc.py project.hhp\n')
        return 0
    hhp = os.path.abspath(argv[1])
    root = os.path.dirname(hhp)
    opt, files = options(hhp)
    out = opt.get('compiled file') or os.path.splitext(os.path.basename(hhp))[0] + '.chm'
    out = os.path.join(root, out.replace('\\', '/'))
    todo = [hhp] + [f for f in (find(root, n) for n in files + [opt.get('contents file', ''), opt.get('index file', ''),
                                                                   opt.get('default topic', '')] if n) if f]
    seen = set()
    while todo:
        f = os.path.normpath(todo.pop())
        if f in seen or not f.startswith(root + os.sep):
            continue
        seen.add(f)
        if f.lower().endswith(PAGES):
            text = open(f, encoding='latin-1').read()
            for m in REF.finditer(text):
                ref = next(g for g in m.groups() if g)
                if re.match(r'^[a-z][a-z0-9+.-]*:', ref, re.I) or ref.startswith('#'):
                    continue  # web links, other help files, anchors
                ref = ref.split('#')[0].split('?')[0]
                g = find(os.path.dirname(f), ref) if ref else None
                if g:
                    todo.append(g)
    with zipfile.ZipFile(out, 'w', zipfile.ZIP_DEFLATED) as z:
        for f in sorted(seen):
            z.write(f, os.path.relpath(f, root).replace(os.sep, '/'))
    print('hhc.py: %s, %d files' % (os.path.basename(out), len(seen)))
    return 0

if __name__ == '__main__':
    try:
        main(sys.argv)
    except Exception as e:
        sys.stderr.write('hhc.py: %s\n' % e)
    sys.exit(0)
