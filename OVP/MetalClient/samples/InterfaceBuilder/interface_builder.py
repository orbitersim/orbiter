#!/usr/bin/env python3
# Generates the end-user header gcCoreAPI.h from gcCore.h and refreshes the
# method binder table in gcCore.cpp.
# Usage: interface_builder.py <gcCore.h> <gcCoreAPI.h output> <gcCore.cpp>

import sys


def parse_line(a):
    wb = (' ', '\t')
    dl = (',', ')', ' ', '\t', '(')
    tm = ('*', '&')
    c = ''
    a += ' '
    for i in range(len(a) - 1):
        ch = ' ' if a[i] == '\t' else a[i]
        if ch in wb and a[i + 1] in dl:
            continue
        c += ch
    s = list(c)
    for i in range(len(s) - 1):
        if s[i] == ' ' and s[i + 1] in tm:
            s[i], s[i + 1] = s[i + 1], ' '
    out = ''
    for i, ch in enumerate(s):
        if i > 0 and ch == ' ' and s[i - 1] == ' ':
            continue
        out += ch
    return out


def collect_methods(lines):
    methods = []
    interface = parse = False
    brackets = 0
    for line in lines:
        if 'INTERFACE_BUILDER' in line:
            interface = True
        if interface:
            brackets += line.count('{') - line.count('}')
            if brackets > 0:
                parse = True
        if parse and brackets == 0:
            parse = interface = False
        if parse and 'gc_interface' in line:
            whs = ''
            for ch in line:
                if ch in (' ', '\t'):
                    whs += ch
                else:
                    break
            line2 = line.replace('gc_interface', '')
            temp = ''
            skip = False
            for ch in line2:
                if ch == ';':
                    break
                if ch == '=':
                    skip = True
                if skip and ch in (',', ')'):
                    skip = False
                if not skip:
                    temp += ch
            temp = parse_line(temp.strip()).strip()
            p = temp.index('(')
            par = temp[p:]
            cmd = temp[:p].split(' ')
            ret, fnc = ('error', 'error')
            if len(cmd) == 2:
                ret, fnc = cmd
            methods.append({'whs': whs, 'par': par, 'ret': ret, 'fnc': fnc})
    return methods


def write_header(lines, methods, out):
    out.write('\n')
    out.write('// WARNING ===============================================================================\n')
    out.write('// This is computer generated file. Do not modify. Make modifications to gcCore.h instead.\n')
    out.write('// WARNING ===============================================================================\n')
    out.write('\n')
    interface = parse = False
    brackets = 0
    for line in lines:
        if 'INTERFACE_BUILDER' in line:
            interface = True
        if '#define' in line and 'fnc_typedefs' in line:
            for m in methods:
                out.write('\t%s (__cdecl * p%s)%s;\n' % (m['ret'], m['fnc'], m['par']))
            continue
        if '#define' in line and 'fnc_binder' in line:
            for m in methods:
                out.write('\t\tpBindCoreMethod((void**)&p%s, "%s");\n' % (m['fnc'], m['fnc']))
            continue
        if interface:
            brackets += line.count('{') - line.count('}')
            if brackets > 0:
                parse = True
        if parse and brackets == 0:
            parse = interface = False
        if parse and 'gc_interface' in line:
            line = line.replace('gc_interface ', '').replace(';', '')
            out.write(line + '\n')
            for m in methods:
                if m['fnc'] in line:
                    out.write(m['whs'] + '{\n')
                    fnc = m['whs'] + '\treturn p' + m['fnc']
                    if m['par'] not in ('()', '(void)', '( )'):
                        fnc += '('
                        q = m['par']
                        parts = q.split(',')
                        for i, pair in enumerate(parts):
                            tokens = pair.strip().split(' ')
                            fnc += tokens[-1]
                            if i != len(parts) - 1:
                                fnc += ', '
                    else:
                        fnc += m['par']
                    out.write(fnc + ';\n')
                    out.write(m['whs'] + '}\n')
                    break
        else:
            out.write(line + '\n')


def rewrite_cpp(path, methods):
    with open(path, encoding='latin-1') as f:
        lines = f.read().split('\n')
    if lines and lines[-1] == '':
        lines.pop()
    out = []
    i = 0
    while i < len(lines):
        line = lines[i]
        i += 1
        if '#define' in line and 'binder_start' in line:
            out.append(line)
            for m in methods:
                out.append('\tif (strcmp(name,"%s")==0) *ppFnc = (void *)&gcCore2::%s;' % (m['fnc'], m['fnc']))
            while i < len(lines):
                line2 = lines[i]
                i += 1
                if '#define' in line2 and 'binder_end' in line2:
                    out.append(line2)
                    break
        else:
            out.append(line)
    new = '\n'.join(out) + '\n'
    with open(path, encoding='latin-1') as f:
        old = f.read()
    if new != old:
        with open(path, 'w', encoding='latin-1') as f:
            f.write(new)


def main():
    if len(sys.argv) != 4:
        sys.exit('usage: interface_builder.py <gcCore.h> <gcCoreAPI.h> <gcCore.cpp>')
    with open(sys.argv[1], encoding='latin-1') as f:
        lines = f.read().split('\n')
    if lines and lines[-1] == '':
        lines.pop()
    methods = collect_methods(lines)
    with open(sys.argv[2], 'w', encoding='latin-1') as out:
        write_header(lines, methods, out)
    rewrite_cpp(sys.argv[3], methods)


if __name__ == '__main__':
    main()
