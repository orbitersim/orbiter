#!/usr/bin/env python3
# not upstream: InterfaceBuilder.exe (Program.cs) as Python, header part: gcCore.h -> gcCoreAPI.h (the gcCore.cpp rewrite left out: the ported gcCore.cpp keeps its binder code)
import sys

def parse_line(a):
    wb = (' ', '\t'); dl = (',', ')', ' ', '\t', '(')
    c = ''
    a += ' '
    for i in range(len(a) - 1):
        ch = ' ' if a[i] == '\t' else a[i]
        if ch in wb and a[i + 1] in dl: continue
        c += ch
    s = list(c)
    for i in range(len(s) - 1):
        if s[i] == ' ' and s[i + 1] in ('*', '&'):
            s[i], s[i + 1] = s[i + 1], ' '
    out = ''
    for i in range(len(s)):
        if i > 0 and s[i] == ' ' and s[i - 1] == ' ': continue
        out += s[i]
    return out

def main(hin, hout):
    lines = open(hin, encoding='utf-8', errors='surrogateescape').read().split('\n')
    if lines and lines[-1] == '': lines.pop()
    methods = []
    binf = bparse = False; br = 0
    for line in lines:
        if 'INTERFACE_BUILDER' in line: binf = True
        if binf:
            br += line.count('{') - line.count('}')
            if br > 0: bparse = True
        if bparse and br == 0: bparse = binf = False
        if bparse and 'gc_interface' in line:
            whs = line[:len(line) - len(line.lstrip(' \t'))]
            l2 = line.replace('gc_interface', '')
            tmp = ''; skip = False
            for ch in l2:
                if ch == ';': break
                if ch == '=': skip = True
                if skip and ch in (',', ')'): skip = False
                if not skip: tmp += ch
            tmp = parse_line(tmp.strip()).strip()
            p = tmp.index('(')
            par = tmp[p:]; cmd = tmp[:p].split(' ')
            ret, fnc = (cmd[0], cmd[1]) if len(cmd) == 2 else ('error', 'error')
            methods.append((whs, par, ret, fnc))
    out = ['', '// WARNING ===============================================================================',
           '// This is computer generated file. Do not modify. Make modifications to gcCore.h instead.',
           '// WARNING ===============================================================================', '']
    binf = bparse = False; br = 0
    for line in lines:
        if 'INTERFACE_BUILDER' in line: binf = True
        if '#define' in line and 'fnc_typedefs' in line:
            for whs, par, ret, fnc in methods: out.append('\t' + ret + ' (* p' + fnc + ')' + par + ';')
            continue
        if '#define' in line and 'fnc_binder' in line:
            for whs, par, ret, fnc in methods: out.append('\t\tpBindCoreMethod((void**)&p' + fnc + ', "' + fnc + '");')
            continue
        if binf:
            br += line.count('{') - line.count('}')
            if br > 0: bparse = True
        if bparse and br == 0: bparse = binf = False
        if bparse and 'gc_interface' in line:
            line = line.replace('gc_interface ', '').replace(';', '')
            out.append(line)
            for whs, par, ret, fnc in methods:
                if fnc in line:
                    out.append(whs + '{')
                    f = whs + '\treturn p' + fnc
                    if par not in ('()', '(void)', '( )'):
                        f += '('
                        lst = par.split(',')
                        for k, pair in enumerate(lst):
                            f += pair.strip().split(' ')[-1]
                            if k < len(lst) - 1: f += ', '
                    else:
                        f += par
                    out.append(f + ';')
                    out.append(whs + '}')
                    break
        else:
            out.append(line)
    open(hout, 'w', encoding='utf-8', errors='surrogateescape').write('\n'.join(out) + '\n')

if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
