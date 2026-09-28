#!/usr/bin/env python3
# not upstream: compiles a Windows .rc resource script (dialogs, menus, bitmaps, icons, strings) into C++ resource tables
# usage: rc2cpp.py input.rc output.cpp [--symbol NAME] [--exe] [--depfile FILE] [-I dir]...
import os, re, sys

WIN = {
    'WS_OVERLAPPED': 0x0, 'WS_POPUP': 0x80000000, 'WS_CHILD': 0x40000000, 'WS_CHILDWINDOW': 0x40000000,
    'WS_MINIMIZE': 0x20000000, 'WS_VISIBLE': 0x10000000, 'WS_DISABLED': 0x08000000, 'WS_CLIPSIBLINGS': 0x04000000,
    'WS_CLIPCHILDREN': 0x02000000, 'WS_MAXIMIZE': 0x01000000, 'WS_CAPTION': 0x00C00000, 'WS_BORDER': 0x00800000,
    'WS_DLGFRAME': 0x00400000, 'WS_VSCROLL': 0x00200000, 'WS_HSCROLL': 0x00100000, 'WS_SYSMENU': 0x00080000,
    'WS_THICKFRAME': 0x00040000, 'WS_GROUP': 0x00020000, 'WS_TABSTOP': 0x00010000, 'WS_MINIMIZEBOX': 0x00020000,
    'WS_MAXIMIZEBOX': 0x00010000, 'WS_OVERLAPPEDWINDOW': 0x00CF0000, 'WS_POPUPWINDOW': 0x80880000,
    'WS_EX_DLGMODALFRAME': 0x1, 'WS_EX_NOPARENTNOTIFY': 0x4, 'WS_EX_TOPMOST': 0x8, 'WS_EX_ACCEPTFILES': 0x10,
    'WS_EX_TRANSPARENT': 0x20, 'WS_EX_MDICHILD': 0x40, 'WS_EX_TOOLWINDOW': 0x80, 'WS_EX_WINDOWEDGE': 0x100,
    'WS_EX_CLIENTEDGE': 0x200, 'WS_EX_CONTEXTHELP': 0x400, 'WS_EX_RIGHT': 0x1000, 'WS_EX_LEFT': 0x0,
    'WS_EX_RTLREADING': 0x2000, 'WS_EX_LEFTSCROLLBAR': 0x4000, 'WS_EX_CONTROLPARENT': 0x10000,
    'WS_EX_STATICEDGE': 0x20000, 'WS_EX_APPWINDOW': 0x40000, 'WS_EX_LAYERED': 0x80000, 'WS_EX_NOACTIVATE': 0x08000000,
    'DS_ABSALIGN': 0x1, 'DS_SYSMODAL': 0x2, 'DS_LOCALEDIT': 0x20, 'DS_SETFONT': 0x40, 'DS_MODALFRAME': 0x80,
    'DS_NOIDLEMSG': 0x100, 'DS_SETFOREGROUND': 0x200, 'DS_3DLOOK': 0x4, 'DS_FIXEDSYS': 0x8, 'DS_NOFAILCREATE': 0x10,
    'DS_CONTROL': 0x400, 'DS_CENTER': 0x800, 'DS_CENTERMOUSE': 0x1000, 'DS_CONTEXTHELP': 0x2000, 'DS_SHELLFONT': 0x48,
    'SS_LEFT': 0x0, 'SS_CENTER': 0x1, 'SS_RIGHT': 0x2, 'SS_ICON': 0x3, 'SS_BLACKRECT': 0x4, 'SS_GRAYRECT': 0x5,
    'SS_WHITERECT': 0x6, 'SS_BLACKFRAME': 0x7, 'SS_GRAYFRAME': 0x8, 'SS_WHITEFRAME': 0x9, 'SS_SIMPLE': 0xB,
    'SS_LEFTNOWORDWRAP': 0xC, 'SS_OWNERDRAW': 0xD, 'SS_BITMAP': 0xE, 'SS_ETCHEDHORZ': 0x10, 'SS_ETCHEDVERT': 0x11,
    'SS_ETCHEDFRAME': 0x12, 'SS_TYPEMASK': 0x1F, 'SS_REALSIZECONTROL': 0x40, 'SS_NOPREFIX': 0x80, 'SS_NOTIFY': 0x100,
    'SS_CENTERIMAGE': 0x200, 'SS_RIGHTJUST': 0x400, 'SS_REALSIZEIMAGE': 0x800, 'SS_SUNKEN': 0x1000,
    'SS_ENDELLIPSIS': 0x4000, 'SS_PATHELLIPSIS': 0x8000, 'SS_WORDELLIPSIS': 0xC000,
    'BS_PUSHBUTTON': 0x0, 'BS_DEFPUSHBUTTON': 0x1, 'BS_CHECKBOX': 0x2, 'BS_AUTOCHECKBOX': 0x3, 'BS_RADIOBUTTON': 0x4,
    'BS_3STATE': 0x5, 'BS_AUTO3STATE': 0x6, 'BS_GROUPBOX': 0x7, 'BS_USERBUTTON': 0x8, 'BS_AUTORADIOBUTTON': 0x9,
    'BS_OWNERDRAW': 0xB, 'BS_TYPEMASK': 0xF, 'BS_LEFTTEXT': 0x20, 'BS_TEXT': 0x0, 'BS_ICON': 0x40, 'BS_BITMAP': 0x80,
    'BS_LEFT': 0x100, 'BS_RIGHT': 0x200, 'BS_CENTER': 0x300, 'BS_TOP': 0x400, 'BS_BOTTOM': 0x800, 'BS_VCENTER': 0xC00,
    'BS_PUSHLIKE': 0x1000, 'BS_MULTILINE': 0x2000, 'BS_NOTIFY': 0x4000, 'BS_FLAT': 0x8000,
    'ES_LEFT': 0x0, 'ES_CENTER': 0x1, 'ES_RIGHT': 0x2, 'ES_MULTILINE': 0x4, 'ES_UPPERCASE': 0x8, 'ES_LOWERCASE': 0x10,
    'ES_PASSWORD': 0x20, 'ES_AUTOVSCROLL': 0x40, 'ES_AUTOHSCROLL': 0x80, 'ES_NOHIDESEL': 0x100, 'ES_OEMCONVERT': 0x400,
    'ES_READONLY': 0x800, 'ES_WANTRETURN': 0x1000, 'ES_NUMBER': 0x2000,
    'CBS_SIMPLE': 0x1, 'CBS_DROPDOWN': 0x2, 'CBS_DROPDOWNLIST': 0x3, 'CBS_OWNERDRAWFIXED': 0x10,
    'CBS_OWNERDRAWVARIABLE': 0x20, 'CBS_AUTOHSCROLL': 0x40, 'CBS_OEMCONVERT': 0x80, 'CBS_SORT': 0x100,
    'CBS_HASSTRINGS': 0x200, 'CBS_NOINTEGRALHEIGHT': 0x400, 'CBS_DISABLENOSCROLL': 0x800, 'CBS_UPPERCASE': 0x2000,
    'CBS_LOWERCASE': 0x4000,
    'LBS_NOTIFY': 0x1, 'LBS_SORT': 0x2, 'LBS_NOREDRAW': 0x4, 'LBS_MULTIPLESEL': 0x8, 'LBS_OWNERDRAWFIXED': 0x10,
    'LBS_OWNERDRAWVARIABLE': 0x20, 'LBS_HASSTRINGS': 0x40, 'LBS_USETABSTOPS': 0x80, 'LBS_NOINTEGRALHEIGHT': 0x100,
    'LBS_MULTICOLUMN': 0x200, 'LBS_WANTKEYBOARDINPUT': 0x400, 'LBS_EXTENDEDSEL': 0x800,
    'LBS_DISABLENOSCROLL': 0x1000, 'LBS_NODATA': 0x2000, 'LBS_NOSEL': 0x4000, 'LBS_STANDARD': 0xA00003,
    'SBS_HORZ': 0x0, 'SBS_VERT': 0x1,
    'TVS_HASBUTTONS': 0x1, 'TVS_HASLINES': 0x2, 'TVS_LINESATROOT': 0x4, 'TVS_EDITLABELS': 0x8,
    'TVS_DISABLEDRAGDROP': 0x10, 'TVS_SHOWSELALWAYS': 0x20, 'TVS_RTLREADING': 0x40, 'TVS_NOTOOLTIPS': 0x80,
    'TVS_CHECKBOXES': 0x100, 'TVS_TRACKSELECT': 0x200, 'TVS_SINGLEEXPAND': 0x400, 'TVS_INFOTIP': 0x800,
    'TVS_FULLROWSELECT': 0x1000, 'TVS_NOSCROLL': 0x2000, 'TVS_NONEVENHEIGHT': 0x4000, 'TVS_NOHSCROLL': 0x8000,
    'TBS_AUTOTICKS': 0x1, 'TBS_VERT': 0x2, 'TBS_HORZ': 0x0, 'TBS_TOP': 0x4, 'TBS_BOTTOM': 0x0, 'TBS_LEFT': 0x4,
    'TBS_RIGHT': 0x0, 'TBS_BOTH': 0x8, 'TBS_NOTICKS': 0x10, 'TBS_ENABLESELRANGE': 0x20, 'TBS_FIXEDLENGTH': 0x40,
    'TBS_NOTHUMB': 0x80, 'TBS_TOOLTIPS': 0x100,
    'UDS_WRAP': 0x1, 'UDS_SETBUDDYINT': 0x2, 'UDS_ALIGNRIGHT': 0x4, 'UDS_ALIGNLEFT': 0x8, 'UDS_AUTOBUDDY': 0x10,
    'UDS_ARROWKEYS': 0x20, 'UDS_HORZ': 0x40, 'UDS_NOTHOUSANDS': 0x80, 'UDS_HOTTRACK': 0x100,
    'TCS_SCROLLOPPOSITE': 0x1, 'TCS_BOTTOM': 0x2, 'TCS_RIGHT': 0x2, 'TCS_MULTISELECT': 0x4, 'TCS_FLATBUTTONS': 0x8,
    'TCS_FORCEICONLEFT': 0x10, 'TCS_FORCELABELLEFT': 0x20, 'TCS_HOTTRACK': 0x40, 'TCS_VERTICAL': 0x80,
    'TCS_TABS': 0x0, 'TCS_BUTTONS': 0x100, 'TCS_SINGLELINE': 0x0, 'TCS_MULTILINE': 0x200, 'TCS_RIGHTJUSTIFY': 0x0,
    'TCS_FIXEDWIDTH': 0x400, 'TCS_RAGGEDRIGHT': 0x800, 'TCS_FOCUSONBUTTONDOWN': 0x1000, 'TCS_OWNERDRAWFIXED': 0x2000,
    'TCS_TOOLTIPS': 0x4000, 'TCS_FOCUSNEVER': 0x8000,
    'PBS_SMOOTH': 0x1, 'PBS_VERTICAL': 0x4,
    'IDOK': 1, 'IDCANCEL': 2, 'IDABORT': 3, 'IDRETRY': 4, 'IDIGNORE': 5, 'IDYES': 6, 'IDNO': 7, 'IDCLOSE': 8,
    'IDHELP': 9, 'IDC_STATIC': -1,
    'MFT_STRING': 0x0, 'MFT_BITMAP': 0x4, 'MFT_MENUBARBREAK': 0x20, 'MFT_MENUBREAK': 0x40, 'MFT_OWNERDRAW': 0x100,
    'MFT_RADIOCHECK': 0x200, 'MFT_SEPARATOR': 0x800, 'MFT_RIGHTORDER': 0x2000, 'MFT_RIGHTJUSTIFY': 0x4000,
    'MFS_ENABLED': 0x0, 'MFS_UNCHECKED': 0x0, 'MFS_UNHILITE': 0x0, 'MFS_GRAYED': 0x3, 'MFS_DISABLED': 0x3,
    'MFS_CHECKED': 0x8, 'MFS_HILITE': 0x80, 'MFS_DEFAULT': 0x1000,
}
# MENU item options -> MF_ flags
MENUOPT = {'GRAYED': 0x1, 'INACTIVE': 0x2, 'CHECKED': 0x8, 'MENUBARBREAK': 0x20, 'MENUBREAK': 0x40, 'HELP': 0x4000}
CLASSMACRO = {'TRACKBAR_CLASS': 'msctls_trackbar32', 'RICHEDIT_CLASS': 'RichEdit20A', 'PROGRESS_CLASS': 'msctls_progress32',
              'UPDOWN_CLASS': 'msctls_updown32', 'WC_TREEVIEW': 'SysTreeView32', 'WC_TABCONTROL': 'SysTabControl32',
              'WC_LISTVIEW': 'SysListView32', 'MSFTEDIT_CLASS': 'RICHEDIT50W'}
SKIPINC = {'afxres.h', 'winres.h', 'windows.h', 'commctrl.h', 'richedit.h', 'winresrc.h', 'winver.h', 'ntdef.h',
           'winuser.h', 'afxres.rc', 'dlgs.h', 'prsht.h'}
# statement -> (has text, default style, kind)
STATEMENTS = {
    'LTEXT': (True, 0x0 | 0x20000, 'STATIC'), 'RTEXT': (True, 0x2 | 0x20000, 'STATIC'), 'CTEXT': (True, 0x1 | 0x20000, 'STATIC'),
    'PUSHBUTTON': (True, 0x0 | 0x10000, 'BUTTON'), 'DEFPUSHBUTTON': (True, 0x1 | 0x10000, 'BUTTON'),
    'GROUPBOX': (True, 0x7, 'BUTTON'), 'CHECKBOX': (True, 0x2 | 0x10000, 'BUTTON'),
    'AUTOCHECKBOX': (True, 0x3 | 0x10000, 'BUTTON'), 'RADIOBUTTON': (True, 0x4, 'BUTTON'),
    'AUTORADIOBUTTON': (True, 0x9, 'BUTTON'), 'STATE3': (True, 0x5 | 0x10000, 'BUTTON'),
    'AUTO3STATE': (True, 0x6 | 0x10000, 'BUTTON'), 'EDITTEXT': (False, 0x800000 | 0x10000, 'EDIT'),
    'COMBOBOX': (False, 0x1 | 0x10000, 'COMBOBOX'), 'LISTBOX': (False, 0x1 | 0x800000, 'LISTBOX'),
    'SCROLLBAR': (False, 0x0, 'SCROLLBAR'), 'ICON': (True, 0x3, 'STATIC'),
}

class RcError(Exception):
    pass

def c_escape(s):
    out = []
    for ch in s.encode('utf-8'):
        c = chr(ch)
        if c == '\\': out.append('\\\\')
        elif c == '"': out.append('\\"')
        elif c == '\n': out.append('\\n')
        elif c == '\r': out.append('\\r')
        elif c == '\t': out.append('\\t')
        elif 32 <= ch < 127: out.append(c)
        else: out.append('\\%03o' % ch)
    return '"' + ''.join(out) + '"'

# preprocessor: #include (header defines), #define, #if/#ifdef/#ifndef/#elif/#else/#endif

def resolve(base, rel):
    # case-insensitive, component-wise lookup (the sources come from Windows)
    cur = base
    for part in [x for x in rel.split('/') if x and x != '.']:
        if part == '..':
            cur = os.path.dirname(cur); continue
        p = os.path.join(cur, part)
        if os.path.exists(p):
            cur = p; continue
        if not os.path.isdir(cur):
            return None
        hit = [f for f in os.listdir(cur) if f.lower() == part.lower()]
        if not hit:
            return None
        cur = os.path.join(cur, hit[0])
    return cur

class Preproc:
    def __init__(self, incdirs):
        self.defs = {'_WIN32': '1', 'RC_INVOKED': '1'}
        self.incdirs = incdirs

    def value(self, name):
        seen = set()
        v = name
        while v in self.defs and v not in seen:
            seen.add(v)
            v = self.defs[v].strip()
        return v

    def cond(self, expr):
        expr = re.sub(r'defined\s*\(\s*(\w+)\s*\)', lambda m: '1' if m.group(1) in self.defs else '0', expr)
        expr = re.sub(r'defined\s+(\w+)', lambda m: '1' if m.group(1) in self.defs else '0', expr)
        def ident(m):
            v = self.value(m.group(0))
            return v if re.fullmatch(r'[-+()0-9xXa-fA-FuUlL|&<>=! ]+', v) else '0'
        expr = re.sub(r'\b[A-Za-z_]\w*\b', ident, expr)
        expr = expr.replace('&&', ' and ').replace('||', ' or ')
        expr = re.sub(r'!(?!=)', ' not ', expr)
        expr = re.sub(r'(\d)[uUlL]+\b', r'\1', expr)
        try:
            return bool(eval(expr, {}, {}))
        except Exception:
            return False

    def find(self, name, curdir):
        name = name.replace('\\', '/')
        for d in [curdir] + self.incdirs:
            p = resolve(d, name)
            if p and os.path.isfile(p):
                return p
        return None

    def run(self, path, deps):
        text = open(path, 'rb').read().decode('latin-1').replace('\r\n', '\n').replace('\r', '\n')
        text = re.sub(r'\\\n', '', text)
        out = []
        stack = []  # (active, taken)
        active = True
        curdir = os.path.dirname(os.path.abspath(path))
        for line in text.split('\n'):
            s = line.strip()
            m = re.match(r'#\s*(\w+)\s*(.*)$', s)
            if not m:
                out.append(line if active else '')
                continue
            d, rest = m.group(1), m.group(2)
            rest = re.sub(r'//.*$', '', rest).strip()
            if d in ('if', 'ifdef', 'ifndef'):
                if d == 'if': c = self.cond(rest)
                elif d == 'ifdef': c = rest.split()[0] in self.defs
                else: c = rest.split()[0] not in self.defs
                stack.append((active, c))
                active = active and c
            elif d == 'elif':
                parent, taken = stack[-1]
                c = (not taken) and self.cond(rest)
                stack[-1] = (parent, taken or c)
                active = parent and c
            elif d == 'else':
                parent, taken = stack[-1]
                stack[-1] = (parent, True)
                active = parent and not taken
            elif d == 'endif':
                active, _ = stack.pop()
            elif not active:
                pass
            elif d == 'define':
                mm = re.match(r'(\w+)(\([^)]*\))?\s*(.*)$', rest)
                if mm and not mm.group(2):
                    self.defs[mm.group(1)] = re.sub(r'/\*.*?\*/', '', mm.group(3)).strip() or '1'
            elif d == 'undef':
                self.defs.pop(rest.split()[0], None)
            elif d == 'include':
                name = rest.strip('"<> ')
                if os.path.basename(name).lower() in SKIPINC:
                    continue
                p = self.find(name, curdir)
                if p:
                    deps.append(p)
                    sub = self.run(p, deps)
                    if not p.lower().endswith('.h'):
                        out.append(sub)
            out.append('')
        return '\n'.join(out)

# tokenizer and expression evaluation

TOKRE = re.compile(r'\s*(?:(//[^\n]*)|(/\*.*?\*/)|("(?:[^"]|"")*")|(0[xX][0-9a-fA-F]+[uUlL]*|\d+[uUlL]*)|([A-Za-z_][\w.]*)|(.))', re.S)

def tokenize(text):
    toks = []
    pos = 0
    n = len(text)
    while pos < n:
        m = TOKRE.match(text, pos)
        if not m:
            break
        pos = m.end()
        if m.group(1) or m.group(2):
            continue
        if m.group(3) is not None:
            toks.append(('str', m.group(3)))
        elif m.group(4) is not None:
            toks.append(('num', int(re.sub(r'[uUlL]+$', '', m.group(4)), 0)))
        elif m.group(5) is not None:
            toks.append(('id', m.group(5)))
        elif m.group(6) is not None and not m.group(6).isspace():
            toks.append(('op', m.group(6)))
    return toks

def rc_string(tok):
    s = tok[1][1:-1].replace('""', '"')
    s = s.encode('latin-1').decode('cp1252', errors='replace')
    out, i = [], 0
    while i < len(s):
        c = s[i]
        if c == '\\' and i+1 < len(s):
            e = s[i+1]
            i += 2
            if e == 'n': out.append('\n')
            elif e == 'r': out.append('\r')
            elif e == 't': out.append('\t')
            elif e == '\\': out.append('\\')
            elif e == '"': out.append('"')
            elif e == 'a': out.append('\t')
            else: out.append('\\' + e)
            continue
        out.append(c)
        i += 1
    return ''.join(out)

class Parser:
    def __init__(self, toks, pp, rcdir):
        self.t = toks
        self.i = 0
        self.pp = pp
        self.rcdir = rcdir
        self.dialogs = []
        self.images = []   # (kind, name, id, path)
        self.strings = []  # (id, text) from STRINGTABLE
        self.data = []     # (type, name, id, path) user-defined resource types with a file (TEXT, IMAGE, RCDATA, ...)
        self.menus = []    # {'id', 'name', 'items': [[id, text, flags, nsub], ...]}

    def peek(self, k=0):
        return self.t[self.i+k] if self.i+k < len(self.t) else ('eof', None)

    def get(self):
        tok = self.peek()
        self.i += 1
        return tok

    def accept(self, kind, val=None):
        tok = self.peek()
        if tok[0] == kind and (val is None or tok[1] == val):
            self.i += 1
            return True
        return False

    def expect_op(self, val):
        tok = self.get()
        if tok != ('op', val):
            raise RcError('expected %r, got %r near token %d' % (val, tok, self.i))

    def ident_value(self, name):
        if name in WIN:
            return WIN[name]
        v = self.pp.value(name)
        if v in WIN:
            return WIN[v]
        if v != name:
            toks = tokenize(v)
            saved = (self.t, self.i)
            self.t, self.i = toks + [('eof', None)], 0
            try:
                val = self.expr()
            finally:
                self.t, self.i = saved
            return val
        raise RcError('unknown symbol %s' % name)

    # expression: term (('|'|'+'|'-') term)* ; term: factor (('*'|'/') factor)* ; factor: num|id|(expr)|-factor|~factor|NOT factor
    def factor(self):
        tok = self.get()
        if tok[0] == 'num': return tok[1]
        if tok == ('op', '('):
            v = self.expr()
            self.expect_op(')')
            return v
        if tok == ('op', '-'): return -self.factor()
        if tok == ('op', '~'): return ~self.factor()
        if tok[0] == 'id':
            if tok[1] == 'NOT':
                return ('NOT', self.factor())
            return self.ident_value(tok[1])
        raise RcError('bad expression token %r' % (tok,))

    def term(self):
        v = self.factor()
        while self.peek() in (('op', '*'), ('op', '/')):
            op = self.get()[1]
            w = self.factor()
            v = v * w if op == '*' else int(v / w)
        return v

    def expr(self, base=0):
        # style expressions: a | b | NOT c ; NOT removes bits from the accumulated value
        v = self.term()
        acc = base
        def apply(acc, v, op):
            if isinstance(v, tuple):
                return acc & ~v[1]
            if op == '|': return acc | v
            if op == '+': return acc + v
            if op == '-': return acc - v
            return v
        acc = apply(acc, v, '|' if base else '=')
        while self.peek() in (('op', '|'), ('op', '+'), ('op', '-')):
            op = self.get()[1]
            acc = apply(acc, self.term(), op)
        return acc

    def num(self):
        return self.expr()

    def text_or_id(self):
        tok = self.peek()
        if tok[0] == 'str':
            self.get()
            return rc_string(tok), None
        v = self.expr()
        return None, v

    def skip_block(self):
        depth = 0
        while True:
            tok = self.get()
            if tok[0] == 'eof':
                return
            if tok in (('id', 'BEGIN'), ('op', '{')):
                depth += 1
            elif tok in (('id', 'END'), ('op', '}')):
                depth -= 1
                if depth == 0:
                    return

    def resource_id(self, tok):
        if tok[0] == 'num':
            return tok[1], None
        name = tok[1]
        try:
            return self.ident_value(name), name
        except RcError:
            return None, name   # string-named resource

    def parse(self):
        while self.peek()[0] != 'eof':
            tok = self.get()
            if tok[0] == 'id' and tok[1] in ('LANGUAGE',):
                self.get(); self.accept('op', ','); self.get()
                continue
            if tok == ('id', 'STRINGTABLE'):
                self.stringtable()
                continue
            if tok[0] not in ('id', 'num'):
                continue
            rtype = self.peek()
            if rtype[0] != 'id':
                continue
            kind = rtype[1]
            if kind in ('DIALOG', 'DIALOGEX'):
                self.get()
                self.dialog(tok, kind == 'DIALOGEX')
            elif kind in ('MENU', 'MENUEX'):
                self.get()
                self.menu(tok, kind == 'MENUEX')
            elif kind in ('BITMAP', 'ICON', 'PNG'):
                self.get()
                while self.peek()[0] == 'id' and self.peek()[1] in ('DISCARDABLE', 'MOVEABLE', 'PURE', 'PRELOAD', 'LOADONCALL', 'FIXED', 'IMPURE'):
                    self.get()
                f = self.get()
                if f[0] == 'str':
                    rid, name = self.resource_id(tok)
                    path = rc_string(f)
                    self.images.append((kind, name, rid, path))
            elif self.peek(1)[0] == 'str' and kind not in ('TEXTINCLUDE', 'DESIGNINFO', 'VERSIONINFO',
                          'ACCELERATORS', 'AFX_DIALOG_LAYOUT', 'TOOLBAR', 'DLGINIT', 'HTML', 'CURSOR', 'FONT'):
                self.get()
                rid, name = self.resource_id(tok)
                self.data.append((kind, name, rid, rc_string(self.get())))
            elif kind in ('TEXTINCLUDE', 'DESIGNINFO', 'VERSIONINFO', 'STRINGTABLE',
                          'ACCELERATORS', 'AFX_DIALOG_LAYOUT', 'RCDATA', 'TOOLBAR', 'DLGINIT', 'HTML'):
                self.get()
                # optional attributes/header up to BEGIN
                while self.peek()[0] != 'eof' and self.peek() not in (('id', 'BEGIN'), ('op', '{')):
                    if self.peek()[0] == 'str' and kind in ('HTML', 'RCDATA'):
                        self.get(); break
                    self.get()
                if self.peek() in (('id', 'BEGIN'), ('op', '{')):
                    self.skip_block()
        return self

    def stringtable(self):
        # STRINGTABLE [attributes] BEGIN id [,] "text" ... END (the STRINGTABLE keyword is already consumed)
        while self.peek()[0] != 'eof' and self.peek() not in (('id', 'BEGIN'), ('op', '{')):
            self.get()
        self.get()
        while True:
            tok = self.peek()
            if tok[0] == 'eof' or tok in (('id', 'END'), ('op', '}')):
                self.get()
                return
            sid = self.num()
            self.accept('op', ',')
            s = self.get()
            if s[0] != 'str':
                raise RcError('bad STRINGTABLE entry %r' % (s,))
            self.strings.append((sid, rc_string(s)))

    def dialog(self, nametok, ex):
        rid, name = self.resource_id(nametok)
        while self.peek()[0] == 'id' and self.peek()[1] in ('DISCARDABLE', 'MOVEABLE', 'PURE', 'PRELOAD', 'LOADONCALL', 'FIXED', 'IMPURE'):
            self.get()
        x = self.num(); self.expect_op(','); y = self.num(); self.expect_op(',')
        cx = self.num(); self.expect_op(','); cy = self.num()
        if self.accept('op', ','):
            self.num()  # help id
        dlg = {'id': rid, 'name': name or str(rid), 'x': x, 'y': y, 'cx': cx, 'cy': cy,
               'style': 0x80000000 | 0x00C00000 | 0x00080000, 'exstyle': 0, 'caption': '',
               'font': '', 'fontsize': 8, 'weight': 400, 'italic': 0, 'ctrls': [], 'menu': -1}
        while True:
            tok = self.peek()
            if tok in (('id', 'BEGIN'), ('op', '{')):
                self.get()
                break
            self.get()
            key = tok[1]
            if key == 'STYLE':
                dlg['style'] = self.expr()
            elif key == 'EXSTYLE':
                dlg['exstyle'] = self.expr()
            elif key == 'CAPTION':
                dlg['caption'] = rc_string(self.get())
            elif key == 'FONT':
                dlg['fontsize'] = self.num(); self.expect_op(',')
                dlg['font'] = rc_string(self.get())
                if self.accept('op', ','):
                    dlg['weight'] = self.num()
                    if self.accept('op', ','):
                        dlg['italic'] = self.num()
                        if self.accept('op', ','):
                            self.num()
            elif key == 'MENU':
                mid, mname = self.resource_id(self.get())
                if mid is None:
                    raise RcError('dialog %s: menu %s referenced by name is not supported' % (dlg['name'], mname))
                dlg['menu'] = mid
            elif key == 'CLASS':
                self.get()
            elif key in ('LANGUAGE', 'CHARACTERISTICS', 'VERSION'):
                self.get()
                if self.accept('op', ','): self.get()
            else:
                raise RcError('unknown dialog statement %r in %s' % (tok, dlg['name']))
        while True:
            tok = self.get()
            if tok in (('id', 'END'), ('op', '}')):
                break
            if tok[0] != 'id':
                raise RcError('bad control statement %r in %s' % (tok, dlg['name']))
            dlg['ctrls'].append(self.control(tok[1], dlg))
        self.dialogs.append(dlg)

    def menu(self, nametok, ex):
        # MENU/MENUEX [attributes] BEGIN items END (the MENU keyword is already consumed)
        rid, name = self.resource_id(nametok)
        while self.peek()[0] != 'eof' and self.peek() not in (('id', 'BEGIN'), ('op', '{')):
            self.get()
        self.get()
        items = []
        self.menu_items(items, ex)
        self.menus.append({'id': rid, 'name': name or str(rid), 'items': items})

    def menu_options(self):
        flags = 0
        while True:
            save = self.i
            self.accept('op', ',')
            t = self.peek()
            if t[0] == 'id' and t[1] in MENUOPT:
                self.get()
                flags |= MENUOPT[t[1]]
            else:
                self.i = save
                return flags

    def menuex_args(self, n):
        # MENUEX: [, id [, type [, state [, helpid]]]], any of them may be empty
        v = [0] * n
        for k in range(n):
            if not self.accept('op', ','):
                break
            if self.peek() != ('op', ',') and self.peek()[0] in ('num', 'id', 'op'):
                if self.peek()[0] == 'id' and self.peek()[1] in ('MENUITEM', 'POPUP', 'BEGIN', 'END'):
                    break
                v[k] = self.num()
        return v

    def menu_items(self, items, ex):
        # items up to END; returns the number of direct items
        n = 0
        while True:
            tok = self.get()
            if tok in (('id', 'END'), ('op', '}')):
                return n
            if tok[0] == 'eof':
                raise RcError('unterminated MENU')
            if tok == ('id', 'MENUITEM'):
                if not ex and self.peek() == ('id', 'SEPARATOR'):
                    self.get()
                    items.append([0, None, 0, 0])
                else:
                    text = rc_string(self.get())
                    if ex:
                        mid, typ, state = self.menuex_args(3)
                        flags = (typ & 0x4060) | (state & 0xB)
                        if typ & 0x800:
                            text = None
                    else:
                        self.accept('op', ',')
                        mid = self.num()
                        flags = self.menu_options()
                    items.append([mid, text, flags, 0])
                n += 1
            elif tok == ('id', 'POPUP'):
                text = rc_string(self.get())
                if ex:
                    mid, typ, state, _ = self.menuex_args(4)
                    flags = (typ & 0x4060) | (state & 0xB)
                else:
                    mid, flags = 0, self.menu_options()
                k = len(items)
                items.append([mid, text, flags | 0x10, 0])
                if self.get() not in (('id', 'BEGIN'), ('op', '{')):
                    raise RcError('POPUP %r without BEGIN' % text)
                items[k][3] = self.menu_items(items, ex)
                n += 1
            else:
                raise RcError('bad menu statement %r' % (tok,))

    def control(self, stmt, dlg):
        base = WIN['WS_CHILD'] | WIN['WS_VISIBLE']
        c = {'stmt': stmt, 'text': None, 'imgid': None, 'cls': None, 'style': 0, 'exstyle': 0}
        if stmt == 'CONTROL':
            c['text'], c['imgid'] = self.text_or_id(); self.expect_op(',')
            c['idname'] = self.peek()[1] if self.peek()[0] == 'id' and self.peek(1) == ('op', ',') else None
            c['id'] = self.num(); self.expect_op(',')
            cls = self.get()
            c['cls'] = rc_string(cls) if cls[0] == 'str' else CLASSMACRO.get(cls[1], cls[1])
            self.expect_op(',')
            c['style'] = self.expr(base); self.expect_op(',')
            c['x'] = self.num(); self.expect_op(','); c['y'] = self.num(); self.expect_op(',')
            c['cx'] = self.num(); self.expect_op(','); c['cy'] = self.num()
            if self.accept('op', ','):
                c['exstyle'] = self.expr()
            return c
        if stmt not in STATEMENTS:
            raise RcError('unknown control %s in %s' % (stmt, dlg['name']))
        hastext, defstyle, _ = STATEMENTS[stmt]
        if hastext:
            c['text'], c['imgid'] = self.text_or_id(); self.expect_op(',')
        c['idname'] = self.peek()[1] if self.peek()[0] == 'id' and self.peek(1) == ('op', ',') else None
        c['id'] = self.num(); self.expect_op(',')
        c['x'] = self.num(); self.expect_op(','); c['y'] = self.num()
        if stmt == 'ICON':
            c['cx'] = c['cy'] = 0
            c['style'] = base | defstyle
            if self.accept('op', ','):
                c['cx'] = self.num(); self.expect_op(','); c['cy'] = self.num()
                if self.accept('op', ','):
                    c['style'] = self.expr(base | defstyle)
                    if self.accept('op', ','):
                        c['exstyle'] = self.expr()
            return c
        self.expect_op(',')
        c['cx'] = self.num(); self.expect_op(','); c['cy'] = self.num()
        c['style'] = base | defstyle
        if self.accept('op', ','):
            c['style'] = self.expr(base | defstyle)
            if self.accept('op', ','):
                c['exstyle'] = self.expr()
        return c

# control classification (kinds must match enum RESKIND in OrbiterResource.h)

def classify(c):
    stmt = c['stmt']
    style = c['style']
    if stmt != 'CONTROL':
        base = STATEMENTS[stmt][2]
        cls = {'STATIC': 'static', 'BUTTON': 'button', 'EDIT': 'edit', 'COMBOBOX': 'combobox',
               'LISTBOX': 'listbox', 'SCROLLBAR': 'scrollbar'}[base]
    else:
        cls = c['cls'].lower()
    if cls == 'static':
        t = style & 0x1F
        if t in (0x0, 0x1, 0x2, 0xB, 0xC): return 'RES_STATIC'
        if t in (0x3, 0xE): return 'RES_STATICIMAGE'
        return 'RES_STATICFRAME'
    if cls == 'button':
        t = style & 0xF
        if t in (0x2, 0x3, 0x5, 0x6): return 'RES_CHECKBOX'
        if t in (0x4, 0x9): return 'RES_RADIOBUTTON'
        if t == 0x7: return 'RES_GROUPBOX'
        return 'RES_BUTTON'
    return {'edit': 'RES_EDIT', 'combobox': 'RES_COMBOBOX', 'listbox': 'RES_LISTBOX', 'scrollbar': 'RES_SCROLLBAR',
            'msctls_updown32': 'RES_UPDOWN', 'msctls_trackbar32': 'RES_TRACKBAR', 'systreeview32': 'RES_TREEVIEW',
            'systabcontrol32': 'RES_TABCONTROL', 'msctls_progress32': 'RES_PROGRESS', 'richedit20a': 'RES_RICHEDIT',
            'richedit20w': 'RES_RICHEDIT', 'richedit50w': 'RES_RICHEDIT', 'richedit': 'RES_RICHEDIT',
            'syslistview32': 'RES_LISTVIEW'}.get(cls, 'RES_CUSTOM')

def cstr(s):
    return 'nullptr' if s is None else c_escape(s)

def emit(parser, rcpath, symbol, exe):
    rcdir = os.path.dirname(os.path.abspath(rcpath))
    out = ['// generated by rc2cpp.py from %s - do not edit' % os.path.basename(rcpath),
           '#include "OrbiterResource.h"', '']
    deps = []
    imgrows = []
    for n, (kind, name, rid, path) in enumerate(parser.images):
        p = parser.pp.find(path.replace('\\', '/'), rcdir)
        if not p:
            raise RcError('image file not found: %s' % path)
        deps.append(p)
        data = open(p, 'rb').read()
        out.append('static const unsigned char img_%d[%d] = {' % (n, len(data)))
        for k in range(0, len(data), 32):
            out.append('\t' + ','.join('0x%02x' % b for b in data[k:k+32]) + ',')
        out.append('};')
        rk = {'BITMAP': 'RES_BITMAP', 'ICON': 'RES_ICON', 'PNG': 'RES_PNG'}[kind]
        imgrows.append('\t{%s, %d, %s, img_%d, %d},' % (rk, rid if rid is not None else -1, cstr(name), n, len(data)))
    if imgrows:
        out += ['', 'static const RESIMAGE images[] = {'] + imgrows + ['};', '']
    datarows = []
    for n, (rtype, name, rid, path) in enumerate(parser.data):
        p = parser.pp.find(path.replace('\\', '/'), rcdir)
        if not p:
            raise RcError('resource file not found: %s' % path)
        deps.append(p)
        data = open(p, 'rb').read()
        out.append('static const unsigned char data_%d[%d] = { // %s, zero-terminated' % (n, len(data) + 1, os.path.basename(p)))
        for k in range(0, len(data), 32):
            out.append('\t' + ','.join('0x%02x' % b for b in data[k:k+32]) + ',')
        out.append('\t0x00')
        out.append('};')
        datarows.append('\t{%s, %d, %s, data_%d, %d},' % (c_escape(rtype), rid if rid is not None else -1, cstr(name), n, len(data)))
    if datarows:
        out += ['', 'static const RESDATA resdata[] = {'] + datarows + ['};', '']
    strrows = ['\t{%d, %s},' % (sid, c_escape(s)) for sid, s in parser.strings]
    if strrows:
        out += ['', 'static const RESSTRING strings[] = {'] + strrows + ['};', '']
        # the same strings in an ELF section of their own, readable from the file without loading the module
        # (LOAD_LIBRARY_AS_DATAFILE): "OAPISTR1", then per string a little-endian u32 id, u32 length and the UTF-8 bytes
        blob = bytearray(b'OAPISTR1')
        for sid, s in parser.strings:
            b = s.encode('utf-8')
            blob += (sid & 0xFFFFFFFF).to_bytes(4, 'little') + len(b).to_bytes(4, 'little') + b
        out.append('__attribute__((section(".oapi_strtab"), used)) static const unsigned char strtab[%d] = {' % len(blob))
        for k in range(0, len(blob), 32):
            out.append('\t' + ','.join('0x%02x' % x for x in blob[k:k+32]) + ',')
        out += ['};', '']
    dlgrows = []
    for n, d in enumerate(parser.dialogs):
        if d['ctrls']:
            out.append('static const RESCONTROL dlg_%d_ctrl[] = { // %s' % (n, d['name']))
            for c in d['ctrls']:
                kind = classify(c)
                out.append('\t{%s, %d, %s, %s, %d, %d, %d, %d, %d, 0x%08x, 0x%08x, %s},' % (
                    kind, c['id'], cstr(c['text']), cstr(c['cls'] if kind == 'RES_CUSTOM' else None),
                    c['imgid'] if c['imgid'] is not None else -1,
                    c['x'], c['y'], c['cx'], c['cy'], c['style'] & 0xFFFFFFFF, c['exstyle'] & 0xFFFFFFFF, cstr(c.get('idname'))))
            out.append('};')
        dlgrows.append('\t{%d, %s, %s, %s, %d, %d, %d, %d, %d, %d, %d, 0x%08x, 0x%08x, %d, %s, %d},' % (
            d['id'] if d['id'] is not None else -1, cstr(d['name']), cstr(d['caption']), cstr(d['font'] or 'MS Shell Dlg'),
            d['fontsize'], d['weight'], d['italic'], d['x'], d['y'], d['cx'], d['cy'],
            d['style'] & 0xFFFFFFFF, d['exstyle'] & 0xFFFFFFFF, len(d['ctrls']), 'dlg_%d_ctrl' % n if d['ctrls'] else 'nullptr',
            d['menu']))
    if dlgrows:
        out += ['', 'static const RESDIALOG dialogs[] = {'] + dlgrows + ['};']
    menurows = []
    for n, m in enumerate(parser.menus):
        out.append('static const RESMENUITEM menu_%d_item[] = { // %s' % (n, m['name']))
        for mid, text, flags, nsub in m['items']:
            out.append('\t{%d, %s, 0x%x, %d},' % (mid, cstr(text), flags, nsub))
        out.append('};')
        menurows.append('\t{%d, %s, %d, menu_%d_item},' % (m['id'] if m['id'] is not None else -1, cstr(m['name']), len(m['items']), n))
    if menurows:
        out += ['', 'static const RESMENU menus[] = {'] + menurows + ['};']
    out += ['', 'static const RESTABLE table = {',
            '\t%s, %s,' % ('sizeof(dialogs)/sizeof(dialogs[0])' if dlgrows else '0', 'dialogs' if dlgrows else 'nullptr'),
            '\t%s, %s,' % ('sizeof(images)/sizeof(images[0])' if imgrows else '0', 'images' if imgrows else 'nullptr'),
            '\t%s, %s,' % ('sizeof(resdata)/sizeof(resdata[0])' if datarows else '0', 'resdata' if datarows else 'nullptr'),
            '\t%s, %s,' % ('sizeof(strings)/sizeof(strings[0])' if strrows else '0', 'strings' if strrows else 'nullptr'),
            '\t%s, %s' % ('sizeof(menus)/sizeof(menus[0])' if menurows else '0', 'menus' if menurows else 'nullptr'),
            '};', '']
    if exe:
        out.append('const RESTABLE *%s ()' % symbol)
    else:
        out.append('DLLCLBK const RESTABLE *%s ()' % symbol)
    out += ['{', '\treturn &table;', '}', '']
    return '\n'.join(out), deps

def main(argv):
    args = argv[1:]
    incdirs, symbol, exe, depfile = [], 'oapiModuleResources', False, None
    pos = []
    i = 0
    while i < len(args):
        a = args[i]
        if a == '-I': incdirs.append(args[i+1]); i += 2; continue
        if a == '--symbol': symbol = args[i+1]; i += 2; continue
        if a == '--exe': exe = True; i += 1; continue
        if a == '--depfile': depfile = args[i+1]; i += 2; continue
        pos.append(a); i += 1
    rcpath, outpath = pos
    pp = Preproc(incdirs)
    deps = [rcpath]
    text = pp.run(rcpath, deps)
    parser = Parser(tokenize(text) + [('eof', None)], pp, os.path.dirname(rcpath)).parse()
    code, imgdeps = emit(parser, rcpath, symbol, exe)
    old = open(outpath).read() if os.path.exists(outpath) else None
    if old != code:
        open(outpath, 'w').write(code)
    if depfile:
        with open(depfile, 'w') as f:
            f.write('%s: %s\n' % (outpath.replace(' ', '\\ '), ' '.join(d.replace(' ', '\\ ') for d in deps + imgdeps)))
    return 0

if __name__ == '__main__':
    try:
        sys.exit(main(sys.argv))
    except RcError as e:
        sys.stderr.write('rc2cpp: %s: %s\n' % (sys.argv[1] if len(sys.argv) > 1 else '', e))
        sys.exit(1)
