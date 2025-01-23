LUA= $(shell echo `which lua`)
LUA_BINDIR= $(shell echo `dirname $(LUA)`)
LUA_PREFIX= $(shell echo `dirname $(LUA_BINDIR)`)
LUA_SHAREDIR=$(LUA_PREFIX)/share/lua/5.1

_REPODIR != cd "$(shell dirname $(firstword $(MAKEFILE_LIST)))/" && pwd

ldoc:

install: install_parts
	@echo "lua $(LUA_SHAREDIR)/ldoc.lua \$$*" > "$(DESTDIR)$(LUA_BINDIR)/ldoc"
	@chmod -v +x "$(DESTDIR)$(LUA_BINDIR)/ldoc"

install_luajit: install_parts
	@echo "luajit $(LUA_SHAREDIR)/ldoc.lua \$$*" > "$(DESTDIR)$(LUA_BINDIR)/ldoc"
	@chmod -v +x "$(DESTDIR)$(LUA_BINDIR)/ldoc"

install_parts:
	@if [ ! -d "$(DESTDIR)$(LUA_BINDIR)" ]; then \
		mkdir -vp "$(DESTDIR)$(LUA_BINDIR)"; \
	fi
	@mkdir -vp "$(DESTDIR)$(LUA_SHAREDIR)"
	@cp -v ldoc.lua "$(DESTDIR)$(LUA_SHAREDIR)"
	@cp -vr ldoc "$(DESTDIR)$(LUA_SHAREDIR)"

uninstall:
	@-rm -v "$(DESTDIR)$(LUA_SHAREDIR)/ldoc.lua"
	@-rm -vr "$(DESTDIR)$(LUA_SHAREDIR)/ldoc"
	@-rm -v "$(DESTDIR)$(LUA_BINDIR)/ldoc"

test: test-basic test-example test-md test-tables

RUN=&& lua $(_REPODIR)/ldoc.lua . && diff -r public cdocs && echo ok

test-prep:
	find -type d -name public -execdir rsync -av --del {}/ cdocs/ \;

test-basic:
	cd tests $(RUN)

test-example:
	cd tests/example $(RUN)

test-md:
	cd tests/md-test $(RUN)

test-tables:
	cd tests/simple $(RUN)

test-clean: clean-basic clean-example clean-md clean-tables

doc-site:
	lua $(_REPODIR)/ldoc.lua .

CLEAN=&& lua $(_REPODIR)/ldoc.lua . && rd /S /Q cdocs && cp -rf public cdocs

clean-basic:
	cd tests $(CLEAN)

clean-example:
	cd tests && cd example $(CLEAN)

clean-md:
	cd tests && cd md-test $(CLEAN)

clean-tables:
	cd tests && cd simple $(CLEAN)
