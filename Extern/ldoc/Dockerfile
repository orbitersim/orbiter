# syntax=docker/dockerfile:1.4.3-labs

FROM akorn/luarocks:lua5.4-alpine AS builder

RUN apk add --no-cache -X http://dl-cdn.alpinelinux.org/alpine/edge/testing \
	dumb-init gcc git libc-dev

COPY ./ /src
WORKDIR /src

RUN luarocks --tree /pkgdir/usr/local --local install cmark && \
    luarocks --tree /pkgdir/usr/local --local install lunamark && \
    luarocks --tree /pkgdir/usr/local --local install lua-discount
RUN luarocks --tree /pkgdir/usr/local make
RUN find /pkgdir -type f -exec sed -i -e 's!/pkgdir!!g' {} \;

FROM akorn/lua:5.4-alpine AS final

RUN apk add --no-cache -X http://dl-cdn.alpinelinux.org/alpine/edge/testing \
    dumb-init setpriv

LABEL org.opencontainers.image.title="ldoc"
LABEL org.opencontainers.image.description="A containerized version of LDoc, a documentation generator for Lua and related C modules"
LABEL org.opencontainers.image.authors="Caleb Maclennan <caleb@alerque.com>"
LABEL org.opencontainers.image.licenses="MIT"
LABEL org.opencontainers.image.url="https://github.com/lunarmodules/ldoc/pkgs/container/ldoc"
LABEL org.opencontainers.image.source="https://github.com/lunarmodules/ldoc"

COPY --from=builder /pkgdir /
RUN ldoc --version

WORKDIR /data

# GitHub Actions and some other environments insist on running the entrypoint
# as root inside the container even when being run by a non priviledged user on
# their own files. Here we check the ownership of the workdir and change our
# effective user/group ID to match.
RUN cat <<'EOF' > /usr/local/bin/entrypoint.sh
#!/bin/sh
if [ "$(id -u)" -ne "$(stat -c '%u' .)" ]; then
  eids="$(stat -c '--euid %u --egid %g' .)"
fi
exec ${eids:+setpriv --clear-groups $eids} ldoc $@
EOF
ENTRYPOINT ["sh", "/usr/local/bin/entrypoint.sh"]
