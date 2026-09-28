#!/bin/bash
# not upstream: writes deps.list into the installed Orbiter folder, what the OpenOrbiter launcher checks and installs
# apt names come from this machine's package database (Debian/Ubuntu); without dpkg only the other lines are written
set -u
D=$1
OUT="$D/deps.list"

NAMES=$(find "$D" \( -type f -o -type l \) -printf '%f\n' | sort -u)
inside() { printf '%s\n' "$NAMES" | awk -v n="$1" '$0 == n { f = 1 } END { exit !f }'; }
# the C and C++ runtimes are always there; the launcher's ldd check reports them when they are too old
base() { case $1 in libc.so* | libm.so* | libgcc_s.so* | libstdc++.so* | ld-linux-x86-64.so* | libdl.so* | libpthread.so* | librt.so*) return 0 ;; esac; return 1; }
# the modules and every program in the package (Orbiter, the Utils); readelf skips the scripts
elfs() { find "$D" -type f \( -perm -u+x -o -name '*.so' -o -name '*.so.*' \) -print0; }

NEEDED=$(elfs | xargs -0 -r -n1 readelf -d 2>/dev/null | awk '/\(NEEDED\)/ { gsub(/[][]/, "", $5); print $5 }' | sort -u |
	while read -r lib; do inside "$lib" || base "$lib" || echo "$lib"; done)
PATHS=$(elfs | xargs -0 -r -n1 ldd 2>/dev/null | awk '$2 == "=>" && $3 ~ /^\// { print $1, $3 }' | sort -u)
path_of() { printf '%s\n' "$PATHS" | awk -v n="$1" '$1 == n { print $2; exit }'; }
GLIBC=$(elfs | xargs -0 -r -n1 objdump -T 2>/dev/null | sed -n 's/.*GLIBC_\([0-9.]*\).*/\1/p' | sort -Vu | tail -n1)

owner() {
	local p o
	for p in "$1" "$(readlink -f "$1")" "/usr$1"; do
		o=$(dpkg -S "$p" 2>/dev/null | awk -F': ' 'NR == 1 { split($1, a, ":"); print a[1] }')
		[ -n "$o" ] && { echo "$o"; return; }
	done
	echo "deps_list.sh: no package owns $1" >&2
}

# loaded at run time by Qt, so no NEEDED names them
QT=$(dirname "$(path_of libQt6Core.so.6)")/qt6/plugins
RUNTIME=$(ls "$QT"/platforms/libqxcb.so "$QT"/platforms/libqwayland*.so "$QT"/wayland-shell-integration/libxdg-shell.so \
	"$QT"/imageformats/libqjpeg.so "$QT"/imageformats/libqico.so 2>/dev/null)

{
	echo "# What the OpenOrbiter launcher checks and installs, made by cmake/deps_list.sh"
	echo "built_on $(. /etc/os-release && echo "$PRETTY_NAME")"
	echo "glibc ${GLIBC:-2.17}"
	if command -v dpkg >/dev/null; then
		for lib in $NEEDED; do p=$(path_of "$lib"); [ -n "$p" ] && echo "apt $(owner "$p")"; done
		for f in $RUNTIME; do echo "apt $(owner "$f")"; done
	fi
	for lib in $NEEDED; do echo "rpm $lib()(64bit)"; done
	echo "dnf qt6-qtwayland"
	echo "zypper qt6-wayland"
	for p in qt6-base qt6-wayland libpng vulkan-icd-loader libpipewire libglvnd glu; do echo "pacman $p"; done
} | awk 'NF > 1 && !seen[$0]++' >"$OUT"
echo "deps_list.sh: $(awk '$1 == "apt"' "$OUT" | wc -l) apt, $(awk '$1 == "rpm"' "$OUT" | wc -l) rpm entries in $OUT"
