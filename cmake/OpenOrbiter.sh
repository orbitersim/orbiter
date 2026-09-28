#!/bin/bash
# not upstream: the "OpenOrbiter" shortcut (CPACK_PACKAGE_EXECUTABLES) for Linux; Orbiter runs from its own folder like the Windows shortcut's "Start in"
# First it offers to install what Orbiter needs from the distribution's own packages (deps.list, made by cmake/deps_list.sh).
#   ./OpenOrbiter [--verbose] [Orbiter arguments]   --verbose keeps Orbiter in this terminal, showing its output
#   ./OpenOrbiter --remove-menu                      takes Orbiter out of the app menu for good
#   OB_LAUNCHER_DRY_RUN=1                  shows the install command instead of running it
#   OB_LAUNCHER_PM=apt|dnf|zypper|pacman   picks the package manager
#   OB_LAUNCHER_NO_EXEC=1                  checks and fixes, then stops before starting Orbiter
set -u

DIR=$(dirname "$(readlink -f "$0")")
BIN="$DIR/Orbiter"
DEPS="$DIR/deps.list"
TITLE="Orbiter"
CACHE="${XDG_CACHE_HOME:-$HOME/.cache}/orbiter64-linux"
STAMP="$CACHE/launcher.ok"
LOG="$CACHE/launcher.log"
APPLOG="$CACHE/orbiter.log"
MENU="${XDG_DATA_HOME:-$HOME/.local/share}/applications/orbiter64-linux.desktop"
NO_MENU="$CACHE/no-menu"

VERBOSE=0
ARGS=()
for arg in "$@"; do
    case $arg in
        --verbose) VERBOSE=1 ;;
        --remove-menu)
            rm -f "$MENU"
            mkdir -p "$CACHE" && touch "$NO_MENU"
            echo "Orbiter is out of the app menu. Starting it once with --add-menu puts it back."
            exit 0 ;;
        --add-menu) rm -f "$NO_MENU" ;;
        *) ARGS+=("$arg") ;;
    esac
done

list() { awk -v f="$1" '$1 == f { print $2 }' "$DEPS"; }
value() { awk -v f="$1" '$1 == f { $1 = ""; sub(/^ /, ""); print; exit }' "$DEPS"; }
words() { printf '%s\n' "$@" | awk 'NF' | sed 's/()(64bit)$//' | paste -sd' ' | fold -s -w 72; }
ver_ge() { [ "$(printf '%s\n%s\n' "$2" "$1" | sort -V | head -n1)" = "$2" ]; }

UI=none
if [ -t 0 ] && [ -t 1 ]; then
    UI="tty"
elif [ -n "${DISPLAY:-}${WAYLAND_DISPLAY:-}" ]; then
    if command -v zenity >/dev/null; then UI=zenity; elif command -v kdialog >/dev/null; then UI=kdialog; fi
fi

info() {
    case $UI in
        tty)
            printf '\n%s\n\n' "$1" >&2
            [ -n "${OB_LAUNCHER_IN_TERM:-}" ] && read -r -p "Press Enter to close. " _ ;;
        zenity) zenity --error --title="$TITLE" --no-markup --width=520 --text="$1" 2>/dev/null ;;
        kdialog) kdialog --title "$TITLE" --error "$1" 2>/dev/null ;;
        *)
            printf '%s\n' "$1" >&2
            command -v notify-send >/dev/null && notify-send "$TITLE" "$1" ;;
    esac
}

ask() {
    case $UI in
        tty)
            printf '\n%s\n\n' "$1"
            local answer
            read -r -p "Install them now? [Y/n] " answer
            [[ -z $answer || $answer == [Yy]* ]] ;;
        zenity)
            zenity --question --title="$TITLE" --no-markup --width=520 --ok-label=Install --cancel-label=Cancel \
                --text="$1"$'\n\nInstall them now?' 2>/dev/null ;;
        kdialog) kdialog --title "$TITLE" --yes-label Install --no-label Cancel --yesno "$1"$'\n\nInstall them now?' 2>/dev/null ;;
        *) return 1 ;;
    esac
}

# with nothing to show a question in, the launcher asks again from a terminal window
open_terminal() {
    local t
    [ -n "${DISPLAY:-}${WAYLAND_DISPLAY:-}" ] || return 1
    shopt -s execfail
    for t in x-terminal-emulator gnome-terminal konsole xfce4-terminal xterm; do
        command -v "$t" >/dev/null || continue
        export OB_LAUNCHER_IN_TERM=1
        if [ "$t" = gnome-terminal ]; then exec "$t" -- "$0" "$@"; fi
        # shellcheck disable=SC2093
        exec "$t" -e "$0" "$@"
    done
    return 1
}

as_root() {
    if [ -n "${OB_LAUNCHER_DRY_RUN:-}" ]; then
        printf 'dry run, as root: %s\n' "$1" >&2
        return 0
    fi
    if [ "$(id -u)" = 0 ]; then sh -c "$1"; return; fi
    if [ "$UI" = tty ]; then
        if ! command -v sudo >/dev/null; then info "sudo isn't installed. As root, run: $1"; return 1; fi
        sudo sh -c "$1"
        return
    fi
    if ! command -v pkexec >/dev/null; then info "pkexec isn't installed. As root, run: $1"; return 1; fi
    mkdir -p "$CACHE"
    if [ "$UI" = zenity ]; then
        # the bar pulses until the install ends and closes the pipe
        (pkexec sh -c "$1" >"$LOG" 2>&1; echo $? >"$CACHE/launcher.rc") |
            zenity --progress --pulsate --auto-close --no-cancel --title="$TITLE" \
                --text="Installing what Orbiter needs…" 2>/dev/null
        return "$(cat "$CACHE/launcher.rc" 2>/dev/null || echo 1)"
    fi
    pkexec sh -c "$1" >"$LOG" 2>&1
}

PM="${OB_LAUNCHER_PM:-}"
if [ -z "$PM" ]; then
    if command -v apt-get >/dev/null && command -v dpkg-query >/dev/null; then PM=apt
    elif command -v dnf >/dev/null; then PM=dnf
    elif command -v zypper >/dev/null; then PM=zypper
    elif command -v pacman >/dev/null; then PM=pacman
    fi
fi

missing_packages() {
    local want
    case $PM in
        apt)
            want=$(list apt)
            # shellcheck disable=SC2086
            dpkg-query -W -f='${db:Status-Abbrev} ${Package}\n' $want 2>/dev/null |
                awk '$1 == "ii" { print $2 }' |
                awk 'FILENAME == "-" { have[$0]; next } !($0 in have)' - <(printf '%s\n' $want) ;;
        dnf | zypper)
            for want in $(list rpm) $(list "$PM"); do
                rpm -q --whatprovides "$want" >/dev/null 2>&1 || echo "$want"
            done ;;
        pacman)
            # shellcheck disable=SC2046
            if command -v pacman >/dev/null; then pacman -T $(list pacman); else list pacman; fi ;;
    esac
}

install_command() {
    local quoted
    quoted=$(printf "'%s' " "$@")
    case $PM in
        apt) echo "apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y $quoted" ;;
        dnf) echo "dnf install -y $quoted" ;;
        zypper) echo "zypper --non-interactive install $quoted" ;;
        pacman) echo "pacman -S --needed --noconfirm $quoted" ;;
    esac
}

# Orbiter and its modules; a library that is a file of this package is found at run time (RUNPATH, or loaded first)
LDD_MISSING=""
LDD_OLD=""
check_libraries() {
    local out names
    out=$(LC_ALL=C ldd "$BIN" 2>&1; find "$DIR" -type f \( -name '*.so' -o -name '*.so.*' \) -exec env LC_ALL=C ldd {} + 2>&1)
    names=$(find "$DIR" \( -type f -o -type l \) -name '*.so*' -printf '%f\n')
    LDD_MISSING=$(printf '%s\n' "$out" | awk '/=> not found/ { print $1 }' | sort -u |
        OWN="$names" awk 'BEGIN { n = split(ENVIRON["OWN"], a, "\n"); for (i = 1; i <= n; i++) have[a[i]] } !($0 in have)')
    LDD_OLD=$(printf '%s\n' "$out" | sed -n "s/.*version \`\([^']*\)' not found.*/\1/p" | sort -u)
}

describe_old() {
    local v
    for v in $LDD_OLD; do
        case $v in
            GLIBC_*) echo "  the C library (glibc) ${v#GLIBC_} or newer" ;;
            GLIBCXX_* | CXXABI_*) echo "  a newer C++ runtime (libstdc++, $v)" ;;
            Qt_*) echo "  Qt ${v#Qt_} or newer" ;;
            *) echo "  $v" ;;
        esac
    done | sort -u
}

too_old() {
    info "Orbiter needs a newer Linux than this one. It is missing:

$1

It's built on $(value built_on) and runs on distributions at least that new. Updating the system, or moving to a newer release of the distribution, fixes this."
    exit 1
}

# the graphics client needs a Vulkan driver; without one Orbiter still runs, in console mode
check_vulkan() {
    local d
    [ -n "${VK_DRIVER_FILES:-}${VK_ICD_FILENAMES:-}" ] && return
    for d in /usr/share/vulkan/icd.d /usr/local/share/vulkan/icd.d /etc/vulkan/icd.d "${XDG_DATA_HOME:-$HOME/.local/share}/vulkan/icd.d"; do
        compgen -G "$d/*.json" >/dev/null && return
    done
    info "No Vulkan driver was found. Orbiter starts, but without a 3D window (console mode) until one is installed: Mesa's Vulkan drivers (mesa-vulkan-drivers on Debian, Ubuntu and Fedora) or your graphics card maker's driver."
}

# the app menu's entry, pointing at wherever this copy is
add_menu_entry() {
    [ "$(id -u)" = 0 ] || [ -e "$NO_MENU" ] && return
    case $DIR in *[\"\`\$\\]*) return ;; esac
    local want
    want="[Desktop Entry]
Type=Application
Name=Orbiter
GenericName=Space flight simulator
Comment=Fly spacecraft through the solar system
Exec=\"${DIR//%/%%}/OpenOrbiter\"
Icon=applications-science
Terminal=false
Categories=Game;Simulation;
StartupWMClass=Orbiter"
    [ "$(cat "$MENU" 2>/dev/null)" = "$want" ] && return
    mkdir -p "$(dirname "$MENU")" && printf '%s\n' "$want" >"$MENU" || return
    command -v update-desktop-database >/dev/null && update-desktop-database -q "$(dirname "$MENU")" 2>/dev/null
}

# Orbiter runs on its own from its folder, its output in a file; --verbose keeps it here
start() {
    if [ -n "${OB_LAUNCHER_NO_EXEC:-}" ]; then echo "Orbiter is ready to start."; exit 0; fi
    add_menu_entry
    cd "$DIR" || exit 1
    if [ "$VERBOSE" = 1 ]; then exec ./Orbiter "${ARGS[@]}"; fi
    mkdir -p "$CACHE"
    [ -f "$APPLOG" ] && mv -f "$APPLOG" "$APPLOG.1"
    if [ "$UI" != tty ]; then exec ./Orbiter "${ARGS[@]}" >"$APPLOG" 2>&1 </dev/null; fi
    if command -v setsid >/dev/null; then
        setsid -f ./Orbiter "${ARGS[@]}" >"$APPLOG" 2>&1 </dev/null
    else
        nohup ./Orbiter "${ARGS[@]}" >"$APPLOG" 2>&1 </dev/null &
        disown
    fi
    echo "Orbiter is running; this terminal can be closed. Its output is in $APPLOG, its log in $DIR/Orbiter.log."
    exit 0
}

# a copy that dropped the permissions (a file manager, a zip, a USB stick) still has the file
[ -f "$BIN" ] && [ ! -x "$BIN" ] && chmod +x "$BIN" 2>/dev/null
if [ ! -x "$BIN" ] || [ ! -r "$DEPS" ]; then
    info "Orbiter or deps.list is missing from $DIR. Unpack the whole download again."
    exit 1
fi
if [ "$(uname -m)" != x86_64 ]; then
    info "This Orbiter build is for 64-bit PCs (x86_64), and this computer is $(uname -m)."
    exit 1
fi

# nothing changed since the last good check: the program and the package database are as they were
key() {
    printf '%s %s\n' "$(stat -c '%Y %s' "$BIN")" \
        "$(stat -c %Y /var/lib/dpkg/status /var/lib/rpm /usr/lib/sysimage/rpm /var/lib/pacman/local 2>/dev/null | paste -sd' ')"
}
remember() { mkdir -p "$CACHE" && key >"$STAMP"; }
if [ -z "${OB_LAUNCHER_DRY_RUN:-}${OB_LAUNCHER_PM:-}${OB_LAUNCHER_NO_EXEC:-}" ] && [ "$(cat "$STAMP" 2>/dev/null)" = "$(key)" ]; then
    start
fi

HAVE_GLIBC=$(getconf GNU_LIBC_VERSION 2>/dev/null | awk '{ print $2 }')
NEED_GLIBC=$(value glibc)
if [ -z "$HAVE_GLIBC" ]; then
    too_old "  the GNU C library (glibc) $NEED_GLIBC or newer (this system uses another C library)"
elif ! ver_ge "$HAVE_GLIBC" "$NEED_GLIBC"; then
    too_old "  the C library (glibc) $NEED_GLIBC or newer (this system has $HAVE_GLIBC)"
fi

MISSING=$(missing_packages)
check_libraries
if [ -n "$LDD_OLD" ]; then too_old "$(describe_old)"; fi

if [ -z "$MISSING" ] && [ -z "$LDD_MISSING" ]; then
    check_vulkan
    remember
    start
fi

if [ -z "$PM" ]; then
    info "Orbiter can't start because these libraries are missing:

$(words $LDD_MISSING)

Install them with your distribution's package manager, then start Orbiter again."
    exit 1
fi
if [ -z "$MISSING" ]; then
    info "Everything Orbiter asks $PM for is installed, but these libraries are still missing:

$(words $LDD_MISSING)

This distribution's repositories may not have them. Orbiter is built and tested on $(value built_on)."
    exit 1
fi

if [ "$UI" = none ]; then
    open_terminal "$@"
    info "Orbiter needs these packages: $(words $MISSING)"
    exit 1
fi

COUNT=$(printf '%s\n' "$MISSING" | awk 'NF' | wc -l)
if [ "$COUNT" = 1 ]; then WHAT="1 package that isn't"; else WHAT="$COUNT packages that aren't"; fi
# shellcheck disable=SC2086
ask "Orbiter needs $WHAT installed yet:

$(words $MISSING)

They come from your distribution's own repositories ($PM) and need your password to install." || exit 1

# shellcheck disable=SC2086
if ! as_root "$(install_command $MISSING)"; then
    info "Installing didn't finish, so Orbiter can't start yet.$([ "$UI" != tty ] && printf ' The details are in %s.' "$LOG")"
    exit 1
fi
if [ -n "${OB_LAUNCHER_DRY_RUN:-}" ]; then exit 0; fi

MISSING=$(missing_packages)
check_libraries
if [ -n "$LDD_OLD" ]; then too_old "$(describe_old)"; fi
if [ -n "$MISSING$LDD_MISSING" ]; then
    info "After installing, Orbiter is still missing:

$(words $MISSING $LDD_MISSING)

This distribution's repositories may not have them. Orbiter is built and tested on $(value built_on)."
    exit 1
fi
check_vulkan
remember
start
