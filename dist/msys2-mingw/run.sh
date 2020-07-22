#!/bin/sh

cd $(dirname $0)

# Stop in case of error
set -e

enable_color() {
  ENABLECOLOR='-c '
  ANSI_RED="\033[31m"
  ANSI_GREEN="\033[32m"
  ANSI_YELLOW="\033[33m"
  ANSI_BLUE="\033[34m"
  ANSI_MAGENTA="\033[35m"
  ANSI_GRAY="\033[90m"
  ANSI_CYAN="\033[36;1m"
  ANSI_DARKCYAN="\033[36m"
  ANSI_NOCOLOR="\033[0m"
}

disable_color() { unset ENABLECOLOR ANSI_RED ANSI_GREEN ANSI_YELLOW ANSI_BLUE ANSI_MAGENTA ANSI_CYAN ANSI_DARKCYAN ANSI_NOCOLOR; }
enable_color

print_start() {
  if [ "x$2" != "x" ]; then
    COL="$2"
  elif [ "x$BASE_COL" != "x" ]; then
    COL="$BASE_COL"
  else
    COL="$ANSI_YELLOW"
  fi
  printf "${COL}${1}$ANSI_NOCOLOR\n"
}

gstart () {
  print_start "$@"
}
gend () {
  :
}
gblock () {
  gstart "$1"
    shift
    $@
  gend
}

[ -n "$CI" ] && {
  echo "INFO: set 'gstart' and 'gend' for CI"
  gstart () {
    printf '::group::'
    print_start "$@"
    SECONDS=0
  }

  gend () {
    duration=$SECONDS
    echo '::endgroup::'
    printf "${ANSI_GRAY}took $(($duration / 60)) min $(($duration % 60)) sec.${ANSI_NOCOLOR}\n"
  }
} || echo "INFO: not in CI"

#---

if [ -z "$TARGET" ]; then
  printf "${ANSI_RED}Undefined TARGET!$ANSI_NOCOLOR"
  exit 1
fi
cd "$TARGET"

# The command 'git describe' (used for version) needs the history. Get it.
# But the following command fails if the repository is complete.
gblock "Fetch --unshallow" git fetch --unshallow || true

case "$MINGW_INSTALLS" in
  *32)
    TARBALL_ARCH="i686"
  ;;
  *64)
    TARBALL_ARCH="x86_64"
  ;;
  *)
    printf "${ANSI_RED}Unknown MINGW_INSTALLS=${MINGW_INSTALLS}!$ANSI_NOCOLOR"
    exit 1
esac

gblock 'Install toolchain' pacman -S --noconfirm --needed base-devel mingw-w64-${TARBALL_ARCH}-toolchain
gblock 'Build package' makepkg-mingw --noconfirm --noprogressbar -sCLf --noarchive
gblock 'Archive package' makepkg-mingw --noconfirm --noprogressbar -R
gblock 'List artifacts' ls -la
gblock 'Install package' pacman --noconfirm -U "mingw-w64-${TARBALL_ARCH}-ghdl-${TARGET}-ci"-*-any.pkg.tar.zst
