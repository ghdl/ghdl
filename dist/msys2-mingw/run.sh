#!/bin/sh

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

cd $(dirname $0)

build () {
  gstart 'Install common build dependencies'
    pacman -S --noconfirm base-devel git
  gend

  if [ -z "$TARGET" ]; then
    printf "${ANSI_RED}Undefined TARGET!$ANSI_NOCOLOR"
    exit 1
  fi
  cd "$TARGET"

  MINGW_INSTALLS="$(echo "$MINGW_INSTALLS" | tr '[:upper:]' '[:lower:]')"

  case "$MINGW_INSTALLS" in
    mingw32)
      TARBALL_ARCH="i686"
    ;;
    mingw64)
      TARBALL_ARCH="x86_64"

      # FIXME: specific versions of these packages should be installed automatically by makepkg-mingw.
      # E.g.: mingw-w64-x86_64-llvm-8.0.1-3 mingw-w64-x86_64-clang-8.0.1-3 mingw-w64-x86_64-z3-4.8.5-1
      # However, specifying the version produces 'error: target not found:'
      gstart "Install build dependencies"
        pacman -S --noconfirm mingw-w64-x86_64-llvm mingw-w64-x86_64-clang mingw-w64-x86_64-z3
      gend
    ;;
    *)
      echo "Unknown MING_INSTALLS=${MINGW_INSTALLS}!"
      exit 1
  esac
  gstart 'Install toolchain'
    pacman -S --noconfirm mingw-w64-${TARBALL_ARCH}-toolchain
  gend

  gstart 'Build package'
    dos2unix PKGBUILD
    makepkg-mingw -sCLfc --noconfirm --noprogressbar
  gend

  ls -la

  gstart 'Install package'
    pacman --noconfirm -U "mingw-w64-${TARBALL_ARCH}-ghdl-${TARGET}-ci"-*-any.pkg.tar.xz
  gend
}

test () {
  gstart 'Environment'
    env | grep MSYSTEM
    env | grep MINGW
  gend

  export PATH=$PATH:"$(cd $(dirname $(which ghdl))/../lib; pwd)"
  GHDL=ghdl ../../testsuite/testsuite.sh
}

case "$1" in
  -t)
    test
  ;;
  *)
    build
  ;;
esac
