#!/usr/bin/env bash
# Build GHDL locally on Windows with MSYS2, mirroring .github/workflows/Build-MSYS2.yml.
#
# Run from an MSYS2 UCRT64 (or MINGW64/CLANG64) shell at the repository root:
#   ./scripts/windows/build-msys2.sh [mcode|llvm|llvm-jit]
#
# Environment:
#   SKIP_DEPS=1   do not install packages with pacman
#   PREFIX=dir    install prefix (default: ./build/install-<backend>)
#
# The build uses upstream's default flags. Afterwards the MSYS2 runtime DLLs
# each binary needs are copied next to it (bin/ and lib/), like the CI
# standalone artifact, so the install directory runs without MSYS2 on PATH
# (mcode and llvm-jit; the llvm backend still needs the MSYS2 toolchain).
set -euo pipefail

backend="${1:-mcode}"
root="$(cd "$(dirname "$0")/../.." && pwd)"
prefix="${PREFIX:-$root/build/install-$backend}"
builddir="$root/build/$backend"

if [ -z "${MINGW_PACKAGE_PREFIX:-}" ]; then
  echo "error: run this from an MSYS2 UCRT64/MINGW64/CLANG64 shell, not MSYS or Git Bash." >&2
  exit 1
fi

if [ -z "${SKIP_DEPS:-}" ]; then
  p="$MINGW_PACKAGE_PREFIX"
  pkgs="make git diffutils $p-gcc $p-gcc-ada $p-binutils $p-zlib $p-python"
  case "$backend" in
    llvm*) pkgs="$pkgs $p-llvm $p-clang $p-libbacktrace" ;;
  esac
  # shellcheck disable=SC2086
  pacman -S --needed --noconfirm $pkgs
fi

# Link LLVM statically, like CI.  configure evals its VAR=value arguments, so
# the backquotes and the $ are escaped: the Makefile gets them unexpanded.
llvm_ldflags='LLVM_LDFLAGS=\`\$(LLVM_CONFIG) --link-static --ldflags --libs --system-libs\`'
config_args=()
case "$backend" in
  mcode) ;;
  llvm)
    config_args+=(--with-llvm-config CXX=clang++
                  "--with-backtrace-lib=$MINGW_PREFIX/lib/libbacktrace.a"
                  "$llvm_ldflags") ;;
  llvm-jit)
    config_args+=(--with-llvm-config CXX=clang++ --with-llvm-jit
                  "$llvm_ldflags") ;;
  *) echo "error: unknown backend '$backend'" >&2; exit 1 ;;
esac

nproc="$(nproc)"
mkdir -p "$builddir" "$prefix"
cd "$builddir"
# configure must be invoked by a relative path: an absolute MSYS path (/c/...)
# ends up in -aI flags that the native Windows gnatmake cannot resolve.
# Likewise give the prefix in mixed Windows form (C:/...).
GNATMAKE="gnatmake -j$nproc" MAKE="make -j$nproc" \
  ../../configure --prefix="$(cygpath -m "$prefix")" "${config_args[@]}"
make -j"$nproc"
make install

# Copy the MSYS2 DLLs each binary needs next to it (ldd lists them transitively).
for f in "$prefix"/bin/*.exe "$prefix"/lib/*.dll; do
  [ -e "$f" ] || continue
  for d in $(ldd "$f" | awk -v m="/${MSYSTEM,,}/" 'index($3, m) == 1 {print $3}'); do
    cp -u "$d" "$(dirname "$f")/"
  done
done

# Check that ghdl runs without MSYS2 on PATH.
PATH="$(cygpath -u "$SYSTEMROOT")/System32" "$prefix/bin/ghdl.exe" version >/dev/null \
  || { echo "error: ghdl.exe does not run without MSYS2 on PATH" >&2; exit 1; }

echo
echo "GHDL installed to $prefix"
if [ "$backend" = llvm ]; then
  # The llvm backend compiles and links each design with the MSYS2 toolchain.
  echo "note: the llvm backend needs $MINGW_PREFIX/bin on PATH to analyse and"
  echo "      elaborate designs (and to run the executables they produce)."
fi
"$prefix/bin/ghdl" version
