#!/usr/bin/env bash

GetMinGWLibraries() {
  ldd "${MSYSTEM_PREFIX}/$1" | while IFS="" read -r dependency; do
    fields=($dependency)
    dep="${fields[2]}"
    if [[ "$dep" == /"${MSYSTEM,,}"/* ]]; then
      echo "$dep"
      GetMinGWLibraries "${dep#"/${MSYSTEM,,}/"}"
    fi
  done
}

for dep in $(GetMinGWLibraries "lib/libghdl-3_0_0_dev.dll"); do
  echo "$dep"
  cp "$dep" ./
done
