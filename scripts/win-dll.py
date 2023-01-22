#!/usr/bin/env python3
# Get non-system DLL dependencies
import pefile
import os
import os.path
import sys

def is_system(filename):
    """Return true if the file is a system file. Very ad-hoc."""
    return filename.startswith('/c/Windows')

def get_imports(filename):
    """Return list of dll imports"""
    if is_system(filename):
        return []
    with pefile.PE(filename) as pe:
        try:
            imports = pe.DIRECTORY_ENTRY_IMPORT
        except AttributeError:
            imports = []
        return [e.dll.decode() for e in imports]

def search_dll(name, libraries_path):
    """Search :param name: in :param libraries_path:"""
    for path in libraries_path:
        filename = os.path.join(path, name)
        if os.path.isfile(filename):
            return filename
    return None

def get_dependencies(name, libraries_path, cache):
    """Return the non-system dll dependencies of :param name:"""
    deps = get_imports(name)
    res = []
    for lib in deps:
        if lib in cache:
            continue
        # Search on the path
        filename = search_dll(lib, libraries_path)
        # Always add in the cache
        cache[lib] = filename
        if filename is not None:
            if not is_system(filename):
                res.append(filename)
            res.extend(get_dependencies(filename, libraries_path, cache))
    return res

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print('usage: {} ldd-file'.format(sys.argv[0]))
        sys.exit(1)

    libraries_path = os.environ['PATH'].split(os.pathsep)
    filename = sys.argv[1]
    cache = {}
    res = get_dependencies(filename, libraries_path, cache)
    for f in res:
        print(f)
