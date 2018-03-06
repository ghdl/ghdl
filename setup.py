#!/usr/bin/env python

from distutils.core import setup, Extension
from distutils.command.build import build
from distutils.errors import CompileError
import distutils.dep_util
import distutils.ccompiler
import subprocess
import os
import os.path

class GHDLBuild(build):
    def copy_if_changed(self, source, destname):
        dest = os.path.join(self.build_temp, destname)
        if distutils.dep_util.newer(source, dest):
            print('copying ' + source)
            self.copy_file(source, dest, preserve_times=0)

    def run(self):
        # Run original build code
        build.run(self)

        self.mkpath(self.build_temp)

        srcdir = os.path.dirname(os.path.abspath(__file__))
        relpath = os.path.relpath(srcdir, self.build_temp)
        self.copy_if_changed(os.path.join(srcdir, 'src', 'version.in'),
                             'version.ads')
        self.copy_if_changed(os.path.join(srcdir, 'src', 'ghdldrv',
                                          'default_paths.ads.in'),
                             'default_paths.ads')

        comp = distutils.ccompiler.new_compiler()
        so_file = comp.shared_object_filename('libghdl')

        cmd = ['gnatmake',
               '-aI' + os.path.join(srcdir, 'src', 'vhdl', 'python'),
               '-aI' + os.path.join(srcdir, 'src', 'vhdl'),
               '-aI' + os.path.join(srcdir, 'src', 'psl'),
               '-aI' + os.path.join(srcdir, 'src'),
               '-aI' + os.path.join(srcdir, 'src', 'ghdldrv'),
               '-aI' + os.path.join(srcdir, 'src', 'grt'),
               '-z', 'libghdl',
               '-o', so_file, '-fPIC', '-gnat05', '-gnata', '-g',
               '-bargs', '-shared', '-Llibghdl_',
               '-largs', '-shared', '-v' ]

        def compile():
            print cmd
            if subprocess.call(cmd, cwd=self.build_temp) != 0:
                raise CompileError('compilation failure')

        self.execute(compile, [], 'Compiling ghdl')
        dstdir = os.path.join(self.build_lib, 'libghdl')
        self.copy_file(os.path.join(self.build_temp, so_file), dstdir)
        self.copy_tree(os.path.join(srcdir, "lib",), dstdir)

setup (name='libghdl',
       version='1.0',
       description = 'Interface to ghdl, a VHDL analyzer',
       author = 'Tristan Gingold',
       author_email = 'tgingold@free.fr',
       url = 'github.com/tgingold/ghdl',
       package_dir = {'libghdl' : 'src/vhdl/python/libghdl'},
       packages = ['libghdl'],
       cmdclass = {
           'build': GHDLBuild})
