# -*- coding: utf-8 -*-
import sys, re
from os.path import abspath
from pathlib import Path
from json import loads


# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
sys.path.insert(0, abspath('.'))
sys.path.insert(0, abspath('..'))
sys.path.insert(0, abspath('../pyGHDL'))


# ==============================================================================
# Project information
# ==============================================================================
project = u'GHDL'
copyright = u'2002-2021, Tristan Gingold and contributors'
author = u'Tristan Gingold and contributors'

# ==============================================================================
# Versioning
# ==============================================================================
# The version info for the project you're documenting, acts as replacement for
# |version| and |release|, also used in various other places throughout the
# built documents.
try:
    with open('../configure') as verin:
        for line in verin:
            line = re.findall(r'ghdl_version=\"([0-9].+)\"', line)
            if line:
                version = line[0]
                break
except Exception as e:
    print('cannot extract version: %s' % e)
    version = "latest"
    pass

release = version  # The full version, including alpha/beta/rc tags.

# ==============================================================================
# Miscellaneous settings
# ==============================================================================
# The master toctree document.
master_doc = 'index'

# The suffix(es) of source filenames.
source_suffix = {
    '.rst': 'restructuredtext',
#    '.txt': 'markdown',
#    '.md': 'markdown',
}

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = [
	"_build",
	"_theme",
	"Thumbs.db",
	".DS_Store"
]

# The name of the Pygments (syntax highlighting) style to use.
pygments_style = 'stata-dark'


# ==============================================================================
# Restructured Text settings
# ==============================================================================
prologPath = "prolog.inc"
try:
    with open(prologPath, "r") as prologFile:
        rst_prolog = prologFile.read()
except Exception as ex:
    print("[ERROR:] While reading '{0!s}'.".format(prologPath))
    print(ex)
    rst_prolog = ""



# ==============================================================================
# Options for HTML output
# ==============================================================================
html_theme = "_theme"
html_theme_path = ["."]
html_theme_options = {
    'logo_only': True,
    'home_breadcrumbs': False,
    'vcs_pageview_mode': 'blob',
}

html_static_path = ['_static']
# '/public' will contain the output from gnatdoc
html_extra_path = [str(Path(__file__).resolve().parent.parent / 'public')]   # FIXME: if not exist, create directory

html_logo = str(Path(html_static_path[0]) / 'logo.png')
html_favicon = str(Path(html_static_path[0]) / 'icon.png')

html_context = {}
ctx = Path(__file__).resolve().parent / 'context.json'
if ctx.is_file():
    html_context.update(loads(ctx.open('r').read()))

# ==============================================================================
# Options for manual page output
# ==============================================================================
# One entry per manual page. List of tuples:
man_pages = [(
    master_doc,             # source start file
    'ghdl',                 # name
    u'GHDL Documentation',  # description
    [author],               # authors
    1                       # manual section
)]


# ==============================================================================
# Options for LaTeX / PDF output
# ==============================================================================
# Grouping the document tree into Texinfo files. List of tuples:
texinfo_documents = [(
    master_doc,             # source start file
    'GHDL',                 # target name
    u'GHDL Documentation',  # title
    author,                 # author
    'GHDL',                 # dir menu entry
    'VHDL simulator.',      # description
    'Miscellaneous'         # category
)]

from textwrap import dedent

latex_elements = {
	# The paper size ('letterpaper' or 'a4paper').
	'papersize': 'a4paper',

	# The font size ('10pt', '11pt' or '12pt').
	#'pointsize': '10pt',

	# Additional stuff for the LaTeX preamble.
	'preamble': dedent(r"""
		% ================================================================================
		% User defined additional preamble code
		% ================================================================================
		% Add more Unicode characters for pdfLaTeX.
		% - Alternatively, compile with XeLaTeX or LuaLaTeX.
		% - https://github.com/sphinx-doc/sphinx/issues/3511
		%
		\ifdefined\DeclareUnicodeCharacter
			\DeclareUnicodeCharacter{2265}{$\geq$}
			\DeclareUnicodeCharacter{21D2}{$\Rightarrow$}
		\fi

		% ================================================================================
		"""),

	# Latex figure (float) alignment
	#'figure_align': 'htbp',
}

# ==============================================================================
# Extensions
# ==============================================================================
extensions = [
    # Standard Sphinx extensions
    'sphinx.ext.autodoc',
    'sphinx.ext.extlinks',
    'sphinx.ext.intersphinx',
    'sphinx.ext.todo',
#    'sphinx.ext.graphviz',
    'sphinx.ext.mathjax',
    'sphinx.ext.ifconfig',
    'sphinx.ext.viewcode',

    # BuildTheDocs extensions
    'btd.sphinx.autoprogram',
    'btd.sphinx.graphviz',
    'btd.sphinx.inheritance_diagram',

    # Other extensions
#    'recommonmark',
    'exec',
#     'DocumentMember',
    'sphinx_fontawesome',
    'sphinx_autodoc_typehints',

    # local extensions (patched)
    'autoapi.sphinx',
]


# ==============================================================================
# Sphinx.Ext.InterSphinx
# ==============================================================================
intersphinx_mapping = {
   'python': ('https://docs.python.org/3.6/', None),
   'cosim': ('https://ghdl.github.io/ghdl-cosim', None),
   'poc': ('https://poc-library.readthedocs.io/en/release', None),
   'vhdlmodel': ('https://vhdl.github.io/pyVHDLModel', None),
}


# ==============================================================================
# Sphinx.Ext.AutoDoc
# ==============================================================================
# see: https://www.sphinx-doc.org/en/master/usage/extensions/autodoc.html#configuration
autodoc_member_order = "bysource"       # alphabetical, groupwise, bysource
# autodoc_default_options = {
#     "members": True,
#     'undoc-members': True,
#     #'private-members': True,
#     'inherited-members': True,
# }


# ==============================================================================
# Sphinx.Ext.Graphviz
# ==============================================================================
graphviz_output_format = "svg"


# ==============================================================================
# Sphinx.Ext.ToDo
# ==============================================================================
# If true, `todo` and `todoList` produce output, else they produce nothing.
todo_include_todos = True
todo_link_only = True


# ==============================================================================
# Sphinx.Ext.ExtLinks
# ==============================================================================
extlinks = {
   'wikipedia': ('https://en.wikipedia.org/wiki/%s', None),
   'ghdlsharp': ('https://github.com/ghdl/ghdl/issues/%s', '#'),
   'ghdlissue': ('https://github.com/ghdl/ghdl/issues/%s', 'issue #'),
   'ghdlpull':  ('https://github.com/ghdl/ghdl/pull/%s', 'pull request #'),
   'ghdlsrc':   ('https://github.com/ghdl/ghdl/blob/master/src/%s', None)
}


# ==============================================================================
# AutoAPI.Sphinx
# ==============================================================================
autoapi_modules = {
  'pyGHDL':  {'output': "pyGHDL", "override": True}
}
