# -*- coding: utf-8 -*-
import sys
from os import environ
from os.path import abspath
from pathlib import Path
from json import loads as json_loads

from pyTooling.Packaging import extractVersionInformation


ROOT = Path(__file__).resolve().parent


# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
sys.path.insert(0, abspath('.'))
sys.path.insert(0, abspath('..'))
sys.path.insert(0, abspath('../pyGHDL'))


# ==============================================================================
# Project information and versioning
# ==============================================================================
# The version info for the project you're documenting, acts as replacement for
# |version| and |release|, also used in various other places throughout the
# built documents.
project = "GHDL"

# Read __version__ from source file
packageInformationFile = Path(f"../py{project}/__init__.py")
versionInformation = extractVersionInformation(packageInformationFile)

author =    versionInformation.Author
copyright = versionInformation.Copyright
version =   ".".join(versionInformation.Version.split(".")[:2])  # e.g. 2.3    The short X.Y version.
release =   versionInformation.Version


# ==============================================================================
# Miscellaneous settings
# ==============================================================================
# The master toctree document.
master_doc = 'index'

# The suffix(es) of source filenames.
source_suffix = {
    '.rst': 'restructuredtext',
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
pygments_style = 'manni'


# ==============================================================================
# Restructured Text settings
# ==============================================================================
prologPath = Path("prolog.inc")
try:
    with prologPath.open("r", encoding="utf-8") as fileHandle:
        rst_prolog = fileHandle.read()
except Exception as ex:
    print(f"[ERROR:] While reading '{prologPath}'.")
    print(ex)
    rst_prolog = ""


# ==============================================================================
# Options for HTML output
# ==============================================================================
html_context = {}
ctx = ROOT / "context.json"
if ctx.is_file():
    html_context.update(json_loads(ctx.open("r").read()))

html_theme = "furo"

html_css_files = [
      "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/fontawesome.min.css",
      "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/solid.min.css",
      "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/brands.min.css",
]

html_theme_options = {
    "source_repository": "https://github.com/ghdl/ghdl",
    "source_branch": environ.get("GITHUB_REF_NAME", "main"),
    "source_directory": "doc",
    "footer_icons": [
        {
            "name": "GitHub ghdl/ghdl",
            "url": "https://github.com/ghdl/ghdl",
            "html": "",
            "class": "fa-solid fa-brands fa-github",
        },
    ],
}

html_title = release

html_static_path = ['_static']
# '/public' will contain the output from gnatdoc
html_extra_path = [str(Path(__file__).resolve().parent.parent / 'public')]   # FIXME: if not exist, create directory

html_logo = str(Path(html_static_path[0]) / 'logo.png')
html_favicon = str(Path(html_static_path[0]) / 'icon.png')


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
    'sphinx.ext.inheritance_diagram',
    'sphinx.ext.intersphinx',
    'sphinx.ext.todo',
    'sphinx.ext.graphviz',
    'sphinx.ext.mathjax',
    'sphinx.ext.ifconfig',
    'sphinx.ext.viewcode',
    # BuildTheDocs extensions
#    'btd.sphinx.autoprogram',
    # Other extensions
    'exec',
    'myst_parser',
    # 'sphinx_fontawesome',
    'sphinx_autodoc_typehints',
#    'autoapi.sphinx',
]


# ==============================================================================
# Sphinx.Ext.InterSphinx
# ==============================================================================
intersphinx_mapping = {
   'python':    ('https://docs.python.org/3', None),
   'cosim':     ('https://ghdl.github.io/ghdl-cosim', None),
   'poc':       ('https://poc-library.readthedocs.io/en/release', None),
   'vhdlmodel': ('https://vhdl.github.io/pyVHDLModel', None),
   'vasg':      ('https://ieee-p1076.gitlab.io', None)
}


# ==============================================================================
# Sphinx.Ext.AutoDoc
# ==============================================================================
# see: https://www.sphinx-doc.org/en/master/usage/extensions/autodoc.html#configuration
autodoc_member_order = "bysource"       # alphabetical, groupwise, bysource
autodoc_typehints = "both"


# ==============================================================================
# Sphinx.Ext.Graphviz
# ==============================================================================
graphviz_output_format = "svg"


# ==============================================================================
# Sphinx.Ext.Inheritance_Diagram
# ==============================================================================
inheritance_node_attrs = {
#    "shape": "ellipse",
#    "fontsize": 14,
#    "height": 0.75,
    "color": "dodgerblue1",
    "style": "filled"
}

# ==============================================================================
# Sphinx.Ext.ToDo
# ==============================================================================
# If true, `todo` and `todoList` produce output, else they produce nothing.
todo_include_todos = True
todo_link_only = True


# ==============================================================================
# Sphinx.Ext.Coverage
# ==============================================================================
coverage_show_missing_items = True


# ==============================================================================
# Sphinx.Ext.ExtLinks
# ==============================================================================
extlinks = {
   'wikipedia': ('https://en.wikipedia.org/wiki/%s', 'w:%s'),
   'ghdlsharp': ('https://github.com/ghdl/ghdl/issues/%s', '#%s'),
   'ghdlissue': ('https://github.com/ghdl/ghdl/issues/%s', 'issue #%s'),
   'ghdlpull':  ('https://github.com/ghdl/ghdl/pull/%s', 'pull request #%s'),
   'ghdlsrc':   ('https://github.com/ghdl/ghdl/blob/master/src/%s', '%s')
}


# ==============================================================================
# AutoAPI.Sphinx
# ==============================================================================
autoapi_modules = {
  'pyGHDL':  {'output': "pyGHDL", "override": True}
}
