.. _RELEASE:

Releases and sources
####################

.. contents:: Contents of this Page
   :local:

Using package managers
**********************

Package managers of many popular distributions provide pre-built packages of GHDL. This is the case for `apt` (Debian/Ubuntu), `dnf` (Fedora/CentOS) or `pacman` (Arch Linux). Since GHDL supports three different backends and two library sets (ieee or openieee), at least six packages with different features might be available in each package manager. See differences between backends in :ref:`BUILD`.

.. _RELEASE:packages:

Downloading pre-built packages
******************************

Assets from nightly GHDL builds are available at `github.com/ghdl/ghdl/releases/nightly <https://github.com/ghdl/ghdl/releases/nightly>`_. These are mostly meant to be used in Continuous Integration (CI) workflows. Precisely, `setup-ghdl-ci <https://github.com/ghdl/setup-ghdl-ci>`_ allows to easily setup nightly assets in GitHub Actions workflows.

Furthermore, assets from stable builds are available for a larger set of platforms:

.. TODO How to extend this directive to use `.. only:: html` and `.. only:: html` in the python code passed to it?

.. exec::
   from helpers import createReleasesShields, printReleaseTab, printReleasesList

   # Optionally, provide a <tag> name to get the assets table of a specific release. By default, the table of 'latest' is returned.
   data = createReleasesShields()

.. only:: html

   .. exec::
      from helpers import printReleaseTab
      printReleaseTab('data')

.. only:: latex

   .. exec::
      from helpers import printReleaseTab
      printReleaseTab('data', latex=True)

.. rubric :: Pre-built packages of older releases

.. only:: html

   .. exec::
      from helpers import printReleasesList
      printReleasesList('data')

.. only:: latex

   .. exec::
      from helpers import printReleasesList
      printReleasesList('data', latex=True)

.. _RELEASE:Sources:

Downloading Source Files
************************

.. HINT::

   All the following procedures will retrieve the latest development version of GHDL, i.e., the `master` branch at `github.com/ghdl/ghdl <https://github.com/ghdl/ghdl>`_.
   We do our best to keep it stable, but bugs can seldom be published. See `HINT` boxes below for instructions to get older releases.

.. _RELEASE:Sources:Zip:

.. rubric :: Tarball/zip-file

GHDL can be downloaded as a zip-file or tarball from GitHub. See the following table, to
choose your desired format/version:

.. only:: html

   .. |zip-master| image:: https://img.shields.io/badge/ZIP-archive/master-323131.svg?longCache=true&style=flat-square&logo=data%3Aimage%2Fpng%3Bbase64%2CiVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAACE1BMVEUAAAAAAABcXFwAAACpqakAAABXV1cAAAAAAADAwMBYWFgAAACcnJxzc3MiIiKPj4%2FExMRaWlohISHo6OgbGxs5OTnMzMw9PT3AwMBWVlZkZGSGhoanp6eLi4vMzMyAgIC3t7eUlJSysrKNjY2Wlparq6uysrKlpaW1tbV6enqzs7PR0dGrq6uEhISwsLDFxcW9vb3Kysrg4OC8vLy3t7fPz8%2FDw8Ojo6OsrKzS0tLQ0NC9vb3ExMTm5ua9vb3Q0NChoaGsrKyurq7e3t7U1NSWlpaJiYmNjY3R0dG0tLSVlZXCwsK8vLzDw8Ph4eHk5OTW1tbW1tbm5ube3t7g4ODKysq3t7fOzs7f39%2FW1tbR0dHOzs7CwsLe3t7c3Nzn5%2BfW1tbq6urIyMjb29vW1tbe3t7X19fa2trb29vt7e3q6urHx8ft7e3k5OTh4eHPz8%2FV1dXT09Pm5ubh4eHg4ODm5ub9%2Ff3%2F%2F%2F%2F%2F%2F%2F%2Fk5OTp6enY2Njo6OjZ2dnn5%2Bfp6enc3Nzu7u76%2Bvr09PTk5OTw8PDn5%2Bf5%2Bfnf39%2Fq6urg4ODo6Ojk5OT4%2BPjm5ubm5ubs7Ozu7u76%2Bvrk5OTu7u739%2Ffq6urr6%2Bvx8fH6%2Bvrt7e34%2BPj6%2Bvr%2B%2Fv7s7Oz5%2Bfn%2B%2Fv7%2F%2F%2F%2Fp6enr6%2Bvt7e3v7%2B%2Fx8fHy8vLz8%2FP09PT29vb39%2Ff5%2Bfn8%2FPz9%2Ff3%2B%2Fv7%2F%2F%2F9qYR%2FuAAAAonRSTlMAAQECAgMDBAYGBwgKCwwMDQ4SEhQUGhwdHiIjIyQkJygpMDIzMzQ1NTY3OTo8PDw%2FP0ZITk9RUlNTVldXV1hYWlpaWltdYGBiY2ZpbHB1dXZ3d3t8fX5%2Ff4aHiIqKj5WXn6KjpKmssrK0t7u8vb7BwsPEyszNzc3O0dLT09fY2tvf4OXm5ufn6ers7O3w8fLy8vL09fX29%2Ff4%2Bvv7%2FP39%2Ff5qibsTAAABrElEQVR4AX2LhfcSURCFBxHBbkWxuwW7Q7AbQ7AbuwMMRQxRVAwMxRBWBRSX%2BRN%2F97y3y9ldlv3OmfPu3PkemfBsVbaQAwsrzPxnLrVh4huc65h3I8iGno9walyj6wzu9CIrVxk86YvU%2BxVS6SKZOP4D5ccxJJnxHtvnvdRk10sUlUVEJy4NFIV33d8S89P1JJj3GOfaDqQlG4%2BcX7tdlL6DKtr7UwgwuOwRdY85h08vuD1A5MFnGEgB7OlGkg0XZj5bPFXEcW91oQHj37Iu0uh%2BYNqXlZtFvKkLN%2FZ9g%2FJ7Qiep9JutjD25AiGpC0nqehZG4%2BEQaXQe%2BX3oUbNA1P8uFPWWTyqzPo2yCGDSAyj%2FT4ncZ%2F%2FzFgEs%2FwClQmDptvk2AtjJsht275C9QJqwevIxZ2ETf3UWrjBPdxR%2B7V6zykkYfY5ek0HIWIXx%2FGIQnowucC1mFmg4JlbTlngRoRw2CiBcRizGSZCoY8mHDEIoj1BPUJOUiiLr1wR%2FFo%2BaIiPeHIO0ENIMcl6yECig%2FqlNIUCtuIMKS5Sgm2xxRao4VyMuaos7qkQtvzsAWpTtdh6JoYQAAAAASUVORK5CYII%3D
      :target: https://github.com/ghdl/ghdl/archive/master.zip
      :alt: Source Code from GitHub - 'master' branch.

   .. |tgz-master| image:: https://img.shields.io/badge/TGZ-archive/master-323131.svg?longCache=true&style=flat-square&logo=data%3Aimage%2Fpng%3Bbase64%2CiVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAACE1BMVEUAAAAAAABcXFwAAACpqakAAABXV1cAAAAAAADAwMBYWFgAAACcnJxzc3MiIiKPj4%2FExMRaWlohISHo6OgbGxs5OTnMzMw9PT3AwMBWVlZkZGSGhoanp6eLi4vMzMyAgIC3t7eUlJSysrKNjY2Wlparq6uysrKlpaW1tbV6enqzs7PR0dGrq6uEhISwsLDFxcW9vb3Kysrg4OC8vLy3t7fPz8%2FDw8Ojo6OsrKzS0tLQ0NC9vb3ExMTm5ua9vb3Q0NChoaGsrKyurq7e3t7U1NSWlpaJiYmNjY3R0dG0tLSVlZXCwsK8vLzDw8Ph4eHk5OTW1tbW1tbm5ube3t7g4ODKysq3t7fOzs7f39%2FW1tbR0dHOzs7CwsLe3t7c3Nzn5%2BfW1tbq6urIyMjb29vW1tbe3t7X19fa2trb29vt7e3q6urHx8ft7e3k5OTh4eHPz8%2FV1dXT09Pm5ubh4eHg4ODm5ub9%2Ff3%2F%2F%2F%2F%2F%2F%2F%2Fk5OTp6enY2Njo6OjZ2dnn5%2Bfp6enc3Nzu7u76%2Bvr09PTk5OTw8PDn5%2Bf5%2Bfnf39%2Fq6urg4ODo6Ojk5OT4%2BPjm5ubm5ubs7Ozu7u76%2Bvrk5OTu7u739%2Ffq6urr6%2Bvx8fH6%2Bvrt7e34%2BPj6%2Bvr%2B%2Fv7s7Oz5%2Bfn%2B%2Fv7%2F%2F%2F%2Fp6enr6%2Bvt7e3v7%2B%2Fx8fHy8vLz8%2FP09PT29vb39%2Ff5%2Bfn8%2FPz9%2Ff3%2B%2Fv7%2F%2F%2F9qYR%2FuAAAAonRSTlMAAQECAgMDBAYGBwgKCwwMDQ4SEhQUGhwdHiIjIyQkJygpMDIzMzQ1NTY3OTo8PDw%2FP0ZITk9RUlNTVldXV1hYWlpaWltdYGBiY2ZpbHB1dXZ3d3t8fX5%2Ff4aHiIqKj5WXn6KjpKmssrK0t7u8vb7BwsPEyszNzc3O0dLT09fY2tvf4OXm5ufn6ers7O3w8fLy8vL09fX29%2Ff4%2Bvv7%2FP39%2Ff5qibsTAAABrElEQVR4AX2LhfcSURCFBxHBbkWxuwW7Q7AbQ7AbuwMMRQxRVAwMxRBWBRSX%2BRN%2F97y3y9ldlv3OmfPu3PkemfBsVbaQAwsrzPxnLrVh4huc65h3I8iGno9walyj6wzu9CIrVxk86YvU%2BxVS6SKZOP4D5ccxJJnxHtvnvdRk10sUlUVEJy4NFIV33d8S89P1JJj3GOfaDqQlG4%2BcX7tdlL6DKtr7UwgwuOwRdY85h08vuD1A5MFnGEgB7OlGkg0XZj5bPFXEcW91oQHj37Iu0uh%2BYNqXlZtFvKkLN%2FZ9g%2FJ7Qiep9JutjD25AiGpC0nqehZG4%2BEQaXQe%2BX3oUbNA1P8uFPWWTyqzPo2yCGDSAyj%2FT4ncZ%2F%2FzFgEs%2FwClQmDptvk2AtjJsht275C9QJqwevIxZ2ETf3UWrjBPdxR%2B7V6zykkYfY5ek0HIWIXx%2FGIQnowucC1mFmg4JlbTlngRoRw2CiBcRizGSZCoY8mHDEIoj1BPUJOUiiLr1wR%2FFo%2BaIiPeHIO0ENIMcl6yECig%2FqlNIUCtuIMKS5Sgm2xxRao4VyMuaos7qkQtvzsAWpTtdh6JoYQAAAAASUVORK5CYII%3D
      :target: https://github.com/ghdl/ghdl/archive/master.tar.gz
      :alt: Source Code from GitHub - 'master' branch.

   +----------+------------------------+
   | Branch   | Download Link          |
   +==========+========================+
   | master   | |zip-master|           |
   +----------+------------------------+
   | master   | |tgz-master|           |
   +----------+------------------------+

.. HINT::

   To download a specific version of GHDL, use this alternative URL, where ``<format>`` is ``tar.gz`` or ``zip``: ``https://codeload.github.com/ghdl/ghdl/<format>/<tag>``.

.. _RELEASE:Sources:GitClone:

.. rubric :: git clone

GHDL can be downloaded (cloned) with ``git clone`` from GitHub. GitHub offers
the transfer protocols HTTPS and SSH. You should use SSH if you have a GitHub
account and have already uploaded an OpenSSH public key to GitHub, otherwise
use HTTPS if you have no account or you want to use login credentials.

+----------+----------------------------------------+
| Protocol | GitHub Repository URL                  |
+==========+========================================+
| HTTPS    | https://github.com/ghdl/ghdl.git       |
+----------+----------------------------------------+
| SSH      | ssh://git@github.com:ghdl/ghdl.git     |
+----------+----------------------------------------+

.. HINT::

   Execute ``git checkout -b stable <tag>`` after ``git clone``, to checkout a specific version of GHDL.

Command line instructions to clone GHDL with HTTPS protocol:

.. code-block:: Bash

   cd GitRoot
   git clone "https://github.com/ghdl/ghdl.git" ghdl
   cd ghdl
   git remote rename origin github

Command line instructions to clone GHDL with SSH protocol:

.. code-block:: Bash

   cd GitRoot
   git clone "ssh://git@github.com:ghdl/ghdl.git" ghdl
   cd ghdl
   git remote rename origin github

.. NOTE::

   Executing the following instructions in Windows Command Prompt (:program:`cmd.exe`)
   won't function or will result in errors! All Windows command line instructions are
   intended for :program:`Windows PowerShell`, if not marked otherwise. :program:`Windows PowerShell`
   can be installed or upgraded to v5.1 by installing the `Windows Management Framework <https://docs.microsoft.com/en-us/powershell/wmf/5.1/install-configure>`_.
