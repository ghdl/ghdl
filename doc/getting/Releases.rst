.. _RELEASE:

Releases
########

.. contents:: Contents of this Page
   :local:

.. _RELEASE:packages:

Downloading pre-build packages
******************************

.. |Ubu1204-llvm38| image:: https://img.shields.io/github/downloads/tgingold/ghdl/2017-03-01/ghdl-0.34-dev-llvm-3.8-2017-03-01-ubu1204.tgz.svg
   :target: https://github.com/tgingold/ghdl/releases/download/2017-03-01/ghdl-0.34-dev-llvm-3.8-2017-03-01-ubu1204.tgz
   :alt: ghdl-0.34-dev-llvm-3.5-2017-03-01-ubu1404.tgz
.. |Ubu1404-llvm35| image:: https://img.shields.io/github/downloads/tgingold/ghdl/2017-03-01/ghdl-0.34-dev-llvm-3.5-2017-03-01-ubu1404.tgz.svg
   :target: https://github.com/tgingold/ghdl/releases/download/2017-03-01/ghdl-0.34-dev-llvm-3.5-2017-03-01-ubu1404.tgz
   :alt: ghdl-0.34-dev-llvm-3.5-2017-03-01-ubu1404.tgz
.. |Fed-llvm| image:: https://img.shields.io/github/downloads/tgingold/ghdl/2017-03-01/ghdl-0.34-dev-llvm-2017-03-01-fed.tgz.svg
   :target: https://github.com/tgingold/ghdl/releases/download/2017-03-01/ghdl-0.34-dev-llvm-2017-03-01-fed.tgz
   :alt: ghdl-0.34-dev-llvm-2017-03-01-fed.tgz
.. |Fed-mcode| image:: https://img.shields.io/github/downloads/tgingold/ghdl/2017-03-01/ghdl-0.34-dev-mcode-2017-03-01-fed.tgz.svg
   :target: https://github.com/tgingold/ghdl/releases/download/2017-03-01/ghdl-0.34-dev-mcode-2017-03-01-fed.tgz
   :alt: ghdl-0.34-dev-mcode-2017-03-01-fed.tgz
.. |Win32-mcode| image:: https://img.shields.io/github/downloads/tgingold/ghdl/2017-03-01/ghdl-0.34-dev-mcode-2017-03-01-win32.tgz.svg
   :target: https://github.com/tgingold/ghdl/releases/download/2017-03-01/ghdl-0.34-dev-mcode-2017-03-01-win32.tgz
   :alt: ghdl-0.34-dev-mcode-2017-03-01-win32.tgz

+--------------+----------+-----------+--------------------+
| OS           | Backend  | Filesize  | Downloads          |
+==============+==========+===========+====================+
| Ubuntu 12.04 | LLVM 3.8 | 13.8 MiB  | |Ubu1204-llvm38|   |
+--------------+----------+-----------+--------------------+
| Ubuntu 14.04 | LLVM 3.5 | 11.9 MiB  | |Ubu1404-llvm35|   |
+--------------+----------+-----------+--------------------+
| Fedora       | LLVM     | 6.58 MiB  | |Fed-llvm|         |
+--------------+----------+-----------+--------------------+
| Fedora       | mcode    | 2.75 MiB  | |Fed-mcode|        |
+--------------+----------+-----------+--------------------+
| Windows x86  | mcode    | 5.25 MiB  | |Win32-mcode|      |
+--------------+----------+-----------+--------------------+



.. _RELEASE:Sources:

Downloading Source Files
************************

.. _RELEASE:Sources:Zip:

Downloading from GitHub
=======================

GHDL can be downloaded as a zip-file from GitHub. See the following table, to
choose your desired git branch.

.. |zip-master| image:: /_static/icons/ZIP.png
   :scale: 40
   :target: https://github.com/tgingold/ghdl/archive/master.zip
   :alt: Source Code from GitHub - 'master' branch.
.. |zip-release| image:: /_static/icons/ZIP.png
   :scale: 40
   :target: https://github.com/tgingold/ghdl/archive/release.zip
   :alt: Source Code from GitHub - 'release' branch.

+----------+------------------------+
| Branch   | Download Link          |
+==========+========================+
| master   | zip-file |zip-master|  |
+----------+------------------------+
| release  | zip-file |zip-release| |
+----------+------------------------+


.. _RELEASE:Sources:GitClone:

Downloading via ``git clone``
=============================

GHDL can be downloaded (cloned) with ``git clone`` from GitHub. GitHub offers
the transfer protocols HTTPS and SSH. You should use SSH if you have a GitHub
account and have already uploaded an OpenSSH public key to GitHub, otherwise
use HTTPS if you have no account or you want to use login credentials.

The created folder :file:`<GitRoot>\\ghdl` is used as :file:`<GHDLRoot>` in
later instructions or on other pages in this documentation.

+----------+----------------------------------------+
| Protocol | GitHub Repository URL                  |
+==========+========================================+
| HTTPS    | https://github.com/tgingold/ghdl.git   |
+----------+----------------------------------------+
| SSH      | ssh://git@github.com:tgingold/ghdl.git |
+----------+----------------------------------------+


On Linux
--------

Command line instructions to clone GHDL onto a Linux machine with HTTPS
protocol:

.. code-block:: Bash

   cd GitRoot
   git clone "https://github.com/tgingold/ghdl.git" ghdl
   cd ghdl
   git remote rename origin github

Command line instructions to clone GHDL onto a Linux machine machine with SSH
protocol:

.. code-block:: Bash

   cd GitRoot
   git clone "ssh://git@github.com:tgingold/ghdl.git" ghdl
   cd ghdl
   git remote rename origin github


On OS X
-------

Please see the Linux instructions.


On Windows
----------

.. NOTE::

   All Windows command line instructions are intended for :program:`Windows PowerShell`,
   if not marked otherwise. So executing the following instructions in Windows
   Command Prompt (:program:`cmd.exe`) won't function or result in errors! See
   the :ref:`Requirements section <GETTING:Require>` on where to
   download or update PowerShell.

Command line instructions to clone GHDL onto a Windows machine with HTTPS
protocol:

.. code-block:: PowerShell

   cd GitRoot
   git clone "https://github.com/tgingold/ghdl.git" ghdl
   cd ghdl
   git remote rename origin github

Command line instructions to clone GHDL onto a Windows machine with SSH
protocol:

.. code-block:: PowerShell

   cd GitRoot
   git clone "ssh://git@github.com:tgingold/ghdl.git" ghdl
   cd ghdl
   git remote rename origin github



---------------------------------------------------------------------

.. TODO::
 
  - Naming:
	- branch ghdl-X.Y
	- tag vX.Y
  - stable, development, nightly