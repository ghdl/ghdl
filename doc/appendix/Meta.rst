.. _CHANGE:Roadmap:

Meta
####

General guidelines to edit the documentation
********************************************

   1) It’s better for version control systems and diff tools to break lines at a sensible number of characters. Long lines appear as one diff. Also merging is more complex because merges are line based. Long unbreakable items may be longer (links, refs, etc.). We chose to use 160 chars.
   2) Please indent all directive content by 3 spaces (not 2, and no tabs).
   3) Please use ``*`` as an itemize character, since ``-`` and ``+`` are supported by docutils, but not officially supported by Sphinx.
   4) Please underline all headlines with at least as many characters as the headline is long. Following the Python pattern for headlines the levels are:

      .. code::
	  
         ############
         ************ (sometimes skipped in small documents)
         ============
         -------------------
         ‘’’’’’’’’’’’’’’’’’’’’’’’
	  
   5) It’s not required to write
   
      .. code::
	    
		 :samp:`code`
	   
      The default role for
	   
	  .. code::
	   
	     ``code``
		  
      is samp. ``:samp:`` is only required when you want to write italic text in code text.
	  
	  .. code::

         :samp:`print 1+{variable}`

      Now, variable becomes italic.

      Please simplify all usages of ``:samp:`code``` to ````code```` for readability. Here are the regular expressions for an editor like Notepad++:
	  
      - Search pattern:: :samp:`(.+?)`
		 
      - Replace pattern:: ``\1`` 

   6) Each backend has one folder and each platform/compiler has one file. Please note that page headlines are different from ToC headlines: 

      .. code::   

         .. toctree::
            :hidden:

            ToC entry <file1>
            file2

   7) Documentation should not use “you”, “we”, …, because it’s not an interactive conversation or informal letter. It’s like a thesis, everything is structured and formal. However, to make it more friendly to newcomers, we agree to allow informal language in the section :ref:`USING:QuickStart`.

   8) Please keep errors to a minimum.
	  

Guidelines to edit section 'Building'
*************************************

We prefer a text block, which explains how a compilation works, what we can configure for that backend, etc. After that, we prefer a code block with e.g. bash instructions on how to compile a backend. A list of instructions with embedded bash lines is not helpful. An experienced, as well as novice user, would like to copy a set of instructions into the shell. But it should be stated what these instructions will do. Complex flows like for GCC, can be split into multiple shell code blocks. Moreover, we find it essential to demonstrate when and where to change directories.

We would like to see a list like:

* gcc (Gnu Compiler Collection)
* gcc-gnat (Ada compiler for GCC)
* llvm-del (LLVM development package)
* ...

The goal is also to explain what a user is installing and what the few lines in the build description do. Now they know the name, can search for similar names if they have another package manager or distro or can ask Google/Wikipedia. We often find many build receipts with cryptic shell code and to execute this unknown stuff with sudo is not comfortable. We would like to know what it does before hitting enter.
	  
Documentation configuration
***************************
	  
* Python snippet for Sphinx's `conf.py` to extract the current version number from Git (latest tag name). [:ghdlsharp:`200`, :ghdlsharp:`221`]

* Reference ``genindex.html`` from the navigation bar. [:ghdlsharp:`200`]

* Create "parts" (LaTeX terminology / chapter headlines) in navigation bar. [:ghdlsharp:`200`]
	
* Intersphinx files [:ghdlsharp:`200`]
	* To decompress the inventory file: `curl -s http://ghdl.readthedocs.io/en/latest/objects.inv | tail -n+5 | openssl zlib -d`. From `how-to-uncompress-zlib-data-in-unix <http://unix.stackexchange.com/questions/22834/how-to-uncompress-zlib-data-in-unix>`_.
	* External ref and link to section::
	
		:ref:`GHDL Roadmap <ghdl:CHANGE:Roadmap>`
		
	* External ref to option (no link)::
	
		:ghdl:option:`--ieee`
		:option:`ghdl:--ieee`

CSS
***

* The indentation of the elements in the side menu have been modified. They are fixed for levels 1, 2 and 3 (`#294 <https://github.com/ghdl/ghdl/pull/294#issuecomment-281555760>`_) and 4 (later).

* The RTD menu (bottom-left) has been modified (`#294 <https://github.com/ghdl/ghdl/pull/294#issuecomment-281513494>`_):

   * No headlines are shown. It is not possible to remove only one of them with CSS only (JS would be required). However, because the content in most of the lines is self-explained, it is preferred not to show any.
   * The Search box is removed.
	
Dist
****
		
* Ubuntu uses `dash` instead of `bash` when a shell script is run. As a result, some functionalities, such as arrays like ``array[1]``, are not supported. Therefore, build scripts in `dist/linux` should not use those functionalities unless they are sourced in a `bash` shell. That is, :file:`travis-ci.sh` uses arrays, since it is sourced in the Travis CI machine. But :file:`docker-buildtest.sh` and :file:`buildtest.sh` do not use any. The same applies to the scripts in `testsuite`.