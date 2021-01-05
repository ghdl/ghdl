.. _DEV:Style:

Coding Style
############

Ada
===

Ada subset: use only a simple (VHDL like) subset of Ada: no tasking, no
controlled types... VHDL users should easily understand that subset.
Allowed Ada95 features: the standard library, child packages.
Use assertions.

We try to follow the 'GNU Coding Standards' when possible: comments before
declarations, one space at the end of sentences, finish sentences with a dot.
But: 2 spaces for indentation in code blocks.

No trailing spaces, no TAB (HT).

Subprograms must have a comment before to describe them, like:

.. code-block:: Ada

   --  Analyze the concurrent statements of PARENT.
   procedure Sem_Concurrent_Statement_Chain (Parent : Iir);

The line before the comment must be a blank line (unless this is the first
declaration). Don't repeat the comment before the subprogram body.

* For subprograms:

  1. Declare on one line when possible:

     .. code-block:: Ada

        function Translate_Static_Aggregate (Aggr : Iir) return O_Cnode

  2. If not possible, put the return on the next line:

     .. code-block:: Ada

        function Translate_Static_String (Str_Type : Iir; Str_Ident : Name_Id)
                                         return O_Cnode

  3. If not possible, put parameters and return on the next line:

     .. code-block:: Ada

        function Create_String_Literal_Var_Inner
          (Str : Iir; Element_Type : Iir; Str_Type : O_Tnode) return Var_Type

  4. If not possible, return on the next line:

     .. code-block:: Ada

        function Translate_Shortcut_Operator
          (Imp : Iir_Implicit_Function_Declaration; Left, Right : Iir)
          return O_Enode

  5. If not possible, one parameter per line, just after subprogram name:

     .. code-block:: Ada

        procedure Translate_Static_Aggregate_1 (List : in out O_Array_Aggr_List;
                                                Aggr : Iir;
                                                Info : Iir;
                                                El_Type : Iir)

  6. If not possible, add a return after subprogram name:

     .. code-block:: Ada

        function Translate_Predefined_TF_Array_Element
          (Op : Predefined_Boolean_Logical;
           Left, Right : Iir;
           Res_Type : Iir;
           Loc : Iir)
          return O_Enode

  7. If not possible, ask yourself what is wrong!  Shorten a name.

* Rule for the 'is': on a new line only if the declarative part is not empty:

     .. code-block:: Ada

        procedure Translate_Assign (Target : Mnode; Expr : Iir; Target_Type : Iir)
        is
          Val : O_Enode;
        begin

  vs.

     .. code-block:: Ada

        function Translate_Static_Range_Dir (Expr : Iir) return O_Cnode is
        begin

  If the parameter line is too long with the 'is', put in on a separate line:

     .. code-block:: Ada

        procedure Predeclare_Scope_Type
          (Scope : in out Var_Scope_Type; Name : O_Ident) is

* Generic instantiation: put the generic actual part on a new line:

     .. code-block:: Ada

        procedure Free is new Ada.Unchecked_Deallocation
          (Action_List, Action_List_Acc);

* For if/then statement:

  1. 'then' on the same line:

     .. code-block:: Ada

        if Get_Expr_Staticness (Decl) = Locally then

  2. If not possible, 'then' is alone on its line aligned with the 'if':

     .. code-block:: Ada

        if Expr = Null_Iir
          or else Get_Kind (Expr) = Iir_Kind_Overflow_Literal
        then

  3. For a multiline condition, 'or else' and 'and then' should start lines.

* 'Local' variable declaration:
  Do not initialize variables, constants must be declared before variables:

     .. code-block:: Ada

        is
          N_Info : constant Iir := Get_Sub_Aggregate_Info (Info);
          Assoc  : Iir;
          Sub    : Iir;
        begin

  If the initialization expression has a side effect (such as allocation), do
  not use a constant.

Shell
=====

Ubuntu uses `dash` instead of `bash` when a shell script is run. As a result, some functionalities, such as arrays like
``array[1]``, are not supported. Therefore, build scripts should not use those functionalities unless
they are sourced in a `bash` shell. The same applies to the scripts in `testsuite`.

Guidelines to edit the documentation
====================================

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

   6) Each backend has one folder and each platform/compiler has one file. Please note that page headlines are different from ToC headline:

      .. code::

         .. toctree::
            :hidden:

            ToC entry <file1>
            file2

   7) Documentation should not use “you”, “we”, …, because it’s not an interactive conversation or informal letter. It’s like a thesis, everything is structured and formal. However, to make it more friendly to newcomers, we agree to allow informal language in the Quick Start Guide.

   8) Please keep errors to a minimum.

Guidelines to edit section 'Building'
-------------------------------------

We prefer a text block, which explains how a compilation works, what we can configure for that backend, etc. After that, we prefer a code block with e.g. bash instructions on how to compile a backend. A list of instructions with embedded bash lines is not helpful. An experienced, as well as novice user, would like to copy a set of instructions into the shell. But it should be stated what these instructions will do. Complex flows like for GCC, can be split into multiple shell code blocks. Moreover, we find it essential to demonstrate when and where to change directories.

We would like to see a list like:

* gcc (Gnu Compiler Collection)
* gcc-gnat (Ada compiler for GCC)
* llvm-del (LLVM development package)
* ...

The goal is also to explain what a user is installing and what the few lines in the build description do. Now they know the name, can search for similar names if they have another package manager or distro or can ask Google/Wikipedia. We often find many build receipts with cryptic shell code and to execute this unknown stuff with sudo is not comfortable. We would like to know what it does before hitting enter.

Documentation configuration
===========================

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
