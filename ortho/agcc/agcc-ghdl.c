/*  Ada bindings for GCC internals.
    Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold

    GHDL is free software; you can redistribute it and/or modify it under
    the terms of the GNU General Public License as published by the Free
    Software Foundation; either version 2, or (at your option) any later
    version.

    GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
    WARRANTY; without even the implied warranty of MERCHANTABILITY or
    FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
    for more details.

    You should have received a copy of the GNU General Public License
    along with GCC; see the file COPYING.  If not, write to the Free
    Software Foundation, 59 Temple Place - Suite 330, Boston, MA
    02111-1307, USA.
*/
#include "config.h"
#include "system.h"
#include "tree.h"
#include "flags.h"
#include <stdio.h>

#if 0
const char *const language_string = "ghdl";
int flag_traditional;
#endif

/* Convertion from a C string to the corresponding cannonical
   Ada (GNAT) String.  */
struct str_template
{
  int first;
  int last;
};

struct str_fatptr
{
  const char *array;
  struct str_template *tpl;
};

#if 0
/* Called by toplev.c, to initialize the parser.  */
const char *
init_parse (const char *filename)
{
  struct str_template temp1 = {1, strlen (filename)};
  struct str_fatptr   fp = {filename, &temp1};

  ghdl1__init_parse (fp);
  return filename;
}
#endif

void
lang_init_options (void)
{
  extern int gnat_argc;
  extern const char **gnat_argv;
  extern const char *progname;

  /* Initialize ada.command_line.  */
  gnat_argc = 1;
  gnat_argv = &progname;

  adainit ();
}

#if 0
/* Decode all the language specific options that cannot be decoded by GCC. The
   option decoding phase of GCC calls this routine on the flags that it cannot
   decode.  Return 1 if successful, otherwise return 0. */

int
lang_decode_option (argc, argv)
     int argc;
     char **argv;
{
  return 0;
}

void
lang_print_xnode(file, t, i)
     FILE *file;
     tree t;
     int i;
{
     return;
}

/* Routines Expected by gcc:  */

/* These are used to build types for various sizes.  The code below
   is a simplified version of that of GNAT.  */

#ifndef MAX_BITS_PER_WORD
#define MAX_BITS_PER_WORD  BITS_PER_WORD
#endif

/* This variable keeps a table for types for each precision so that we only
   allocate each of them once. Signed and unsigned types are kept separate.  */
static tree signed_and_unsigned_types[MAX_BITS_PER_WORD + 1][2];

/* Return an integer type with the number of bits of precision given by
   PRECISION.  UNSIGNEDP is nonzero if the type is unsigned; otherwise
   it is a signed type.  */

tree
type_for_size (precision, unsignedp)
     unsigned precision;
     int unsignedp;
{
  tree t;

  if (precision <= MAX_BITS_PER_WORD
      && signed_and_unsigned_types[precision][unsignedp] != 0)
    return signed_and_unsigned_types[precision][unsignedp];

 if (unsignedp)
    t = signed_and_unsigned_types[precision][1]
      = make_unsigned_type (precision);
  else
    t = signed_and_unsigned_types[precision][0]
      = make_signed_type (precision);

  return t;
}


/* Return a data type that has machine mode MODE.  UNSIGNEDP selects
   an unsigned type; otherwise a signed type is returned.  */

tree
type_for_mode (mode, unsignedp)
     enum machine_mode mode;
     int unsignedp;
{
  return type_for_size (GET_MODE_BITSIZE (mode), unsignedp);
}

/* Return the unsigned version of a TYPE_NODE, a scalar type.  */

tree
unsigned_type (type_node)
     tree type_node;
{
  return type_for_size (TYPE_PRECISION (type_node), 1);
}

/* Return the signed version of a TYPE_NODE, a scalar type.  */

tree
signed_type (type_node)
     tree type_node;
{
  return type_for_size (TYPE_PRECISION (type_node), 0);
}

/* Return a type the same as TYPE except unsigned or signed according to
   UNSIGNEDP.  */

tree
signed_or_unsigned_type (unsignedp, type)
     int unsignedp;
     tree type;
{
  if (! INTEGRAL_TYPE_P (type) || TREE_UNSIGNED (type) == unsignedp)
    return type;
  else
    return type_for_size (TYPE_PRECISION (type), unsignedp);
}

void
init_type_for_size (void)
{
  ggc_add_tree_root (signed_and_unsigned_types,
		     sizeof (signed_and_unsigned_types) / sizeof (tree));
}
#endif


#if 0
/* These functions and variables deal with binding contours.  We only
   need these functions for the list of PARM_DECLs, but we leave the
   functions more general; these are a simplified version of the
   functions from GNAT.  */

/* For each binding contour we allocate a binding_level structure which records
   the entities defined or declared in that contour. Contours include:

        the global one
        one for each subprogram definition
        one for each compound statement (declare block)

   Binding contours are used to create GCC tree BLOCK nodes.  */

struct binding_level
{
  /* A chain of ..._DECL nodes for all variables, constants, functions,
     parameters and type declarations.  These ..._DECL nodes are chained
     through the TREE_CHAIN field. Note that these ..._DECL nodes are stored
     in the reverse of the order supplied to be compatible with the
     back-end.  */
  tree names;
  /* For each level (except the global one), a chain of BLOCK nodes for all
     the levels that were entered and exited one level down from this one.  */
  tree blocks;
  /* The back end may need, for its own internal processing, to create a BLOCK
     node. This field is set aside for this purpose. If this field is non-null
     when the level is popped, i.e. when poplevel is invoked, we will use such
     block instead of creating a new one from the 'names' field, that is the
     ..._DECL nodes accumulated so far.  Typically the routine 'pushlevel'
     will be called before setting this field, so that if the front-end had
     inserted ..._DECL nodes in the current block they will not be lost.   */
  tree block_created_by_back_end;
  /* The binding level containing this one (the enclosing binding level). */
  struct binding_level *level_chain;
};

/* The binding level currently in effect.  */
static struct binding_level *current_binding_level = NULL;

/* The outermost binding level. This binding level is created when the
   compiler is started and it will exist through the entire compilation.  */
static struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */
static struct binding_level clear_binding_level = {NULL, NULL, NULL, NULL};

/* Return non-zero if we are currently in the global binding level.  */

int
global_bindings_p ()
{
  return current_binding_level == global_binding_level ? -1 : 0;
}

/* Return the list of declarations in the current level. Note that this list
   is in reverse order (it has to be so for back-end compatibility).  */

tree
getdecls ()
{
  return current_binding_level->names;
}

/* Nonzero if the current level needs to have a BLOCK made.  */

int
kept_level_p ()
{
  return (current_binding_level->names != 0);
}

/* Enter a new binding level. The input parameter is ignored, but has to be
   specified for back-end compatibility.  */

void
pushlevel (ignore)
     int ignore;
{
  struct binding_level *newlevel
    = (struct binding_level *) xmalloc (sizeof (struct binding_level));

  *newlevel = clear_binding_level;

  /* Add this level to the front of the chain (stack) of levels that are
     active.  */
  newlevel->level_chain = current_binding_level;
  current_binding_level = newlevel;
}

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP is nonzero, this level had explicit declarations, so
   and create a "block" (a BLOCK node) for the level
   to record its declarations and subblocks for symbol table output.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the BLOCK.  */

tree
poplevel (keep, reverse, functionbody)
     int keep;
     int reverse;
     int functionbody;
{
  /* Points to a BLOCK tree node. This is the BLOCK node construted for the
     binding level that we are about to exit and which is returned by this
     routine.  */
  tree block_node = NULL_TREE;
  tree decl_chain;
  tree decl_node;
  tree subblock_chain = current_binding_level->blocks;
  tree subblock_node;
  tree block_created_by_back_end;

  /* Reverse the list of XXXX_DECL nodes if desired.  Note that the ..._DECL
     nodes chained through the `names' field of current_binding_level are in
     reverse order except for PARM_DECL node, which are explicitely stored in
     the right order.  */
  decl_chain = (reverse) ? nreverse (current_binding_level->names)
                         : current_binding_level->names;

  block_created_by_back_end = current_binding_level->block_created_by_back_end;
  if (block_created_by_back_end != 0)
    {
      block_node = block_created_by_back_end;

      /* Check if we are about to discard some information that was gathered
         by the front-end. Nameley check if the back-end created a new block
         without calling pushlevel first. To understand why things are lost
         just look at the next case (i.e. no block created by back-end.  */
      if ((keep || functionbody) && (decl_chain || subblock_chain))
        abort ();
    }

  /* If there were any declarations in the current binding level, or if this
     binding level is a function body, or if there are any nested blocks then
     create a BLOCK node to record them for the life of this function.  */
  else if (keep || functionbody)
    block_node = build_block (keep ? decl_chain : 0, 0, subblock_chain, 0, 0);

  /* Record the BLOCK node just built as the subblock its enclosing scope.  */
  for (subblock_node = subblock_chain; subblock_node;
       subblock_node = TREE_CHAIN (subblock_node))
    BLOCK_SUPERCONTEXT (subblock_node) = block_node;

  /* Clear out the meanings of the local variables of this level.  */

  for (subblock_node = decl_chain; subblock_node;
       subblock_node = TREE_CHAIN (subblock_node))
    if (DECL_NAME (subblock_node) != 0)
      /* If the identifier was used or addressed via a local extern decl,
         don't forget that fact.   */
      if (DECL_EXTERNAL (subblock_node))
        {
          if (TREE_USED (subblock_node))
            TREE_USED (DECL_NAME (subblock_node)) = 1;
          if (TREE_ADDRESSABLE (subblock_node))
            TREE_ADDRESSABLE (DECL_ASSEMBLER_NAME (subblock_node)) = 1;
        }

  /* Pop the current level.  */
  current_binding_level = current_binding_level->level_chain;

  if (functionbody)
    {
      /* This is the top level block of a function. The ..._DECL chain stored
         in BLOCK_VARS are the function's parameters (PARM_DECL nodes). Don't
         leave them in the BLOCK because they are found in the FUNCTION_DECL
         instead.  */
      DECL_INITIAL (current_function_decl) = block_node;
      BLOCK_VARS (block_node) = 0;
    }
  else if (block_node)
    {
      if (block_created_by_back_end == NULL)
        current_binding_level->blocks
          = chainon (current_binding_level->blocks, block_node);
    }

  /* If we did not make a block for the level just exited, any blocks made for
     inner levels (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks of something
     else.  */
  else if (subblock_chain)
    current_binding_level->blocks
      = chainon (current_binding_level->blocks, subblock_chain);
  if (block_node)
    TREE_USED (block_node) = 1;

  return block_node;
}

/* Insert BLOCK at the end of the list of subblocks of the
   current binding level.  This is used when a BIND_EXPR is expanded,
   to handle the BLOCK node inside the BIND_EXPR.  */

void
insert_block (block)
     tree block;
{
  TREE_USED (block) = 1;
  current_binding_level->blocks
    = chainon (current_binding_level->blocks, block);
}

/* Set the BLOCK node for the innermost scope
   (the one we are currently in).  */

void
set_block (block)
     tree block;
{
  current_binding_level->block_created_by_back_end = block;
}

/* Records a ..._DECL node DECL as belonging to the current lexical scope.
   Returns the ..._DECL node. */

tree
pushdecl (decl)
     tree decl;
{
  /* External objects aren't nested, other objects may be.  */
  if (DECL_EXTERNAL (decl))
    DECL_CONTEXT (decl) = 0;
  else
    DECL_CONTEXT (decl) = current_function_decl;

  /* Put the declaration on the list.  The list of declarations is in reverse
     order. The list will be reversed later if necessary.  This needs to be
     this way for compatibility with the back-end.  */

  TREE_CHAIN (decl) = current_binding_level->names;
  current_binding_level->names = decl;

  /* For the declaration of a type, set its name if it is not already set. */

  if (TREE_CODE (decl) == TYPE_DECL
      && TYPE_NAME (TREE_TYPE (decl)) == 0)
    TYPE_NAME (TREE_TYPE (decl)) = decl; /* DECL_NAME (decl); */

  return decl;
}
#endif

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#undef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

#if 0
/* Create the predefined scalar types such as `integer_type_node' needed
   in the gcc back-end and initialize the global binding level.  */

void
init_decl_processing ()
{
  tree endlink;

  error_mark_node = make_node (ERROR_MARK);
  TREE_TYPE (error_mark_node) = error_mark_node;

  initialize_sizetypes ();

  /* The structure `tree_identifier' is the GCC tree data structure that holds
     IDENTIFIER_NODE nodes. We need to call `set_identifier_size' to tell GCC
     that we have not added any language specific fields to IDENTIFIER_NODE
     nodes.  */
  set_identifier_size (sizeof (struct tree_identifier));
  lineno = 0;

  /* Make the binding_level structure for global names.  */
  pushlevel (0);
  global_binding_level = current_binding_level;

  build_common_tree_nodes (0);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("int"),
                        integer_type_node));
  pushdecl (build_decl (TYPE_DECL, get_identifier ("char"),
                        char_type_node));
  set_sizetype (unsigned_type_node);
  build_common_tree_nodes_2 (0);

}
#endif


#if 0
/* Perform all the initialization steps that are language-specific.  */

void
lang_init ()
{}

/* Perform all the finalization steps that are language-specific.  */

void
lang_finish ()
{}

/* Return a short string identifying this language to the debugger.  */

const char *
lang_identify ()
{
  return "vhdl";
}

/* If DECL has a cleanup, build and return that cleanup here.
   This is a callback called by expand_expr.  */

tree
maybe_build_cleanup (decl)
     tree decl;
{ return NULL_TREE; }

/* Print an error message for invalid use of an incomplete type.  */

void
incomplete_type_error (dont_care_1, dont_care_2)
     tree dont_care_1, dont_care_2;
{ abort (); }

tree
truthvalue_conversion (expr)
     tree expr;
{ return expr;}

int
mark_addressable (expr)
     tree expr;
{return 0;}
#endif

#if 0
/* Print any language-specific compilation statistics.  */

void
print_lang_statistics ()
{}

/* Since we don't use the DECL_LANG_SPECIFIC field, this is a no-op.  */

void
copy_lang_decl (node)
     tree node;
{}

/* Hooks for print-tree.c:  */

void
print_lang_decl (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{}

void
print_lang_type (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{}

void
print_lang_identifier (file, node, indent)
     FILE *file;
     tree node;
     int indent;
{}
#endif

#if 0
/* Performs whatever initialization steps are needed by the language-dependent
   lexical analyzer.  */

void
init_lex ()
{}


/* Sets some debug flags for the parser. It does nothing here.  */

void
set_yydebug (value)
     int value;
{}
#endif

#if 0
/* Routine to print parse error message.  */
void
yyerror (str)
     char *str;
{
  fprintf (stderr, "%s\n", str);
}
#endif

#if 0
/* Return the typed-based alias set for T, which may be an expression
   or a type.  Return -1 if we don't do anything special.  */

HOST_WIDE_INT
lang_get_alias_set (t)
     tree t ATTRIBUTE_UNUSED;
{
  return -1;
}
#endif

#if 0
/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  */

tree
builtin_function (name, type, function_code, class, library_name)
     const char *name;
     tree type;
     int function_code;
     enum built_in_class class;
     const char *library_name;
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  if (library_name)
    DECL_ASSEMBLER_NAME (decl) = get_identifier (library_name);
  make_decl_rtl (decl, NULL_PTR, 1);
  pushdecl (decl);
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = function_code;
  return decl;
}
#endif

#if 0
/* Mark language-specific parts of T for garbage-collection.  */

void
lang_mark_tree (t)
     tree t ATTRIBUTE_UNUSED;
{
}
#endif

void
print_chain (tree t)
{
  while (t != NULL)
    {
      print_node_brief (stdout, "", t, 0);
      fprintf (stdout, "\n");
      t = TREE_CHAIN (t);
    }
}
