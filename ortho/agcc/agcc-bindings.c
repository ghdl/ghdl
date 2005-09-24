/*  Ada bindings for GCC internals - Bindings for Ada.
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
#include <stddef.h>
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "tm_p.h"
#include "defaults.h"
#include "ggc.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "real.h"
#include "toplev.h"

enum tree_code
get_tree_code (tree t)
{
  return TREE_CODE (t);
}

void
set_tree_constant (tree t, int flag)
{
  TREE_CONSTANT (t) = flag;
}

int
get_tree_constant (tree t)
{
  return TREE_CONSTANT (t);
}

void
set_tree_public (tree t, int flag)
{
  TREE_PUBLIC (t) = flag;
}

void
set_tree_static (tree t, int flag)
{
  TREE_STATIC (t) = flag;
}

void
set_tree_type (tree t, tree val)
{
  TREE_TYPE (t) = val;
}

tree
get_tree_type (tree t)
{
  return TREE_TYPE (t);
}

void
set_tree_chain (tree t, tree chain)
{
  TREE_CHAIN (t) = chain;
}

tree
get_tree_chain (tree t)
{
  return TREE_CHAIN (t);
}

void
set_tree_unsigned (tree t, int flag)
{
  TREE_UNSIGNED (t) = flag;
}

int
get_tree_unsigned (tree t)
{
  return TREE_UNSIGNED (t);
}

void
set_tree_addressable (tree t, int flag)
{
  TREE_ADDRESSABLE (t) = flag;
}

int
get_tree_addressable (tree t)
{
  return TREE_ADDRESSABLE (t);
}

void
set_tree_side_effects (tree t, int flag)
{
  TREE_SIDE_EFFECTS (t) = flag;
}

void
set_tree_readonly (tree t, int flag)
{
  TREE_READONLY (t) = flag;
}

void
set_tree_operand (tree t, unsigned int n, tree val)
{
  TREE_OPERAND (t, n) = val;
}

tree
get_tree_operand (tree t, unsigned int n)
{
  return TREE_OPERAND (t, n);
}

int
get_tree_this_volatile (tree t)
{
  return TREE_THIS_VOLATILE (t);
}

int
set_tree_this_volatile (tree t, int val)
{
  TREE_THIS_VOLATILE (t) = val;
}

tree
get_tree_purpose (tree l)
{
  return TREE_PURPOSE (l);
}

tree
get_tree_value (tree l)
{
  return TREE_VALUE (l);
}

int
get_tree_used (tree n)
{
  return TREE_USED (n);
}

void
set_tree_used (tree n, int flag)
{
  TREE_USED (n) = flag;
}

HOST_WIDE_INT
get_tree_int_cst_low (tree node)
{
  return TREE_INT_CST_LOW (node);
}

HOST_WIDE_INT
get_tree_int_cst_high (tree node)
{
  return TREE_INT_CST_HIGH (node);
}

tree
get_constructor_elts (tree c)
{
  return CONSTRUCTOR_ELTS (c);
}

tree
(build_int_2) (HOST_WIDE_INT lo, HOST_WIDE_INT hi)
{
  return build_int_2 (lo, hi);
}

void
set_decl_arg_type (tree decl, tree val)
{
  DECL_ARG_TYPE (decl) = val;
}

void
set_decl_external (tree decl, int val)
{
  DECL_EXTERNAL (decl) = val;
}

int
get_decl_external (tree decl)
{
  return DECL_EXTERNAL (decl);
}

void
set_decl_arguments (tree decl, tree args)
{
  DECL_ARGUMENTS (decl) = args;
}

tree
get_decl_arguments (tree decl)
{
  return DECL_ARGUMENTS (decl);
}

void
set_decl_result (tree decl, tree res)
{
  DECL_RESULT (decl) = res;
}

tree
get_decl_result (tree decl)
{
  return DECL_RESULT (decl);
}

void
set_decl_context (tree decl, tree context)
{
  DECL_CONTEXT (decl) = context;
}

tree
get_decl_context (tree decl)
{
  return DECL_CONTEXT (decl);
}

void
set_decl_initial (tree decl, tree res)
{
  DECL_INITIAL (decl) = res;
}

tree
get_decl_initial (tree decl)
{
  return DECL_INITIAL (decl);
}

tree
get_decl_name (tree decl)
{
  return DECL_NAME (decl);
}

tree
get_decl_assembler_name (tree decl)
{
  return DECL_ASSEMBLER_NAME (decl);
}

void
set_DECL_ASSEMBLER_NAME (tree decl, tree name)
{
  SET_DECL_ASSEMBLER_NAME (decl, name);
}

void
set_decl_built_in_class (tree decl, enum built_in_class class)
{
  DECL_BUILT_IN_CLASS (decl) = class;
}

void
set_decl_function_code (tree decl, int code)
{
  DECL_FUNCTION_CODE (decl) = code;
}

tree
get_decl_field_offset (tree decl)
{
  return DECL_FIELD_OFFSET (decl);
}

tree
get_decl_field_bit_offset (tree decl)
{
  return DECL_FIELD_BIT_OFFSET (decl);
}

int
integral_type_p (tree type)
{
  return INTEGRAL_TYPE_P (type);
}

void
set_type_values (tree type, tree values)
{
  TYPE_VALUES (type) = values;
}

void
set_type_name (tree type, tree name)
{
  TYPE_NAME (type) = name;
}

tree
get_type_name (tree type)
{
  return TYPE_NAME (type);
}

void
set_type_min_value (tree type, tree val)
{
  TYPE_MIN_VALUE (type) = val;
}

tree
get_type_min_value (tree type)
{
  return TYPE_MIN_VALUE (type);
}

void
set_type_max_value (tree type, tree val)
{
  TYPE_MAX_VALUE (type) = val;
}

tree
get_type_max_value (tree type)
{
  return TYPE_MAX_VALUE (type);
}

void
set_type_size (tree type, tree size)
{
  TYPE_SIZE (type) = size;
}

tree
get_type_size (tree type)
{
  return TYPE_SIZE (type);
}

void
set_type_precision (tree type, int precision)
{
  TYPE_PRECISION (type) = precision;
}

int
get_type_precision (tree type)
{
  return TYPE_PRECISION (type);
}

void
set_type_fields (tree type, tree fields)
{
  TYPE_FIELDS (type) = fields;
}

tree
get_type_fields (tree type)
{
  return TYPE_FIELDS (type);
}

void
set_type_stub_decl (tree type, tree decl)
{
  TYPE_STUB_DECL (type) = decl;
}

tree
get_type_domain (tree type)
{
  return TYPE_DOMAIN (type);
}

void
set_type_domain (tree type, tree domain)
{
  TYPE_DOMAIN (type) = domain;
}

void *
get_type_lang_specific (tree node)
{
  return TYPE_LANG_SPECIFIC (node);
}

void
set_type_lang_specific (tree node, void *val)
{
  TYPE_LANG_SPECIFIC (node) = val;
}

int
get_type_is_sizetype (tree node)
{
  return TYPE_IS_SIZETYPE (node);
}

void
set_type_pointer_to (tree node, tree dnode)
{
  TYPE_POINTER_TO (node) = dnode;
}

tree
get_type_pointer_to (tree node)
{
  return TYPE_POINTER_TO (node);
}

enum machine_mode
get_type_mode (tree node)
{
  return TYPE_MODE (node);
}

void
set_type_mode (tree node, enum machine_mode mode)
{
  TYPE_MODE (node) = mode;
}

void
set_current_function_decl (tree decl)
{
  current_function_decl = decl;
}

tree
get_current_function_decl (void)
{
  return current_function_decl;
}

int
double_type_size (void)
{
  return DOUBLE_TYPE_SIZE;
}

int
bits_per_unit (void)
{
  return BITS_PER_UNIT;
}

tree
(size_int) (HOST_WIDE_INT number)
{
  return size_int (number);
}

tree
get_type_size_unit (tree node)
{
  return TYPE_SIZE_UNIT (node);
}

/* For agcc.real:  */
REAL_VALUE_TYPE
get_REAL_VALUE_ATOF (const char *s, enum machine_mode mode)
{
  return REAL_VALUE_ATOF (s, mode);
}

REAL_VALUE_TYPE
get_REAL_VALUE_LDEXP (REAL_VALUE_TYPE x, int n)
{
  REAL_VALUE_TYPE res;
  real_ldexp (&res, &x, n);
  return res;
}

void
get_REAL_VALUE_FROM_INT (REAL_VALUE_TYPE *d, HOST_WIDE_INT l, HOST_WIDE_INT h,
			 enum machine_mode mode)
{
  REAL_VALUE_FROM_INT (*d, l, h, mode);
}

int
get_identifier_length (tree node)
{
  return IDENTIFIER_LENGTH (node);
}

const char *
get_identifier_pointer (tree node)
{
  return IDENTIFIER_POINTER (node);
}

tree
get_block_supercontext (tree node)
{
  return BLOCK_SUPERCONTEXT (node);
}

void
set_block_supercontext (tree block, tree sc)
{
  BLOCK_SUPERCONTEXT (block) = sc;
}

void
set_block_vars (tree block, tree vars)
{
  BLOCK_VARS (block) = vars;
}

const int tree_identifier_size = sizeof (struct tree_identifier);

#if 0
static void
ggc_mark_tree_ptr (void *elt)
{
  ggc_mark_tree (*(tree *) elt);
}
#endif

#undef ggc_mark_tree
void
ggc_mark_tree (tree expr)
{
  gt_ggc_m_9tree_node (expr);
}

#if 0
void
ggc_add_tree_root (void *base, int nelt)
{
  ggc_add_root (base, nelt, sizeof (tree), ggc_mark_tree_ptr);
}
#endif

int
get_mode_bitsize (enum machine_mode mode)
{
  return GET_MODE_BITSIZE (mode);
}

int
get_errorcount (void)
{
  return errorcount;
}

void
set_errorcount (int c)
{
  errorcount = c;
}


/* Defined in agcc.fe  */
extern const char language_name[];
extern bool lang_init (void);
extern void lang_finish (void);
extern unsigned int lang_init_options (unsigned int argc, const char **argv);
extern int lang_handle_option (size_t code, const char *argc, int value);
extern bool lang_post_options (const char **);
extern HOST_WIDE_INT lang_get_alias_set (tree t);
extern bool mark_addressable (tree t);

extern int global_bindings_p (void);
extern int kept_level_p (void);
extern tree getdecls (void);
extern void pushlevel (int);
extern tree poplevel (int, int, int);
extern void insert_block (tree);
extern void set_block (tree);
extern tree pushdecl (tree);

extern tree type_for_mode (enum machine_mode, int);
extern tree type_for_size (unsigned int, int);
extern tree unsigned_type (tree);
extern tree signed_type (tree);
extern tree signed_or_unsigned_type (int, tree);
extern tree truthvalue_conversion (tree);
extern void lang_parse_file (int);

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME language_name
#undef LANG_HOOKS_IDENTIFIER_SIZE
#define LANG_HOOKS_IDENTIFIER_SIZE sizeof (struct tree_identifier)
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT lang_init
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH lang_finish
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS lang_init_options
#undef LANG_HOOKS_HANDLE_OPTION
#define LANG_HOOKS_HANDLE_OPTION lang_handle_option
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS lang_post_options
#undef LANG_HOOKS_GET_ALIAS_SET
#define LANG_HOOKS_GET_ALIAS_SET lang_get_alias_set
#undef LANG_HOOKS_HONOR_READONLY
#define LANG_HOOKS_HONOR_READONLY true
#undef LANG_HOOKS_TRUTHVALUE_CONVERSION
#define LANG_HOOKS_TRUTHVALUE_CONVERSION truthvalue_conversion
#undef LANG_HOOKS_MARK_ADDRESSABLE
#define LANG_HOOKS_MARK_ADDRESSABLE mark_addressable

#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE type_for_mode
#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE type_for_size
#undef LANG_HOOKS_SIGNED_TYPE
#define LANG_HOOKS_SIGNED_TYPE signed_type
#undef LANG_HOOKS_UNSIGNED_TYPE
#define LANG_HOOKS_UNSIGNED_TYPE unsigned_type
#undef LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE
#define LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE signed_or_unsigned_type
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE lang_parse_file

const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* Tree code classes.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

const char tree_code_type[] = {
#include "tree.def"
  'x'
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

const unsigned char tree_code_length[] = {
#include "tree.def"
  0
};
#undef DEFTREECODE

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) NAME,
const char * const tree_code_name[] = {
#include "tree.def"
  "@@dummy"
};
#undef DEFTREECODE

union lang_tree_node 
  GTY((desc ("0"),
       chain_next ("(union lang_tree_node *)TREE_CHAIN (&%h.generic)")))
{
  union tree_node GTY ((tag ("0"), 
			desc ("tree_node_structure (&%h)"))) 
    generic;
};

struct lang_decl GTY(())
{
};

struct lang_type GTY (())
{
};

struct language_function GTY (())
{
};
 
tree
c_common_truthvalue_conversion (tree expr)
{
  if (TREE_CODE (TREE_TYPE (expr)) == BOOLEAN_TYPE)
    return expr;
  if (TREE_CODE (expr) == INTEGER_CST)
    return integer_zerop (expr) ? integer_zero_node : integer_one_node;
  
  abort ();
}

int
get_PROMOTE_PROTOTYPES (void)
{
  return PROMOTE_PROTOTYPES;
}

struct binding_level GTY(())
{
  tree names;
  tree blocks;
  tree block_created_by_back_end;
  struct binding_level *level_chain;
};

extern GTY(()) struct binding_level *current_binding_level;
extern GTY((deletable (""))) struct binding_level *old_binding_level;

struct binding_level *
alloc_binding_level (void)
{
  return (struct binding_level *)ggc_alloc (sizeof (struct binding_level));
}

#ifndef MAX_BITS_PER_WORD
#define MAX_BITS_PER_WORD  BITS_PER_WORD
#endif

extern GTY(()) tree signed_and_unsigned_types[MAX_BITS_PER_WORD + 1][2];

#include "debug.h"
#include "gt-vhdl-agcc-bindings.h"
#include "gtype-vhdl.h"

