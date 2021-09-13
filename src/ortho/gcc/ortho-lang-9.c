/* GCC back-end for ortho
  Copyright (C) 2002-1014 Tristan Gingold and al.

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <gnu.org/licenses>.
*/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "hash-set.h"
#include "machmode.h"
#include "vec.h"
#include "double-int.h"
#include "input.h"
#include "alias.h"
#include "symtab.h"
#include "wide-int.h"
#include "inchash.h"
#include "real.h"
#include "tree.h"

#include "bitmap.h"
#include "hash-map.h"
#include "is-a.h"
#include "plugin-api.h"
#include "hard-reg-set.h"
#include "input.h"
#include "function.h"
#include "ipa-ref.h"
#include "cgraph.h"

#include "fold-const.h"

#include <stddef.h>
#include <math.h>

#include "tm_p.h"
#include "defaults.h"
#include "ggc.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "langhooks-def.h"
#include "toplev.h"
#include "opts.h"
#include "options.h"
#include "tree-iterator.h"
#include "target.h"
#include "convert.h"
#include "tree-pass.h"
#include "tree-dump.h"

#include "print-tree.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "varasm.h"

#define TYPE_UNBOUNDED(t) TYPE_LANG_FLAG_0(t)

/* Returns the number of FIELD_DECLs in TYPE.
   Copied here from expr.c in gcc4.9 as it is no longer exported by tree.h.  */

static int
fields_length (const_tree type)
{
  tree t = TYPE_FIELDS (type);
  int count = 0;

  for (; t; t = DECL_CHAIN (t))
    if (TREE_CODE (t) == FIELD_DECL)
      ++count;

  return count;
}

/* TODO:
 * remove stmt_list_stack, save in if/case/loop block
 * Re-add -v (if necessary)
 */

static tree type_for_size (unsigned int precision, int unsignedp);

const int tree_identifier_size = sizeof (struct tree_identifier);

struct GTY(()) binding_level
{
  /*  The BIND_EXPR node for this binding.  */
  tree bind;

  /*  The BLOCK node for this binding.  */
  tree block;

  /*  If true, stack must be saved (alloca is used).  */
  int save_stack;

  /*  Parent binding level.  */
  struct binding_level *prev;

  /*  Decls in this binding.  */
  tree first_decl;
  tree last_decl;

  /*  Blocks in this binding.  */
  tree first_block;
  tree last_block;

  /* Statements list containing the binding. */
  tree prev_stmts;
};

/*  The current binding level.  */
static GTY(()) struct binding_level *cur_binding_level = NULL;

/*  Chain of unused binding levels.  */
static GTY(()) struct binding_level *old_binding_levels = NULL;

/*  Chain of statements currently generated.  */
static GTY(()) tree cur_stmts = NULL_TREE;

enum binding_kind { GLOBAL_BINDING, FUNCTION_BINDING, LOCAL_BINDING };

static void
push_binding (enum binding_kind kind)
{
  struct binding_level *res;

  /* Get a binding level (old ones are recycled).  */
  if (old_binding_levels == NULL)
    res = ggc_alloc<binding_level> ();
  else
    {
      res = old_binding_levels;
      old_binding_levels = res->prev;
    }

  /* Init.  */
  res->first_decl = NULL_TREE;
  res->last_decl = NULL_TREE;

  res->first_block = NULL_TREE;
  res->last_block = NULL_TREE;

  res->save_stack = 0;

  switch (kind)
    {
    case GLOBAL_BINDING:
      res->bind = NULL_TREE;
      res->block = NULL_TREE;
      res->prev = NULL;
      res->prev_stmts = NULL;
      break;
    case FUNCTION_BINDING:
    case LOCAL_BINDING:
      res->block = make_node (BLOCK);
      TREE_USED (res->block) = true;
      res->bind = build3 (BIND_EXPR, void_type_node,
			  NULL_TREE, NULL_TREE, res->block);
      TREE_SIDE_EFFECTS (res->bind) = true;
      res->prev_stmts = cur_stmts;
      cur_stmts = alloc_stmt_list ();
      break;
    }

  switch (kind)
    {
    case GLOBAL_BINDING:
      /* No supercontext for the global binding.  */
      break;
    case FUNCTION_BINDING:
      /* No containing block.  */
      BLOCK_SUPERCONTEXT (res->block) = current_function_decl;
      break;
    case LOCAL_BINDING:
      /* Append the block created.  */
      if (cur_binding_level->first_block == NULL)
	cur_binding_level->first_block = res->block;
      else
	BLOCK_CHAIN (cur_binding_level->last_block) = res->block;
      cur_binding_level->last_block = res->block;

      BLOCK_SUPERCONTEXT (res->block) = cur_binding_level->block;
      break;
    }

  /* Chain previous binding, set current binding.  */
  res->prev = cur_binding_level;
  cur_binding_level = res;
}

static tree
pushdecl (tree decl)
{
  /* Set context (always a function or NULL if top-level).  */
  DECL_CONTEXT (decl) = current_function_decl;

  /* Chain the declaration.  */
  if (cur_binding_level->first_decl == NULL)
    cur_binding_level->first_decl = decl;
  else
    TREE_CHAIN (cur_binding_level->last_decl) = decl;
  cur_binding_level->last_decl = decl;

  return decl;
}

static tree
pop_binding (void)
{
  tree res;
  struct binding_level *cur;

  cur = cur_binding_level;
  res = cur->bind;

  if (cur->save_stack)
    {
      tree tmp_var;
      tree save;
      tree save_call;
      tree restore;
      tree t;

      /* Create an artificial var to save the stack pointer.  */
      tmp_var = build_decl (input_location, VAR_DECL, NULL, ptr_type_node);
      DECL_ARTIFICIAL (tmp_var) = true;
      DECL_IGNORED_P (tmp_var) = true;
      TREE_USED (tmp_var) = true;
      pushdecl (tmp_var);

      /* Create the save stmt.  */
      save_call = build_call_expr
	(builtin_decl_implicit (BUILT_IN_STACK_SAVE), 0);
      save = build2 (MODIFY_EXPR, ptr_type_node, tmp_var, save_call);
      TREE_SIDE_EFFECTS (save) = true;

      /* Create the restore stmt.  */
      restore = build_call_expr
	(builtin_decl_implicit (BUILT_IN_STACK_RESTORE), 1, tmp_var);

      /* Build a try-finally block.
	 The statement list is the block of current statements.  */
      t = build2 (TRY_FINALLY_EXPR, void_type_node, cur_stmts, NULL_TREE);
      TREE_SIDE_EFFECTS (t) = true;

      /* The finally block is the restore stmt.  */
      append_to_statement_list (restore, &TREE_OPERAND (t, 1));

      /* The body of the BIND_BLOCK is the save stmt, followed by the
	 try block.  */
      BIND_EXPR_BODY (res) = NULL_TREE;
      append_to_statement_list (save, &BIND_EXPR_BODY (res));
      append_to_statement_list (t, &BIND_EXPR_BODY (res));
    }
  else
    {
      /* The body of the BIND_BLOCK is the statement block.  */
      BIND_EXPR_BODY (res) = cur_stmts;
    }
  BIND_EXPR_VARS (res) = cur->first_decl;

  BLOCK_SUBBLOCKS (cur->block) = cur->first_block;
  BLOCK_VARS (cur->block) = cur->first_decl;

  /* Set current statements list and current binding.  */
  cur_stmts = cur->prev_stmts;
  cur_binding_level = cur->prev;

  /* Put removed binding to the recycle list.  */
  cur->prev = old_binding_levels;
  old_binding_levels = cur;

  return res;
}

static void
append_stmt (tree stmt)
{
  /* Set location (if not done).  */
  if (!EXPR_HAS_LOCATION (stmt))
    SET_EXPR_LOCATION (stmt, input_location);

  TREE_SIDE_EFFECTS (stmt) = true;
  append_to_statement_list (stmt, &cur_stmts);
}

static GTY(()) tree stack_alloc_function_ptr;

static bool
global_bindings_p (void)
{
  return cur_binding_level->prev == NULL;
}

/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.  */
static void
define_builtin (const char *name,
		tree type,
		enum built_in_function code,
		const char *library_name,
		int attr)
{
  tree decl;

  decl = add_builtin_function (name, type, code, BUILT_IN_NORMAL,
			       library_name, NULL_TREE);
  set_call_expr_flags (decl, attr);

  set_builtin_decl (code, decl, true);
}

static REAL_VALUE_TYPE fp_const_m_p5; /* -0.5 */

static bool
ortho_init (void)
{
  tree n;

  input_location = BUILTINS_LOCATION;

  /* Create a global binding.  Don't use push_binding, as neither a BLOCK nor
     a BIND_EXPR are needed.  */
  push_binding (GLOBAL_BINDING);

  build_common_tree_nodes (false);

  n = build_decl (input_location,
                  TYPE_DECL, get_identifier ("int"), integer_type_node);
  pushdecl (n);
  n = build_decl (input_location,
                  TYPE_DECL, get_identifier ("char"), char_type_node);
  pushdecl (n);

  /* Create alloca builtin.  */
  {
    tree args_type = tree_cons (NULL_TREE, size_type_node, void_list_node);
    tree func_type = build_function_type (ptr_type_node, args_type);

    define_builtin ("__builtin_alloca", func_type,
		    BUILT_IN_ALLOCA, NULL, 0);

    stack_alloc_function_ptr = build1
      (ADDR_EXPR,
       build_pointer_type (func_type),
       builtin_decl_implicit (BUILT_IN_ALLOCA));
  }

  {
    tree ptr_ftype = build_function_type (ptr_type_node, NULL_TREE);

    define_builtin ("__builtin_stack_save", ptr_ftype,
		    BUILT_IN_STACK_SAVE, NULL, 0);
  }

  {
    tree ftype_ptr = build_function_type_list (void_type_node,
					       ptr_type_node, NULL_TREE);

    define_builtin ("__builtin_stack_restore", ftype_ptr,
		    BUILT_IN_STACK_RESTORE, NULL, 0);
  }

  {
    tree ftype_ptr = build_function_type_list (void_type_node, NULL_TREE);

    define_builtin ("__builtin_trap", ftype_ptr,
		    BUILT_IN_TRAP, NULL, ECF_NOTHROW | ECF_LEAF);
    TREE_THIS_VOLATILE (builtin_decl_explicit (BUILT_IN_TRAP)) = 1;
  }

  fp_const_m_p5 = real_value_negate (&dconsthalf);

  build_common_builtin_nodes ();
  // FIXME: this MAY remove the need for creating the builtins above...
  // Evaluate tree.c / build_common_builtin_nodes (); for each in turn.

  return true;
}

static void
ortho_finish (void)
{
}

static unsigned int
ortho_option_lang_mask (void)
{
  return CL_vhdl;
}

static bool
ortho_post_options (const char **pfilename)
{
  if (*pfilename == NULL || strcmp (*pfilename, "-") == 0)
    *pfilename = "*stdin*";
  else if (aux_base_name == NULL)
    {
      /* Define auxbase.  The default mechanism in toplev.c doesn't
         handle extensions longer than 3 characters.  */
      char *name = xstrdup (lbasename (*pfilename));
      int len;

      /* Remove extension.  */
      for (len = strlen (name) - 1; len > 1; len--)
        if (name[len] == '.')
          {
            name[len] = 0;
            break;
          }
      aux_base_name = name;
    }

  /* Default hook.  */
  lhd_post_options (pfilename);

  /* Run the back-end.  */
  return false;
}

extern "C" int lang_handle_option (const char *opt, const char *arg);

static bool
ortho_handle_option (size_t code, const char *arg,
		     HOST_WIDE_INT value ATTRIBUTE_UNUSED,
		     int kind ATTRIBUTE_UNUSED,
                     location_t loc ATTRIBUTE_UNUSED,
                     const struct cl_option_handlers *handlers ATTRIBUTE_UNUSED)
{
  const char *opt;

  opt = cl_options[code].opt_text;

  switch (code)
    {
    case OPT__elab:
    case OPT_l:
    case OPT_c:
    case OPT__anaelab:
      /* Only a few options have a real arguments.  */
      return lang_handle_option (opt, arg) != 0;
    default:
      /* The other options must have a joint argument.  */
      if (arg != NULL)
	{
	  size_t len1;
	  size_t len2;
	  char *nopt;

	  len1 = strlen (opt);
	  len2 = strlen (arg);
	  nopt = (char *) alloca (len1 + len2 + 1);
	  memcpy (nopt, opt, len1);
	  memcpy (nopt + len1, arg, len2);
	  nopt[len1 + len2] = 0;
	  opt = nopt;
	}
      return lang_handle_option (opt, NULL) != 0;
    }
}

extern "C" int lang_parse_file (const char *filename);

static void
ortho_parse_file (void)
{
  const char *filename;
  const char *dbg_filename;

  if (num_in_fnames == 0)
    filename = NULL;
  else
    filename = in_fnames[0];

  /* Use absolute filenames for debug info.  Works better than relative
     filenames with some debuggers/tools.  */
  if (filename == NULL)
    dbg_filename = "*stdin*";
  else if (IS_ABSOLUTE_PATH (filename))
    dbg_filename = filename;
  else
    dbg_filename = concat (getpwd (), "/", filename, NULL);

  linemap_add (line_table, LC_ENTER, 0, dbg_filename, 1);
  input_location = linemap_line_start (line_table, 1, 252);

  if (!lang_parse_file (filename))
    errorcount++;
  linemap_add (line_table, LC_LEAVE, 0, NULL, 1);
}

/*  Called by the back-end or by the front-end when the address of EXP
    must be taken.
    This function should found the base object (if any), and mark it as
    addressable (via TREE_ADDRESSABLE).  It may emit a warning if this
    object cannot be addressable (front-end restriction).
    Returns TRUE in case of success, FALSE in case of failure.
    Note that the status is never checked by the back-end.  */
static bool
ortho_mark_addressable (tree exp)
{
  tree n;

  n = exp;

  while (1)
    switch (TREE_CODE (n))
      {
      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	TREE_ADDRESSABLE (n) = true;
	return true;

      case COMPONENT_REF:
      case ARRAY_REF:
      case ARRAY_RANGE_REF:
	n = TREE_OPERAND (n, 0);
	break;

      case FUNCTION_DECL:
      case CONSTRUCTOR:
	TREE_ADDRESSABLE (n) = true;
	return true;

      case INDIRECT_REF:
	return true;

      default:
	gcc_unreachable ();
      }
}

static tree
ortho_truthvalue_conversion (tree expr)
{
  tree expr_type;
  tree t;
  tree f;

  expr_type = TREE_TYPE (expr);
  if (TREE_CODE (expr_type) != BOOLEAN_TYPE)
    {
      t = integer_one_node;
      f = integer_zero_node;
    }
  else
    {
      f = TYPE_MIN_VALUE (expr_type);
      t = TYPE_MAX_VALUE (expr_type);
    }


  switch (TREE_CODE (expr))
    {
    case EQ_EXPR:
    case NE_EXPR:
    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case ERROR_MARK:
      return expr;

    case INTEGER_CST:
      /* Not 0 is true.  */
      return integer_zerop (expr) ? f : t;

    case REAL_CST:
      return real_zerop (expr) ? f : t;

    default:
      gcc_unreachable ();
    }
}

/* Do not deal with alias set.  In particular, it doesn't work well with
   incomplete type, and universal pointers are not expressed in ortho.  */

static alias_set_type
ortho_get_alias_set (tree)
{
  return 0;
}

/* The following function has been copied and modified from c-convert.c.  */

/* Change of width--truncation and extension of integers or reals--
   is represented with NOP_EXPR.  Proper functioning of many things
   assumes that no other conversions can be NOP_EXPRs.

   Conversion between integer and pointer is represented with CONVERT_EXPR.
   Converting integer to real uses FLOAT_EXPR
   and real to integer uses FIX_TRUNC_EXPR.

   Here is a list of all the functions that assume that widening and
   narrowing is always done with a NOP_EXPR:
     In convert.c, convert_to_integer.
     In c-typeck.c, build_binary_op (boolean ops), and
	c_common_truthvalue_conversion.
     In expr.c: expand_expr, for operands of a MULT_EXPR.
     In fold-const.c: fold.
     In tree.c: get_narrower and get_unwidened.  */

/* Subroutines of `convert'.  */



/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */

tree
convert (tree type, tree expr)
{
  tree e = expr;
  enum tree_code code = TREE_CODE (type);
  const char *invalid_conv_diag;

  if (type == error_mark_node
      || expr == error_mark_node
      || TREE_TYPE (expr) == error_mark_node)
    return error_mark_node;

  if ((invalid_conv_diag
       = targetm.invalid_conversion (TREE_TYPE (expr), type)))
    {
      error (invalid_conv_diag);
      return error_mark_node;
    }

  if (type == TREE_TYPE (expr))
    return expr;

  if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (expr)))
    return fold_build1 (NOP_EXPR, type, expr);
  if (TREE_CODE (TREE_TYPE (expr)) == ERROR_MARK)
    return error_mark_node;
  if (TREE_CODE (TREE_TYPE (expr)) == VOID_TYPE || code == VOID_TYPE)
    {
      gcc_unreachable ();
    }
  if (code == INTEGER_TYPE || code == ENUMERAL_TYPE)
    return fold (convert_to_integer (type, e));
  if (code == BOOLEAN_TYPE)
    {
      tree t = ortho_truthvalue_conversion (expr);
      if (TREE_CODE (t) == ERROR_MARK)
	return t;

      /* If it returns a NOP_EXPR, we must fold it here to avoid
	 infinite recursion between fold () and convert ().  */
      if (TREE_CODE (t) == NOP_EXPR)
	return fold_build1 (NOP_EXPR, type, TREE_OPERAND (t, 0));
      else
	return fold_build1 (NOP_EXPR, type, t);
    }
  if (code == POINTER_TYPE || code == REFERENCE_TYPE)
    return fold (convert_to_pointer (type, e));
  if (code == REAL_TYPE)
    return fold (convert_to_real (type, e));

  gcc_unreachable ();
}

#ifndef MAX_BITS_PER_WORD
#define MAX_BITS_PER_WORD BITS_PER_WORD
#endif

/*  This variable keeps a table for types for each precision so that we only
    allocate each of them once. Signed and unsigned types are kept separate.
 */
static GTY(()) tree signed_and_unsigned_types[MAX_BITS_PER_WORD + 1][2];

/*  Return an integer type with the number of bits of precision given by
    PRECISION.  UNSIGNEDP is nonzero if the type is unsigned; otherwise
    it is a signed type.  */
static tree
type_for_size (unsigned int precision, int unsignedp)
{
  tree t;

  if (precision <= MAX_BITS_PER_WORD
      && signed_and_unsigned_types[precision][unsignedp] != NULL_TREE)
    return signed_and_unsigned_types[precision][unsignedp];

  if (unsignedp)
    t = make_unsigned_type (precision);
  else
    t = make_signed_type (precision);

  if (precision <= MAX_BITS_PER_WORD)
    signed_and_unsigned_types[precision][unsignedp] = t;

  return t;
}

/*  Return a data type that has machine mode MODE.  UNSIGNEDP selects
    an unsigned type; otherwise a signed type is returned.  */
static tree
type_for_mode (enum machine_mode mode, int unsignedp)
{
  scalar_int_mode int_mode;
  if (is_a <scalar_int_mode> (mode, &int_mode))
    return type_for_size (GET_MODE_BITSIZE (int_mode), unsignedp);

  if (mode == TYPE_MODE (void_type_node))
    return void_type_node;

  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;

  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;

  if (mode == TYPE_MODE (long_double_type_node))
    return long_double_type_node;

   if (VECTOR_MODE_P (mode))
    {
      machine_mode inner_mode = GET_MODE_INNER (mode);
      tree inner_type = type_for_mode (inner_mode, unsignedp);
      if (inner_type)
	return build_vector_type_for_mode (inner_type, mode);
    }

  return NULL_TREE;
}

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "vhdl"
#undef LANG_HOOKS_IDENTIFIER_SIZE
#define LANG_HOOKS_IDENTIFIER_SIZE sizeof (struct tree_identifier)
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT ortho_init
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH ortho_finish
#undef LANG_HOOKS_OPTION_LANG_MASK
#define LANG_HOOKS_OPTION_LANG_MASK ortho_option_lang_mask
#undef LANG_HOOKS_HANDLE_OPTION
#define LANG_HOOKS_HANDLE_OPTION ortho_handle_option
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS ortho_post_options
#undef LANG_HOOKS_HONOR_READONLY
#define LANG_HOOKS_HONOR_READONLY true
#undef LANG_HOOKS_MARK_ADDRESSABLE
#define LANG_HOOKS_MARK_ADDRESSABLE ortho_mark_addressable
#undef LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION
#define LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION ortho_expand_function

#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE type_for_mode
#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE type_for_size
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE ortho_parse_file

#define pushlevel lhd_do_nothing_i
#define poplevel lhd_do_nothing_iii_return_null_tree
#define set_block lhd_do_nothing_t
#undef LANG_HOOKS_GETDECLS
#define LANG_HOOKS_GETDECLS hook_tree_void_null

#undef  LANG_HOOKS_GET_ALIAS_SET
#define LANG_HOOKS_GET_ALIAS_SET ortho_get_alias_set

struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

union GTY((desc ("0"),
	   chain_next ("CODE_CONTAINS_STRUCT (TREE_CODE (&%h.generic), TS_COMMON) ? ((union lang_tree_node *) TREE_CHAIN (&%h.generic)) : NULL")))
  lang_tree_node
{
  union tree_node GTY((tag ("0"),
		       desc ("tree_node_structure (&%h)"))) generic;
};

/* GHDL does not use the lang_decl and lang_type.

   FIXME: the variable_size annotation here is needed because these types are
   variable-sized in some other front-ends.  Due to gengtype deficiency, the
   GTY options of such types have to agree across all front-ends.  */

struct GTY((variable_size)) lang_type { char dummy; };
struct GTY((variable_size)) lang_decl { char dummy; };

struct GTY(()) language_function
{
  char dummy;
};


extern "C" {

struct GTY(()) chain_constr_type
{
  tree first;
  tree last;
};

static void
chain_init (struct chain_constr_type *constr)
{
  constr->first = NULL_TREE;
  constr->last = NULL_TREE;
}

static void
chain_append (struct chain_constr_type *constr, tree el)
{
  if (constr->first == NULL_TREE)
    {
      gcc_assert (constr->last == NULL_TREE);
      constr->first = el;
    }
  else
    TREE_CHAIN (constr->last) = el;
  constr->last = el;
}

struct GTY(()) list_constr_type
{
  tree first;
  tree last;
};

static void
list_init (struct list_constr_type *constr)
{
  constr->first = NULL_TREE;
  constr->last = NULL_TREE;
}

static void
ortho_list_append (struct list_constr_type *constr, tree el)
{
  tree res;

  res = tree_cons (NULL_TREE, el, NULL_TREE);
  if (constr->first == NULL_TREE)
    constr->first = res;
  else
    TREE_CHAIN (constr->last) = res;
  constr->last = res;
}

enum ON_op_kind {
  /*  Not an operation; invalid.  */
  ON_Nil,

  /*  Dyadic operations.  */
  ON_Add_Ov,
  ON_Sub_Ov,
  ON_Mul_Ov,
  ON_Div_Ov,
  ON_Rem_Ov,
  ON_Mod_Ov,

  /*  Binary operations.  */
  ON_And,
  ON_Or,
  ON_Xor,

  /*  Monadic operations.  */
  ON_Not,
  ON_Neg_Ov,
  ON_Abs_Ov,

  /*  Comparaisons  */
  ON_Eq,
  ON_Neq,
  ON_Le,
  ON_Lt,
  ON_Ge,
  ON_Gt,

  ON_LAST
};

static enum tree_code ON_op_to_TREE_CODE[ON_LAST] = {
  ERROR_MARK,

  PLUS_EXPR,
  MINUS_EXPR,
  MULT_EXPR,
  ERROR_MARK,
  TRUNC_MOD_EXPR,
  FLOOR_MOD_EXPR,

  BIT_AND_EXPR,
  BIT_IOR_EXPR,
  BIT_XOR_EXPR,

  BIT_NOT_EXPR,
  NEGATE_EXPR,
  ABS_EXPR,

  EQ_EXPR,
  NE_EXPR,
  LE_EXPR,
  LT_EXPR,
  GE_EXPR,
  GT_EXPR,
};

tree
new_dyadic_op (enum ON_op_kind kind, tree left, tree right)
{
  tree left_type;
  enum tree_code code;

  /* Truncate to avoid representations issue.  */
  kind = (enum ON_op_kind)((unsigned)kind & 0xff);

  left_type = TREE_TYPE (left);
  gcc_assert (left_type == TREE_TYPE (right));

  switch (kind)
    {
    case ON_Div_Ov:
      if (TREE_CODE (left_type) == REAL_TYPE)
	code = RDIV_EXPR;
      else
	code = TRUNC_DIV_EXPR;
      break;
    default:
      code = ON_op_to_TREE_CODE[kind];
      break;
    }
  return build2 (code, left_type, left, right);
}

tree
new_monadic_op (enum ON_op_kind kind, tree operand)
{
  /* Truncate to avoid representations issue.  */
  kind = (enum ON_op_kind)((unsigned)kind & 0xff);

  return build1 (ON_op_to_TREE_CODE[kind], TREE_TYPE (operand), operand);
}

tree
new_compare_op (enum ON_op_kind kind, tree left, tree right, tree ntype)
{
  gcc_assert (TREE_CODE (ntype) == BOOLEAN_TYPE);
  gcc_assert (TREE_TYPE (left) == TREE_TYPE (right));

  /* Truncate to avoid representations issue.  */
  kind = (enum ON_op_kind)((unsigned)kind & 0xff);

  return build2 (ON_op_to_TREE_CODE[kind], ntype, left, right);
}

tree
new_convert (tree val, tree rtype)
{
  tree val_type;
  enum tree_code val_code;
  enum tree_code rtype_code;
  enum tree_code code;

  val_type = TREE_TYPE (val);
  if (val_type == rtype)
    return val;

  /*  FIXME: check conversions.  */
  val_code = TREE_CODE (val_type);
  rtype_code = TREE_CODE (rtype);
  if (val_code == POINTER_TYPE && rtype_code == POINTER_TYPE)
    code = NOP_EXPR;
  else if (val_code == INTEGER_TYPE && rtype_code == INTEGER_TYPE)
    code = CONVERT_EXPR;
  else if (val_code == REAL_TYPE && rtype_code == INTEGER_TYPE)
    {
      /*  REAL to INTEGER
          Gcc only handles FIX_TRUNC_EXPR, but we need rounding.  */
      tree m_p5;
      tree p5;
      tree zero;
      tree saved;
      tree comp;
      tree adj;
      tree res;

      m_p5 = build_real (val_type, fp_const_m_p5);
      p5 = build_real (val_type, dconsthalf);
      zero = build_real (val_type, dconst0);
      saved = save_expr (val);
      comp = build2 (GE_EXPR, integer_type_node, saved, zero);
      /*  FIXME: instead of res = res + (comp ? .5 : -.5)
	  do: res = res (comp ? + : -) .5  */
      adj = build3 (COND_EXPR, val_type, comp, p5, m_p5);
      res = build2 (PLUS_EXPR, val_type, saved, adj);
      res = build1 (FIX_TRUNC_EXPR, rtype, res);
      return res;
    }
  else if (val_code == INTEGER_TYPE && rtype_code == ENUMERAL_TYPE)
    code = CONVERT_EXPR;
  else if (val_code == ENUMERAL_TYPE && rtype_code == INTEGER_TYPE)
    code = CONVERT_EXPR;
  else if (val_code == INTEGER_TYPE && rtype_code == REAL_TYPE)
    code = FLOAT_EXPR;
  else if (val_code == BOOLEAN_TYPE && rtype_code == BOOLEAN_TYPE)
    code = NOP_EXPR;
  else if (val_code == BOOLEAN_TYPE && rtype_code == INTEGER_TYPE)
    code = CONVERT_EXPR;
  else if (val_code == INTEGER_TYPE && rtype_code == BOOLEAN_TYPE)
    code = NOP_EXPR;
  else if (val_code == REAL_TYPE && rtype_code == REAL_TYPE)
    code = NOP_EXPR;
  else
    gcc_unreachable ();

  return build1 (code, rtype, val);
}

tree
new_convert_ov (tree val, tree rtype)
{
  return new_convert (val, rtype);
}

tree
new_alloca (tree rtype, tree size)
{
  tree res;

  /* Must save stack except when at function level.  */
  if (cur_binding_level->prev != NULL
      && cur_binding_level->prev->prev != NULL)
    cur_binding_level->save_stack = 1;

  res = build_call_nary (ptr_type_node, stack_alloc_function_ptr,
                         1, fold_convert (size_type_node, size));
  return fold_convert (rtype, res);
}

tree
new_signed_literal (tree ltype, long long value)
{
  tree res;
  HOST_WIDE_INT lo;
  HOST_WIDE_INT hi;

  lo = value;
  hi = (value >> 1) >> (8 * sizeof (HOST_WIDE_INT) - 1);
  res = double_int_to_tree (ltype, double_int::from_pair (hi, lo));
  return res;
}

tree
new_unsigned_literal (tree ltype, unsigned long long value)
{
  tree res;
  unsigned HOST_WIDE_INT lo;
  unsigned HOST_WIDE_INT hi;

  lo = value;
  hi = (value >> 1) >> (8 * sizeof (HOST_WIDE_INT) - 1);
  res = double_int_to_tree (ltype, double_int::from_pair (hi, lo));
  return res;
}

tree
new_null_access (tree ltype)
{
  tree res;

  res = build_int_cst (ltype, 0);
  return res;
}

tree
new_float_literal (tree ltype, double value)
{
  signed long long s;
  double frac;
  int ex;
  REAL_VALUE_TYPE r_sign;
  REAL_VALUE_TYPE r_exp;
  REAL_VALUE_TYPE r;
  tree res;
  HOST_WIDE_INT lo;
  HOST_WIDE_INT hi;

  frac = frexp (value, &ex);

  s = ldexp (frac, 60);
  lo = s;
  hi = (s >> 1) >> (8 * sizeof (HOST_WIDE_INT) - 1);
  real_from_integer (&r_sign, DFmode, double_int::from_pair (hi, lo), SIGNED);
  real_2expN (&r_exp, ex - 60, DFmode);
  real_arithmetic (&r, MULT_EXPR, &r_sign, &r_exp);
  res = build_real (ltype, r);
  return res;
}

struct GTY(()) o_element_list
{
  tree res;
  struct chain_constr_type chain;
};

struct GTY(()) o_element_sublist
{
  tree base;
  tree field;
  tree res;
  struct chain_constr_type chain;
};

void
new_uncomplete_record_type (tree *res)
{
  *res = make_node (RECORD_TYPE);
}

void
start_record_type (struct o_element_list *elements)
{
  elements->res = make_node (RECORD_TYPE);
  chain_init (&elements->chain);
}

void
start_uncomplete_record_type (tree res, struct o_element_list *elements)
{
  elements->res = res;
  chain_init (&elements->chain);
}

static void
new_record_union_field (struct o_element_list *list,
			tree *el,
			tree ident,
			tree etype)
{
  tree res;

  if (TYPE_UNBOUNDED(etype)) {
    /* If the field type is unbounded, it mustn't use any space in the
       record.  Use VOID instead.  */
    TYPE_UNBOUNDED(list->res) = 1;
    etype = void_type_node;
  }

  res = build_decl (input_location, FIELD_DECL, ident, etype);
  DECL_CONTEXT (res) = list->res;
  chain_append (&list->chain, res);
  *el = res;
}

void
new_record_field (struct o_element_list *list,
		  tree *el,
		  tree ident,
		  tree etype)
{
  return new_record_union_field (list, el, ident, etype);
}

void
finish_record_type (struct o_element_list *elements, tree *res)
{
  TYPE_FIELDS (elements->res) = elements->chain.first;
  layout_type (elements->res);
  *res = elements->res;

  if (TYPE_NAME (elements->res) != NULL_TREE)
    {
      /*  The type was completed.  */
      rest_of_type_compilation (elements->res, 1);
    }
}

void
start_record_subtype (tree rtype, struct o_element_sublist *elements)
{
  elements->base = rtype;
  elements->field = TYPE_FIELDS (rtype);
  elements->res = make_node (RECORD_TYPE);
  chain_init (&elements->chain);
}

void
new_subrecord_field (struct o_element_sublist *list,
                     tree *el,
                     tree etype)
{
  tree res;

  res = build_decl (input_location, FIELD_DECL, DECL_NAME(list->field), etype);
  DECL_CONTEXT (res) = list->res;
  chain_append (&list->chain, res);
  list->field = TREE_CHAIN(list->field);
  *el = res;
}

void
finish_record_subtype (struct o_element_sublist *elements, tree *res)
{
  TYPE_FIELDS (elements->res) = elements->chain.first;
  layout_type (elements->res);
  *res = elements->res;
}

void
start_union_type (struct o_element_list *elements)
{
  elements->res = make_node (UNION_TYPE);
  chain_init (&elements->chain);
}

void
new_union_field (struct o_element_list *elements,
		 tree *el,
		 tree ident,
		 tree etype)
{
  return new_record_union_field (elements, el, ident, etype);
}

void
finish_union_type (struct o_element_list *elements, tree *res)
{
  TYPE_FIELDS (elements->res) = elements->chain.first;
  layout_type (elements->res);
  *res = elements->res;
}

tree
new_unsigned_type (int size)
{
  return make_unsigned_type (size);
}

tree
new_signed_type (int size)
{
  return make_signed_type (size);
}

tree
new_float_type (void)
{
  tree res;

  res = make_node (REAL_TYPE);
  TYPE_PRECISION (res) = DOUBLE_TYPE_SIZE;
  layout_type (res);
  return res;
}

tree
new_access_type (tree dtype)
{
  tree res;

  if (dtype == NULL_TREE)
    {
      res = make_node (POINTER_TYPE);
      TREE_TYPE (res) = NULL_TREE;
      /* Seems necessary.  */
      SET_TYPE_MODE (res, Pmode);
      layout_type (res);
      return res;
    }
  else
    return build_pointer_type (dtype);
}

void
finish_access_type (tree atype, tree dtype)
{
  gcc_assert (TREE_CODE (atype) == POINTER_TYPE
	      && TREE_TYPE (atype) == NULL_TREE);

  TREE_TYPE (atype) = dtype;
}

/*  Create a range type from INDEX_TYPE of length LENGTH.  */
static tree
ortho_build_array_range(tree index_type, tree length)
{
  tree len;

  if (integer_zerop (length))
    {
      /*  Handle null array, by creating a one-length array...  */
      len = size_zero_node;
    }
  else
    {
      len = fold_build2 (MINUS_EXPR, index_type,
			 convert (index_type, length),
			 convert (index_type, size_one_node));
    }
  return build_range_type (index_type, size_zero_node, len);
}

tree
new_array_type (tree el_type, tree index_type)
{
  /* Incomplete array.  */
  tree range_type;
  tree res;

  /* Build an incomplete array.  */
  range_type = build_range_type (index_type, size_zero_node, NULL_TREE);
  res = build_array_type (el_type, range_type);
  TYPE_UNBOUNDED(res) = 1;
  return res;
}

tree
new_array_subtype (tree atype, tree eltype, tree length)
{
  tree range_type;
  tree index_type;
  tree res;

  gcc_assert(!TYPE_UNBOUNDED(eltype));
  index_type = TYPE_DOMAIN (atype);

  range_type = ortho_build_array_range(index_type, length);
  res = build_array_type (eltype, range_type);

  /* Constrained arrays are *always* a subtype of its array type.
     Just copy alias set.  */
  TYPE_ALIAS_SET (res) = get_alias_set (atype);

  return res;
}

void
new_boolean_type (tree *res,
		  tree false_id ATTRIBUTE_UNUSED, tree *false_e,
		  tree true_id ATTRIBUTE_UNUSED, tree *true_e)
{
  *res = make_node (BOOLEAN_TYPE);
  TYPE_PRECISION (*res) = 1;
  fixup_unsigned_type (*res);
  *false_e = TYPE_MIN_VALUE (*res);
  *true_e = TYPE_MAX_VALUE (*res);
}

struct o_enum_list
{
  tree res;
  struct chain_constr_type chain;
  int num;
  int size;
};

void
start_enum_type (struct o_enum_list *list, int size)
{
  list->res = make_node (ENUMERAL_TYPE);
  /* Set precision and sign now, as this is used to normalize literals.  */
  TYPE_PRECISION (list->res) = size;
  TYPE_UNSIGNED (list->res) = 1;
  chain_init (&list->chain);
  list->num = 0;
  list->size = size;
}

void
new_enum_literal (struct o_enum_list *list, tree ident, tree *res)
{
  *res = build_int_cstu (list->res, (HOST_WIDE_INT)(list->num));
  chain_append (&list->chain, tree_cons (ident, *res, NULL_TREE));
  list->num++;
}

void
finish_enum_type (struct o_enum_list *list, tree *res)
{
  *res = list->res;
  TYPE_VALUES (*res) = list->chain.first;
  set_min_and_max_values_for_integral_type (*res, list->size, UNSIGNED);
  layout_type (*res);
}

struct GTY(()) o_record_aggr_list
{
  /* Type of the record.  */
  tree atype;
  /* Type of the next field to be added.  */
  tree field;
  /* Vector of elements.  */
  // VEC(constructor_elt,gc) *elts;
  vec<constructor_elt,va_gc> *elts;
};

void
start_record_aggr (struct o_record_aggr_list *list, tree atype)
{
  list->atype = atype;
  list->field = TYPE_FIELDS (atype);
  //list->elts = VEC_alloc (constructor_elt, gc, fields_length (atype));
  vec_alloc(list->elts, fields_length (atype));
}

void
new_record_aggr_el (struct o_record_aggr_list *list, tree value)
{
  CONSTRUCTOR_APPEND_ELT (list->elts, list->field, value);
  list->field = TREE_CHAIN (list->field);
}

void
finish_record_aggr (struct o_record_aggr_list *list, tree *res)
{
  *res = build_constructor (list->atype, list->elts);
}

struct GTY(()) o_array_aggr_list
{
  tree atype;
  /* Vector of elements.  */
  vec<constructor_elt,va_gc> *elts;
};

void
start_array_aggr (struct o_array_aggr_list *list, tree atype, unsigned len)
{
  tree length;

  length = new_unsigned_literal (sizetype, len);
  list->atype = new_array_subtype (atype, TREE_TYPE (atype), length);
  vec_alloc(list->elts, len);
}

void
new_array_aggr_el (struct o_array_aggr_list *list, tree value)
{
  CONSTRUCTOR_APPEND_ELT (list->elts, NULL_TREE, value);
}

void
finish_array_aggr (struct o_array_aggr_list *list, tree *res)
{
  *res = build_constructor (list->atype, list->elts);
}

tree
new_union_aggr (tree atype, tree field, tree value)
{
  tree res;

  res = build_constructor_single (atype, field, value);
  TREE_CONSTANT (res) = 1;
  return res;
}

tree
new_default_value (tree atype)
{
  return build_constructor (atype, NULL);
}

tree
new_indexed_element (tree arr, tree index)
{
  ortho_mark_addressable (arr);
  return build4 (ARRAY_REF, TREE_TYPE (TREE_TYPE (arr)),
		 arr, index, NULL_TREE, NULL_TREE);
}

tree
new_slice (tree arr, tree res_type, tree index)
{
  gcc_assert (TREE_CODE (res_type) == ARRAY_TYPE);

  /* gcc needs a complete array type, so create the biggest one if it is
     not.  */
  if (TYPE_MAX_VALUE (TYPE_DOMAIN (res_type)) == NULL_TREE)
    {
      res_type = build_array_type (TREE_TYPE (res_type),
                                   TREE_TYPE (TYPE_DOMAIN (res_type)));
    }

  /* Take the element size from RES_TYPE (and not from ARR, which is the
     default.  */
  tree elmt_type = TREE_TYPE (res_type);
  tree elmt_size = TYPE_SIZE_UNIT (elmt_type);
  tree factor = size_int (TYPE_ALIGN_UNIT (elmt_type));

  /* Divide the element size by the alignment of the element type (above).  */
  elmt_size = size_binop (EXACT_DIV_EXPR, elmt_size, factor);

  ortho_mark_addressable (arr);
  return build4 (ARRAY_RANGE_REF, res_type, arr, index, NULL_TREE, elmt_size);
}

tree
new_selected_element (tree rec, tree el)
{
  tree res;

  gcc_assert (RECORD_OR_UNION_TYPE_P (TREE_TYPE (rec)));

  res = build3 (COMPONENT_REF, TREE_TYPE (el), rec, el, NULL_TREE);
  return res;
}

tree
new_access_element (tree acc)
{
  tree acc_type;

  acc_type = TREE_TYPE (acc);
  gcc_assert (TREE_CODE (acc_type) == POINTER_TYPE);

  return build1 (INDIRECT_REF, TREE_TYPE (acc_type), acc);
}

tree
new_offsetof (tree rec_type, tree field, tree rtype)
{
  tree off;
  tree bit_off;
  HOST_WIDE_INT pos;
  tree res;

  gcc_assert (DECL_CONTEXT (field) == rec_type);

  off = DECL_FIELD_OFFSET (field);

  /*  The offset must be a constant.  */
  gcc_assert (tree_fits_uhwi_p (off));

  bit_off = DECL_FIELD_BIT_OFFSET (field);

  /*  The offset must be a constant.  */
  gcc_assert (tree_fits_uhwi_p (bit_off));

  pos = TREE_INT_CST_LOW (off)
        + (TREE_INT_CST_LOW (bit_off) / BITS_PER_UNIT);
  res = build_int_cstu (rtype, pos);
  return res;
}

tree
new_sizeof (tree atype, tree rtype)
{
 tree size;

 size = TYPE_SIZE_UNIT (atype);

 return fold (build1 (NOP_EXPR, rtype, size));
}

tree
new_record_sizeof (tree atype, tree rtype)
{
  return new_sizeof (atype, rtype);
}

tree
new_alignof (tree atype, tree rtype)
{
  return build_int_cstu (rtype, TYPE_ALIGN_UNIT (atype));
}

static tree
ortho_build_addr (tree lvalue, tree atype)
{
  tree res;

  if (TREE_CODE (lvalue) == INDIRECT_REF)
    {
      /* ADDR_REF(INDIRECT_REF(x)) -> x.  */
      res = TREE_OPERAND (lvalue, 0);
    }
  else
    {
      tree ptr_type;

      /* &base[off] -> base+off.  */
      ortho_mark_addressable (lvalue);

      if (TREE_TYPE (lvalue) != TREE_TYPE (atype))
	ptr_type = build_pointer_type (TREE_TYPE (lvalue));
      else
	ptr_type = atype;
      res = fold_build1 (ADDR_EXPR, ptr_type, lvalue);
    }

  if (TREE_TYPE (res) != atype)
    res = fold_build1 (NOP_EXPR, atype, res);

  return res;
}

tree
new_unchecked_address (tree lvalue, tree atype)
{
  return ortho_build_addr (lvalue, atype);
}

tree
new_address (tree lvalue, tree atype)
{
  return ortho_build_addr (lvalue, atype);
}

tree
new_global_address (tree lvalue, tree atype)
{
  return ortho_build_addr (lvalue, atype);
}

tree
new_global_unchecked_address (tree lvalue, tree atype)
{
  return ortho_build_addr (lvalue, atype);
}

/*  Return a pointer to function FUNC. */
static tree
build_function_ptr (tree func)
{
  return build1 (ADDR_EXPR,
		 build_pointer_type (TREE_TYPE (func)), func);
}

tree
new_subprogram_address (tree subprg, tree atype)
{
  return fold (build1 (NOP_EXPR, atype, build_function_ptr (subprg)));
}

tree
new_value (tree lvalue)
{
  return lvalue;
}

void
new_debug_line_decl (int line)
{
  input_location = linemap_line_start (line_table, line, 252);
}

void
new_type_decl (tree ident, tree atype)
{
  tree decl;

  TYPE_NAME (atype) = ident;
  decl = build_decl (input_location, TYPE_DECL, ident, atype);
  TYPE_STUB_DECL (atype) = decl;
  pushdecl (decl);
  /*
      if Get_TYPE_SIZE (Ttype) /= NULL_TREE then
         --  Do not generate debug info for uncompleted types.
         Rest_Of_Type_Compilation (Ttype, C_True);
      end if;
  */
}

enum o_storage { o_storage_external,
		 o_storage_public,
		 o_storage_private,
		 o_storage_local };

static void
set_storage (tree Node, enum o_storage storage)
{
  switch (storage)
    {
    case o_storage_external:
      DECL_EXTERNAL (Node) = 1;
      TREE_PUBLIC (Node) = 1;
      TREE_STATIC (Node) = 0;
      break;
    case o_storage_public:
      DECL_EXTERNAL (Node) = 0;
      TREE_PUBLIC (Node) = 1;
      TREE_STATIC (Node) = 1;
      break;
    case o_storage_private:
      DECL_EXTERNAL (Node) = 0;
      TREE_PUBLIC (Node) = 0;
      TREE_STATIC (Node) = 1;
      break;
    case o_storage_local:
      DECL_EXTERNAL (Node) = 0;
      TREE_PUBLIC (Node) = 0;
      TREE_STATIC (Node) = 0;
      break;
    }
}

void
new_const_decl (tree *res, tree ident, enum o_storage storage, tree atype)
{
  tree cst;

  cst = build_decl (input_location, VAR_DECL, ident, atype);
  set_storage (cst, storage);
  TREE_READONLY (cst) = 1;
  pushdecl (cst);
  switch (storage)
    {
    case o_storage_local:
      gcc_unreachable ();
    case o_storage_external:
      /*  We are at top level if Current_Function_Decl is null.  */
      rest_of_decl_compilation (cst, current_function_decl == NULL_TREE, 0);
      break;
    case o_storage_public:
    case o_storage_private:
      break;
    }
  *res = cst;
}

void
start_init_value (tree *decl ATTRIBUTE_UNUSED)
{
}

void
finish_init_value (tree *decl, tree val)
{
  DECL_INITIAL (*decl) = val;
  TREE_CONSTANT (val) = 1;
  TREE_STATIC (*decl) = 1;

  /* The variable may be declared with an incomplete array, so be sure it
     has a completed type.
     Force re-layout by clearing the size.  */
  DECL_SIZE (*decl) = NULL_TREE;
  TREE_TYPE (*decl) = TREE_TYPE (val);
  layout_decl (*decl, 0);

  rest_of_decl_compilation (*decl, current_function_decl == NULL_TREE, 0);
}

void
new_var_decl (tree *res, tree ident, enum o_storage storage, tree atype)
{
  tree var;

  var = build_decl (input_location, VAR_DECL, ident, atype);
  if (current_function_decl != NULL_TREE)
    {
      /*  Local variable. */
      TREE_STATIC (var) = 0;
      DECL_EXTERNAL (var) = 0;
      TREE_PUBLIC (var) = 0;
    }
  else
    set_storage (var, storage);

  pushdecl (var);

  if (current_function_decl == NULL_TREE)
    rest_of_decl_compilation (var, 1, 0);

  *res = var;
}

struct GTY(()) o_inter_list
{
  tree ident;
  enum o_storage storage;

  /*  Return type.  */
  tree rtype;

  /*  List of parameter types.  */
  struct list_constr_type param_list;

  /*  Chain of parameters declarations.  */
  struct chain_constr_type param_chain;
};

void
start_function_decl (struct o_inter_list *interfaces,
		     tree ident,
		     enum o_storage storage,
		     tree rtype)
{
  interfaces->ident = ident;
  interfaces->storage = storage;
  interfaces->rtype = rtype;
  chain_init (&interfaces->param_chain);
  list_init (&interfaces->param_list);
}

void
start_procedure_decl (struct o_inter_list *interfaces,
		      tree ident,
		      enum o_storage storage)
{
  start_function_decl (interfaces, ident, storage, void_type_node);
}

void
new_interface_decl (struct o_inter_list *interfaces,
		    tree *res,
		    tree ident,
		    tree atype)
{
  tree r;

  r = build_decl (input_location, PARM_DECL, ident, atype);
  /* DECL_CONTEXT (Res, Xxx); */

  /*  Do type conversion: convert boolean and enums to int  */
  switch (TREE_CODE (atype))
    {
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
      DECL_ARG_TYPE (r) = integer_type_node;
      break;
    default:
      DECL_ARG_TYPE (r) = atype;
      break;
    }

  layout_decl (r, 0);

  chain_append (&interfaces->param_chain, r);
  ortho_list_append (&interfaces->param_list, atype);
  *res = r;
}

void
finish_subprogram_decl (struct o_inter_list *interfaces, tree *res)
{
  tree decl;
  tree result;
  tree parm;
  int is_global;

  /* Append a void type in the parameter types chain, so that the function
     is known not be have variables arguments.  */
  ortho_list_append (&interfaces->param_list, void_type_node);

  decl = build_decl (input_location, FUNCTION_DECL, interfaces->ident,
		     build_function_type (interfaces->rtype,
					  interfaces->param_list.first));
  DECL_SOURCE_LOCATION (decl) = input_location;

  is_global = current_function_decl == NULL_TREE
    || interfaces->storage == o_storage_external;
  if (is_global)
    set_storage (decl, interfaces->storage);
  else
    {
      /*  A nested subprogram.  */
      DECL_EXTERNAL (decl) = 0;
      TREE_PUBLIC (decl) = 0;
    }
  /*  The function exist in static storage. */
  TREE_STATIC (decl) = 1;
  DECL_INITIAL (decl) = error_mark_node;
  TREE_ADDRESSABLE (decl) = 1;

  /*  Declare the result.
      FIXME: should be moved in start_function_body. */
  result = build_decl (input_location,
                       RESULT_DECL, NULL_TREE, interfaces->rtype);
  DECL_RESULT (decl) = result;
  DECL_CONTEXT (result) = decl;

  DECL_ARGUMENTS (decl) = interfaces->param_chain.first;
  /* Set DECL_CONTEXT of parameters.  */
  for (parm = interfaces->param_chain.first;
       parm != NULL_TREE;
       parm = TREE_CHAIN (parm))
    DECL_CONTEXT (parm) = decl;

  pushdecl (decl);

  /* External functions are never nested.
     Remove their context, which is set by pushdecl.  */
  if (interfaces->storage == o_storage_external)
    DECL_CONTEXT (decl) = NULL_TREE;

  if (is_global)
    rest_of_decl_compilation (decl, 1, 0);

  *res = decl;
}

void
start_subprogram_body (tree func)
{
  gcc_assert (current_function_decl == DECL_CONTEXT (func));
  current_function_decl = func;

  /* The function is not anymore external.  */
  DECL_EXTERNAL (func) = 0;

  push_binding (FUNCTION_BINDING);
}

void
finish_subprogram_body (void)
{
  tree bind;
  tree func;
  tree parent;

  bind = pop_binding ();

  func = current_function_decl;

  /* Decl initial contains the BLOCK for the function.  */
  DECL_INITIAL (func) = BIND_EXPR_BLOCK (bind);

  /* The saved tree is the BIND_EXPR.  */
  DECL_SAVED_TREE (func) = bind;

  /* Initialize the RTL code for the function.  */
  allocate_struct_function (func, false);

  /* Store the end of the function.  */
  cfun->function_end_locus = input_location;

  parent = DECL_CONTEXT (func);

  if (parent != NULL)
    cgraph_node::get_create (func);
  else
    cgraph_node::finalize_function (func, false);

  current_function_decl = parent;
  set_cfun (NULL);
}


void
new_debug_line_stmt (int line)
{
  input_location = linemap_line_start (line_table, line, 252);
}

void
start_declare_stmt (void)
{
  push_binding (LOCAL_BINDING);
}

void
finish_declare_stmt (void)
{
  tree bind;

  bind = pop_binding ();
  append_stmt (bind);
}


struct GTY(()) o_assoc_list
{
  tree subprg;
  vec<tree, va_gc> *vecptr;
};

void
start_association (struct o_assoc_list *assocs, tree subprg)
{
  assocs->subprg = subprg;
  assocs->vecptr = NULL;
}

void
new_association (struct o_assoc_list *assocs, tree val)
{
  vec_safe_push(assocs->vecptr, val);
}

tree
new_function_call (struct o_assoc_list *assocs)
{
  return build_call_vec (TREE_TYPE (TREE_TYPE (assocs->subprg)),
                         build_function_ptr (assocs->subprg),
                         assocs->vecptr);
}

void
new_procedure_call (struct o_assoc_list *assocs)
{
  tree res;

  res = build_call_vec (TREE_TYPE (TREE_TYPE (assocs->subprg)),
                        build_function_ptr (assocs->subprg),
                        assocs->vecptr);
  TREE_SIDE_EFFECTS (res) = 1;
  append_stmt (res);
}

void
new_assign_stmt (tree target, tree value)
{
  tree n;

  n = build2 (MODIFY_EXPR, TREE_TYPE (target), target, value);
  TREE_SIDE_EFFECTS (n) = 1;
  append_stmt (n);
}

void
new_func_return_stmt (tree value)
{
  tree assign;
  tree stmt;
  tree res;

  res = DECL_RESULT (current_function_decl);
  assign = build2 (MODIFY_EXPR, TREE_TYPE (value), res, value);
  TREE_SIDE_EFFECTS (assign) = 1;
  stmt = build1 (RETURN_EXPR, void_type_node, assign);
  TREE_SIDE_EFFECTS (stmt) = 1;
  append_stmt (stmt);
}

void
new_proc_return_stmt (void)
{
  tree stmt;

  stmt = build1 (RETURN_EXPR, void_type_node, NULL_TREE);
  TREE_SIDE_EFFECTS (stmt) = 1;
  append_stmt (stmt);
}


struct GTY(()) o_if_block
{
  /* STATEMENT_LIST containing the if.  */
  tree prev_stmts;

  /* The COND_EXPR.  */
  tree if_stmt;
};

void
start_if_stmt (struct o_if_block *block, tree cond)
{
  tree stmt;
  tree stmts;

  stmts = alloc_stmt_list ();
  stmt = build3 (COND_EXPR, void_type_node, cond, stmts, NULL_TREE);
  append_stmt (stmt);
  block->prev_stmts = cur_stmts;
  block->if_stmt = stmt;
  cur_stmts = stmts;
}

void
new_else_stmt (struct o_if_block *block)
{
  cur_stmts = alloc_stmt_list ();
  COND_EXPR_ELSE (block->if_stmt) = cur_stmts;
}

void
finish_if_stmt (struct o_if_block *block)
{
  cur_stmts = block->prev_stmts;
}

struct GTY(()) o_snode
{
  tree beg_label;
  tree end_label;
};

/* Create an artificial label.  */
static tree
build_label (void)
{
  tree res;

  res = build_decl (input_location, LABEL_DECL, NULL_TREE, void_type_node);
  DECL_CONTEXT (res) = current_function_decl;
  DECL_ARTIFICIAL (res) = 1;
  return res;
}

void
start_loop_stmt (struct o_snode *label)
{
  tree stmt;

  label->beg_label = build_label ();

  stmt = build1 (LABEL_EXPR, void_type_node, label->beg_label);
  append_stmt (stmt);

  label->end_label = build_label ();
}

void
finish_loop_stmt (struct o_snode *label)
{
  tree stmt;

  stmt = build1 (GOTO_EXPR, void_type_node, label->beg_label);
  TREE_USED (label->beg_label) = 1;
  append_stmt (stmt);
  /*  Emit the end label only if there is a goto to it.
      (Return may be used to exit from the loop).  */
  if (TREE_USED (label->end_label))
    {
      stmt = build1 (LABEL_EXPR, void_type_node, label->end_label);
      append_stmt (stmt);
    }
}

void
new_exit_stmt (struct o_snode *l)
{
  tree stmt;

  stmt = build1 (GOTO_EXPR, void_type_node, l->end_label);
  append_stmt (stmt);
  TREE_USED (l->end_label) = 1;
}

void
new_next_stmt (struct o_snode *l)
{
  tree stmt;

  stmt = build1 (GOTO_EXPR, void_type_node, l->beg_label);
  TREE_USED (l->beg_label) = 1;
  append_stmt (stmt);
}

struct GTY(()) o_case_block
{
  tree prev_stmts;
  tree case_type;
  tree end_label;
  int add_break;
};

void
start_case_stmt (struct o_case_block *block, tree value)
{
  tree stmt;
  tree stmts;

  block->prev_stmts = cur_stmts;
  block->case_type = TREE_TYPE (value);
  block->end_label = build_label ();
  block->add_break = 0;

  stmts = alloc_stmt_list ();
  stmt = build2 (SWITCH_EXPR, block->case_type, value, stmts);
  append_stmt (stmt);
  cur_stmts = stmts;
}

void
start_choice (struct o_case_block *block)
{
  tree stmt;

  if (block->add_break)
    {
      stmt = build1 (GOTO_EXPR, block->case_type, block->end_label);
      append_stmt (stmt);

      block->add_break = 0;
    }
}

void
new_expr_choice (struct o_case_block *block ATTRIBUTE_UNUSED, tree expr)
{
  tree stmt;

  stmt = build_case_label
    (expr, NULL_TREE, create_artificial_label (input_location));
  append_stmt (stmt);
}

void
new_range_choice (struct o_case_block *block ATTRIBUTE_UNUSED,
		  tree low, tree high)
{
  tree stmt;

  stmt = build_case_label
    (low, high, create_artificial_label (input_location));
  append_stmt (stmt);
}

void
new_default_choice (struct o_case_block *block ATTRIBUTE_UNUSED)
{
  tree stmt;

  stmt = build_case_label
    (NULL_TREE, NULL_TREE, create_artificial_label (input_location));
  append_stmt (stmt);
}

void
finish_choice (struct o_case_block *block)
{
  block->add_break = 1;
}

void
finish_case_stmt (struct o_case_block *block)
{
  tree stmt;

  cur_stmts = block->prev_stmts;
  stmt = build1 (LABEL_EXPR, void_type_node, block->end_label);
  append_stmt (stmt);
}

bool
compare_identifier_string (tree id, const char *str, size_t len)
{
  if (IDENTIFIER_LENGTH (id) != len)
    return false;
  if (!memcmp (IDENTIFIER_POINTER (id), str, len))
    return true;
  else
    return false;
}

void
get_identifier_string (tree id, const char **str, int *len)
{
  *len = IDENTIFIER_LENGTH (id);
  *str = IDENTIFIER_POINTER (id);
}

// C linkage wrappers for two (now C++) functions so that
// Ada code can call them without name mangling
tree get_identifier_with_length_c (const char *c, size_t s)
{
  return get_identifier_with_length(c, s);
}

int toplev_main_c (int argc, char **argv)
{
  toplev toplev (NULL, true);
  return toplev.main(argc, argv);
}

void
debug_tree_c (tree expr)
{
  warning (OPT_Wall, "Debug tree");
  debug_tree (expr);
}

} // end extern "C"

#include "debug.h"
#include "gt-vhdl-ortho-lang.h"
#include "gtype-vhdl.h"
