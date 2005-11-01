#include <stddef.h>
#include <math.h>
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
#include "toplev.h"
#include "opts.h"
#include "options.h"
#include "real.h"
#include "tree-gimple.h"
#include "function.h"
#include "cgraph.h"

const int tree_identifier_size = sizeof (struct tree_identifier);

struct binding_level GTY(())
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
};

/*  The current binding level.  */
static GTY(()) struct binding_level *cur_binding_level = NULL;

/*  Chain of unused binding levels.  */
static GTY(()) struct binding_level *old_binding_levels = NULL;

static tree cur_stmts = NULL_TREE;

static void
push_binding (void)
{
  struct binding_level *res;

  if (old_binding_levels == NULL)
    res = (struct binding_level *) ggc_alloc (sizeof (struct binding_level));
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

  res->bind = make_node (BIND_EXPR);
  res->block = make_node (BLOCK);
  BIND_EXPR_BLOCK (res->bind) = res->block;
  TREE_SIDE_EFFECTS (res->bind) = 1;
  TREE_TYPE (res->bind) = void_type_node;
  TREE_USED (res->block) = 1;

  if (cur_binding_level != NULL)
    {
      /* Append the block created.  */
      if (cur_binding_level->first_block == NULL)
	cur_binding_level->first_block = res->block;
      else
	TREE_CHAIN (cur_binding_level->last_block) = res->block;
      cur_binding_level->last_block = res->block;

      BLOCK_SUPERCONTEXT (res->block) = cur_binding_level->block;
    }

  res->prev = cur_binding_level;
  cur_binding_level = res;
}

static void
push_decl (tree decl)
{
  DECL_CONTEXT (decl) = current_function_decl;

  if (cur_binding_level->first_decl == NULL)
    cur_binding_level->first_decl = decl;
  else
    TREE_CHAIN (cur_binding_level->last_decl) = decl;
  cur_binding_level->last_decl = decl;
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
      tmp_var = build_decl (VAR_DECL, NULL, ptr_type_node);
      DECL_ARTIFICIAL (tmp_var) = 1;
      DECL_IGNORED_P (tmp_var) = 1;
      TREE_USED (tmp_var) = 1;
      push_decl (tmp_var);

      /* Create the save stmt.  */
      save_call = build_function_call_expr
	(implicit_built_in_decls[BUILT_IN_STACK_SAVE], NULL_TREE);
      save = build (MODIFY_EXPR, ptr_type_node, tmp_var, save_call);

      /* Create the restore stmt.  */
      restore = build_function_call_expr
	(implicit_built_in_decls[BUILT_IN_STACK_RESTORE],
	 tree_cons (NULL_TREE, tmp_var, NULL_TREE));

      /* Build a try-finally block.
	 The statement list is the block of current statements.  */
      t = build (TRY_FINALLY_EXPR, void_type_node, cur_stmts, NULL_TREE);
      TREE_SIDE_EFFECTS (t) = 1;

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

  cur_binding_level = cur->prev;
  cur->prev = old_binding_levels;
  old_binding_levels = cur;

  return res;
}

static void
append_stmt (tree stmt)
{
  if (!EXPR_HAS_LOCATION (stmt))
    SET_EXPR_LOCATION (stmt, input_location);
  TREE_SIDE_EFFECTS (stmt) = 1;
  append_to_statement_list (stmt, &cur_stmts);
}

static void
push_stmts (tree stmts)
{
  TREE_CHAIN (stmts) = cur_stmts;
  cur_stmts = stmts;
}

static void
pop_stmts (void)
{
  tree prev;

  prev = cur_stmts;
  cur_stmts = TREE_CHAIN (prev);
  TREE_CHAIN (prev) = NULL_TREE;
}

static GTY(()) tree top;

static GTY(()) tree stack_alloc_function_ptr;
extern void ortho_fe_init (void);

static int
global_bindings_p (void)
{
  return cur_binding_level->prev == NULL;
}

static void
insert_block (tree b)
{
  abort ();
}

static tree
pushdecl (tree t)
{
  abort ();
}

static tree
builtin_function (const char *name,
		  tree type,
		  int function_code,
		  enum built_in_class class,
		  const char *library_name,
		  tree attrs ATTRIBUTE_UNUSED);

REAL_VALUE_TYPE fp_const_p5; /* 0.5 */
REAL_VALUE_TYPE fp_const_m_p5; /* -0.5 */
REAL_VALUE_TYPE fp_const_zero; /* 0.0 */

static bool
ortho_init (void)
{
  tree n;

  input_location.line = 0;

  /* Create a global binding.  */
  push_binding ();

  build_common_tree_nodes (0, 0);
  n = build_decl (TYPE_DECL, get_identifier ("int"), integer_type_node);
  push_decl (n);
  n = build_decl (TYPE_DECL, get_identifier ("char"), char_type_node);
  push_decl (n);
  size_type_node = unsigned_type_node;
  set_sizetype (unsigned_type_node);
  build_common_tree_nodes_2 (0);

  /* Create alloca builtin.  */
  {
    tree args_type = tree_cons (NULL_TREE, size_type_node, void_list_node);
    tree func_type = build_function_type (ptr_type_node, args_type);
       
    implicit_built_in_decls[BUILT_IN_ALLOCA] = builtin_function
      ("__builtin_alloca", func_type,
       BUILT_IN_ALLOCA, BUILT_IN_NORMAL, NULL, NULL_TREE);
    
    stack_alloc_function_ptr = build1
      (ADDR_EXPR, 
       build_pointer_type (func_type),
       implicit_built_in_decls[BUILT_IN_ALLOCA]);
  }

  {
    tree ptr_ftype = build_function_type (ptr_type_node, NULL_TREE);

    implicit_built_in_decls[BUILT_IN_STACK_SAVE] = builtin_function
      ("__builtin_stack_save", ptr_ftype,
       BUILT_IN_STACK_SAVE, BUILT_IN_NORMAL, NULL, NULL_TREE);
  }

  {
    tree ftype_ptr;

    ftype_ptr = build_function_type
      (void_type_node,
       tree_cons (NULL_TREE, ptr_type_node, NULL_TREE));

    implicit_built_in_decls[BUILT_IN_STACK_RESTORE] = builtin_function
      ("__builtin_stack_restore", ftype_ptr,
       BUILT_IN_STACK_RESTORE, BUILT_IN_NORMAL, NULL, NULL_TREE);
  }

  {
    REAL_VALUE_TYPE v;
    
    REAL_VALUE_FROM_INT (v, 1, 0, DFmode);
    real_ldexp (&fp_const_p5, &v, -1);
    
    REAL_VALUE_FROM_INT (v, -1, -1, DFmode);
    real_ldexp (&fp_const_m_p5, &v, -1);
    
    REAL_VALUE_FROM_INT (fp_const_zero, 0, 0, DFmode);
  }

  ortho_fe_init ();

  return true;
}

static void
ortho_finish (void)
{
}

static unsigned int
ortho_init_options (unsigned int argc, const char **argv)
{
  return CL_vhdl;
}

static bool
ortho_post_options (const char **pfilename)
{
  if (*pfilename == NULL || strcmp (*pfilename, "-") == 0)
    *pfilename = "*stdin*";

  /* Run the back-end.  */
  return false;
}

extern int lang_handle_option (const char *opt, const char *arg);

static int
ortho_handle_option (size_t code, const char *arg, int value)
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
      return lang_handle_option (opt, arg);
    default:
      /* The other options must have a joint argument.  */
      if (arg != NULL)
	{
	  size_t len1;
	  size_t len2;
	  char *nopt;
	  
	  len1 = strlen (opt);
	  len2 = strlen (arg);
	  nopt = alloca (len1 + len2 + 1);
	  memcpy (nopt, opt, len1);
	  memcpy (nopt + len1, arg, len2);
	  nopt[len1 + len2] = 0;
	  opt = nopt;
	}
      return lang_handle_option (opt, NULL);
    }
}

#if 0
void
linemap_init (void *s)
{
}
#endif

extern int lang_parse_file (const char *filename);

static void
ortho_parse_file (int debug)
{
  const char *filename;

  if (num_in_fnames == 0)
    filename = NULL;
  else
    filename = in_fnames[0];

  if (!lang_parse_file (filename))
    errorcount++;
  else
    {
      cgraph_finalize_compilation_unit ();
      cgraph_optimize ();
    }
}

static void
ortho_expand_function (tree fndecl)
{
  if (DECL_CONTEXT (fndecl) != NULL_TREE)
    {
      push_function_context ();
      tree_rest_of_compilation (fndecl);
      pop_function_context ();
    }
  else
      tree_rest_of_compilation (fndecl);
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
	TREE_ADDRESSABLE (n) = 1;
	return true;

      case COMPONENT_REF:
      case ARRAY_REF:
	n = TREE_OPERAND (n, 0);
	break;

      case FUNCTION_DECL:
      case CONSTRUCTOR:
	TREE_ADDRESSABLE (n) = 1;
	return true;

      case INDIRECT_REF:
	return true;

      default:
	abort ();
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
      abort ();
    }
}

/* Return a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.

   If LIBRARY_NAME is nonzero, use that for DECL_ASSEMBLER_NAME,
   the name to be called if we can't opencode the function.  If
   ATTRS is nonzero, use that for the function's attribute list.  */
static tree
builtin_function (const char *name,
		  tree type,
		  int function_code,
		  enum built_in_class class,
		  const char *library_name,
		  tree attrs ATTRIBUTE_UNUSED)
{
  tree decl = build_decl (FUNCTION_DECL, get_identifier (name), type);
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  if (library_name)
    SET_DECL_ASSEMBLER_NAME (decl, get_identifier (library_name));
  make_decl_rtl (decl);
  DECL_BUILT_IN_CLASS (decl) = class;
  DECL_FUNCTION_CODE (decl) = function_code;
  return decl;
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
  return type_for_size (GET_MODE_BITSIZE (mode), unsignedp);
}

/*  Return the unsigned version of a TYPE_NODE, a scalar type.  */
static tree
unsigned_type (tree type)
{
  return type_for_size (TYPE_PRECISION (type), 1);
}

/*  Return the signed version of a TYPE_NODE, a scalar type.  */
static tree
signed_type (tree type)
{
  return type_for_size (TYPE_PRECISION (type), 0);
}

/*  Return a type the same as TYPE except unsigned or signed according to
    UNSIGNEDP.  */
static tree
signed_or_unsigned_type (int unsignedp, tree type)
{
  if (!INTEGRAL_TYPE_P (type)
      || TYPE_UNSIGNED (type) == unsignedp)
    return type;
  else
    return type_for_size (TYPE_PRECISION (type), unsignedp);
}

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "vhdl"
#undef LANG_HOOKS_IDENTIFIER_SIZE
#define LANG_HOOKS_IDENTIFIER_SIZE sizeof (struct tree_identifier)
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT ortho_init
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH ortho_finish
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS ortho_init_options
#undef LANG_HOOKS_HANDLE_OPTION
#define LANG_HOOKS_HANDLE_OPTION ortho_handle_option
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS ortho_post_options
#undef LANG_HOOKS_HONOR_READONLY
#define LANG_HOOKS_HONOR_READONLY true
#undef LANG_HOOKS_TRUTHVALUE_CONVERSION
#define LANG_HOOKS_TRUTHVALUE_CONVERSION ortho_truthvalue_conversion
#undef LANG_HOOKS_MARK_ADDRESSABLE
#define LANG_HOOKS_MARK_ADDRESSABLE ortho_mark_addressable
#undef LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION
#define LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION ortho_expand_function

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
#define LANG_HOOKS_PARSE_FILE ortho_parse_file

#define pushlevel lhd_do_nothing_i
#define poplevel lhd_do_nothing_iii_return_null_tree
#define set_block lhd_do_nothing_t
#undef LANG_HOOKS_GETDECLS
#define LANG_HOOKS_GETDECLS lhd_return_null_tree_v

const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

/* Tree code classes.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

const enum tree_code_class tree_code_type[] = {
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

struct chain_constr_type
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
      if (constr->last != NULL_TREE)
	abort ();
      constr->first = el;
    }
  else
    TREE_CHAIN (constr->last) = el;
  constr->last = el;
}

struct list_constr_type
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
  ON_And_Then,
  ON_Or_Else,

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

  TRUTH_AND_EXPR,
  TRUTH_OR_EXPR,
  TRUTH_XOR_EXPR,
  TRUTH_ANDIF_EXPR,
  TRUTH_ORIF_EXPR,

  TRUTH_NOT_EXPR,
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

  left_type = TREE_TYPE (left);
  if (left_type != TREE_TYPE (right))
    abort ();

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
  return build1 (ON_op_to_TREE_CODE[kind], TREE_TYPE (operand), operand);
}

tree
new_compare_op (enum ON_op_kind kind, tree left, tree right, tree ntype)
{
  if (TREE_CODE (ntype) != BOOLEAN_TYPE)
    abort ();
  if (TREE_TYPE (left) != TREE_TYPE (right))
    abort ();
  return build2 (ON_op_to_TREE_CODE[kind], ntype, left, right);
}

tree
new_convert_ov (tree val, tree rtype)
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
      p5 = build_real (val_type, fp_const_p5);
      zero = build_real (val_type, fp_const_zero);
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
    abort ();

  return build1 (code, rtype, val);
}

tree
new_alloca (tree rtype, tree size)
{
  tree res;
  tree args;

  /* Must save stack except when at function level.  */
  if (cur_binding_level->prev != NULL
      && cur_binding_level->prev->prev != NULL)
    cur_binding_level->save_stack = 1;

  args = tree_cons (NULL_TREE, fold_convert (size_type_node, size), NULL_TREE);
  res = build3 (CALL_EXPR, ptr_type_node, stack_alloc_function_ptr,
		 args, NULL_TREE);
  return fold_convert (rtype, res);
}

tree
new_signed_literal (tree ltype, long long value)
{
  tree res;
  HOST_WIDE_INT lo;
  HOST_WIDE_INT hi;

  lo = value;
  if (sizeof (HOST_WIDE_INT) == sizeof (long long))
    hi = value >> (8 * sizeof (HOST_WIDE_INT) - 1);
  else
    hi = value >> (8 * sizeof (HOST_WIDE_INT));

  res = build_int_cst_wide (ltype, lo, hi);
  return res;
}

tree
new_unsigned_literal (tree ltype, unsigned long long value)
{
  tree res;
  unsigned HOST_WIDE_INT lo;
  unsigned HOST_WIDE_INT hi;

  lo = value;
  if (sizeof (HOST_WIDE_INT) == sizeof (long long))
    hi = 0;
  else
    hi = value >> (8 * sizeof (HOST_WIDE_INT));

  res = build_int_cst_wide (ltype, lo, hi);
  return res;
}

tree
new_null_access (tree ltype)
{
  tree res;

  res = build_int_cst_wide (ltype, 0, 0);
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
  if (sizeof (HOST_WIDE_INT) == sizeof (long long))
    hi = s >> (8 * sizeof (HOST_WIDE_INT) - 1);
  else
    hi = s >> (8 * sizeof (HOST_WIDE_INT));

  res = build_int_cst_wide (ltype, lo, hi);
  REAL_VALUE_FROM_INT (r_sign, lo, hi, DFmode);
  real_2expN (&r_exp, ex - 60);
  real_arithmetic (&r, MULT_EXPR, &r_sign, &r_exp);
  res = build_real (ltype, r);
  return res;
}

struct o_element_list
{
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

  res = build_decl (FIELD_DECL, ident, etype);
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
start_union_type (struct o_element_list *elements)
{
  elements->res =  make_node (UNION_TYPE);
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
      TYPE_MODE (res) = Pmode;
      layout_type (res);
      return res;
    }
  else
    return build_pointer_type (dtype);
}

void
finish_access_type (tree atype, tree dtype)
{
  if (TREE_CODE (atype) != POINTER_TYPE
      || TREE_TYPE (atype) != NULL_TREE)
    abort ();

  TREE_TYPE (atype) = dtype;
}

tree
new_array_type (tree el_type, tree index_type)
{
  return build_array_type (el_type, index_type);
}


tree
new_constrained_array_type (tree atype, tree length)
{
  tree range_type;
  tree index_type;
  tree len;
  tree one;

  index_type = TYPE_DOMAIN (atype);
  if (integer_zerop (length))
    {
      /*  Handle null array, by creating a one-length array...  */
      len = size_zero_node;
    }
  else
    {
      one = build_int_cstu (index_type, 1);
      len = build2 (MINUS_EXPR, index_type, length, one);
      len = fold (len);
    }

  range_type = build_range_type (index_type, size_zero_node, len);
  return build_array_type (TREE_TYPE (atype), range_type);
}

void
new_boolean_type (tree *res,
		  tree false_id, tree *false_e,
		  tree true_id, tree *true_e)
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
  chain_init (&list->chain);
  list->num = 0;
  list->size = size;
}

void
new_enum_literal (struct o_enum_list *list, tree ident, tree *res)
{
  *res = build_int_cstu (list->res, list->num);
  chain_append (&list->chain, tree_cons (ident, *res, NULL_TREE));
  list->num++;
}

void
finish_enum_type (struct o_enum_list *list, tree *res)
{
  *res = list->res;
  TYPE_VALUES (*res) = list->chain.first;
  TYPE_MIN_VALUE (*res) = TREE_VALUE (list->chain.first);
  TYPE_MAX_VALUE (*res) = TREE_VALUE (list->chain.last);
  TYPE_UNSIGNED (*res) = 1;
  TYPE_PRECISION (*res) = list->size;
  layout_type (*res);
}

struct o_record_aggr_list
{
  tree atype;
  struct chain_constr_type chain;
};

void
start_record_aggr (struct o_record_aggr_list *list, tree atype)
{
  list->atype = atype;
  chain_init (&list->chain);
}

void
new_record_aggr_el (struct o_record_aggr_list *list, tree value)
{
  chain_append (&list->chain, build_tree_list (NULL_TREE, value));
}

void
finish_record_aggr (struct o_record_aggr_list *list, tree *res)
{
  *res = build_constructor (list->atype, list->chain.first);
}
 

struct o_array_aggr_list
{
  tree atype;
  struct chain_constr_type chain;
};

void
start_array_aggr (struct o_array_aggr_list *list, tree atype)
{
  list->atype = atype;
  chain_init (&list->chain);
}

void
new_array_aggr_el (struct o_array_aggr_list *list, tree value)
{
  chain_append (&list->chain, build_tree_list (NULL_TREE, value));
}
 
void
finish_array_aggr (struct o_array_aggr_list *list, tree *res)
{
  *res = build_constructor (list->atype, list->chain.first);
}


tree
new_union_aggr (tree atype, tree field, tree value)
{
  tree el;
  tree res;

  el = build_tree_list (field, value);
  res = build_constructor (atype, el);
  TREE_CONSTANT (res) = 1;
  return res;
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
  tree res;
  tree el_ptr_type;
  tree el_type;
  tree res_ptr_type;

  /*  *((RES_TYPE *)(&ARR[INDEX]))
      convert ARR to a pointer, add index, and reconvert to array ?  */
  if (TREE_CODE (res_type) != ARRAY_TYPE)
    abort ();

  ortho_mark_addressable (arr);
  el_type = TREE_TYPE (TREE_TYPE (arr));
  el_ptr_type = build_pointer_type (el_type);

  res = build4 (ARRAY_REF, el_type, arr, index, NULL_TREE, NULL_TREE);
  res = build1 (ADDR_EXPR, el_ptr_type, res);
  res_ptr_type = build_pointer_type (res_type);
  res = build1 (NOP_EXPR, res_ptr_type, res);
  res = build1 (INDIRECT_REF, res_type, res);
  return res;
}

tree
new_selected_element (tree rec, tree el)
{
  tree res;

  if (TREE_CODE (TREE_TYPE (rec)) != RECORD_TYPE)
    abort ();

  res = build3 (COMPONENT_REF, TREE_TYPE (el), rec, el, NULL_TREE);
  return res;
}

tree
new_access_element (tree acc)
{
  tree acc_type;

  acc_type = TREE_TYPE (acc);
  if (TREE_CODE (acc_type) != POINTER_TYPE)
    abort ();

  return build1 (INDIRECT_REF, TREE_TYPE (acc_type), acc);
}

tree
new_offsetof (tree field, tree rtype)
{
  tree off;
  tree bit_off;
  HOST_WIDE_INT pos;
  tree res;

  off = DECL_FIELD_OFFSET (field);
  if (!host_integerp (off, 1))
    {
      /*  The offset must be a constant.  */
      abort ();
    }

  bit_off = DECL_FIELD_BIT_OFFSET (field);
  if (!host_integerp (bit_off, 1))
    {
      /*  The offset must be a constant.  */
      abort ();
    }

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

#if 0
static tree
ortho_build_addr (tree operand, tree atype)
{
  tree base = exp;

  while (handled_component_p (base))
    base = TREE_OPERAND (base, 0);

  if (DECL_P (base))
    TREE_ADDRESSABLE (base) = 1;

  return build1 (ADDR_EXPR, atype, exp);
}
#endif

tree
new_unchecked_address (tree lvalue, tree atype)
{
  tree res;

  if (TREE_CODE (lvalue) == INDIRECT_REF)
    {
      res = TREE_OPERAND (lvalue, 0);
    }
  else
    {
      ortho_mark_addressable (lvalue);

      if (TREE_TYPE (lvalue) != TREE_TYPE (atype))
	{
	  tree ptr;
	  ptr = build_pointer_type (TREE_TYPE (lvalue));
	  res = build1 (ADDR_EXPR, ptr, lvalue);
	}
      else
	res = build1 (ADDR_EXPR, atype, lvalue);
    }

  if (TREE_TYPE (res) != atype)
    res = fold (build1 (NOP_EXPR, atype, res));

  return res;

#if 0
  /* res = build_addr (lvalue, atype); */
  if (TREE_TYPE (res) != atype)
    {
      if (TREE_CODE (TREE_TYPE (res)) != POINTER_TYPE)
	abort ();
      res = build1 (NOP_EXPR, atype, res);
    }
  return res;
#endif
}

tree
new_address (tree lvalue, tree atype)
{
  return new_unchecked_address (lvalue, atype);
}

tree
new_global_address (tree lvalue, tree atype)
{
  return new_unchecked_address (lvalue, atype);
}

tree
new_global_unchecked_address (tree lvalue, tree atype)
{
  return new_unchecked_address (lvalue, atype);
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
  input_location.line = line;
}

void
new_type_decl (tree ident, tree atype)
{
  tree decl;

  TYPE_NAME (atype) = ident;
  decl = build_decl (TYPE_DECL, ident, atype);
  TYPE_STUB_DECL (atype) = decl;
  push_decl (decl);
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

  cst = build_decl (VAR_DECL, ident, atype);
  set_storage (cst, storage);
  TREE_READONLY (cst) = 1;
  push_decl (cst);
  switch (storage)
    {
    case o_storage_local:
      abort ();
    case o_storage_external:
      /*  We are at top level if Current_Function_Decl is null.  */
      rest_of_decl_compilation
	(cst, current_function_decl == NULL_TREE, 0);
      break;
    case o_storage_public:
    case o_storage_private:
      break;
    }
  *res = cst;
}

void
start_const_value (tree *cst)
{
}

void
finish_const_value (tree *cst, tree val)
{
  DECL_INITIAL (*cst) = val;
  TREE_CONSTANT (val) = 1;
  rest_of_decl_compilation
        (*cst, current_function_decl == NULL_TREE, 0);
}

void
new_var_decl (tree *res, tree ident, enum o_storage storage, tree atype)
{
  tree var;

  var = build_decl (VAR_DECL, ident, atype);
  if (current_function_decl != NULL_TREE)
    {    
      /*  Local variable. */
      TREE_STATIC (var) = 0;
      DECL_EXTERNAL (var) = 0;
      TREE_PUBLIC (var) = 0;
    }
  else
    set_storage (var, storage);

  push_decl (var);

  if (current_function_decl == NULL_TREE)
    rest_of_decl_compilation (var, 1, 0);

  *res = var;
}

struct o_inter_list
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

  r = build_decl (PARM_DECL, ident, atype);
  /* DECL_CONTEXT (Res, Xxx); */

  /*  Do type conversion: convert boolean and enums to int  */
  switch (TREE_CODE (atype))
    {
    case ENUMERAL_TYPE:
    case BOOLEAN_TYPE:
      DECL_ARG_TYPE (r) = integer_type_node;
    default:
      DECL_ARG_TYPE (r) = atype;
    }

  layout_decl (r, 0);

  chain_append (&interfaces->param_chain, r);
  ortho_list_append (&interfaces->param_list, atype);
  *res = r;
}

/*  Current function nest level, or the number of parents.  */
/* static int function_nest_level; */


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

  decl = build_decl (FUNCTION_DECL, interfaces->ident,
		     build_function_type (interfaces->rtype,
					  interfaces->param_list.first));
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
  result = build_decl (RESULT_DECL, NULL_TREE, interfaces->rtype);
  DECL_RESULT (decl) = result;
  DECL_CONTEXT (result) = decl;

  DECL_ARGUMENTS (decl) = interfaces->param_chain.first;
  /* Set DECL_CONTEXT of parameters.  */
  for (parm = interfaces->param_chain.first;
       parm != NULL_TREE;
       parm = TREE_CHAIN (parm))
    DECL_CONTEXT (parm) = decl;

  push_decl (decl);

  /* External functions are never nested.
     Remove their context, which is set by push_decl.  */
  if (interfaces->storage == o_storage_external)
    DECL_CONTEXT (decl) = NULL_TREE;

  if (is_global)
    rest_of_decl_compilation (decl, 1, 0);

  *res = decl;
}

void
start_subprogram_body (tree func)
{
  if (current_function_decl != DECL_CONTEXT (func))
    abort ();
  current_function_decl = func;

  /* The function is not anymore external.  */
  DECL_EXTERNAL (func) = 0;

  push_stmts (alloc_stmt_list ());
  push_binding ();
}

void
finish_subprogram_body (void)
{
  tree bind;
  tree func;
  tree parent;

  bind = pop_binding ();
  pop_stmts ();

  func = current_function_decl;
  DECL_INITIAL (func) = BIND_EXPR_BLOCK (bind);
  DECL_SAVED_TREE (func) = bind;

  /* Initialize the RTL code for the function.  */
  allocate_struct_function (func);

  /* Store the end of the function.  */
  cfun->function_end_locus = input_location;
  
  /* This function is being processed in whole-function mode.  */
  /* cfun->x_whole_function_mode_p = 1; */

  gimplify_function_tree (func);

  parent = DECL_CONTEXT (func);

  if (parent != NULL)
    cgraph_node (func);
  else
    cgraph_finalize_function (func, false);

  current_function_decl = parent;
  cfun = NULL;
}


void
new_debug_line_stmt (int line)
{
  input_location.line = line;
}

void
start_declare_stmt (void)
{
  push_stmts (alloc_stmt_list ());
  push_binding ();
}

void
finish_declare_stmt (void)
{
  tree bind;

  bind = pop_binding ();
  pop_stmts ();
  append_stmt (bind);
}


struct o_assoc_list
{
  tree subprg;
  struct list_constr_type list;
};

void
start_association (struct o_assoc_list *assocs, tree subprg)
{
  assocs->subprg = subprg;
  list_init (&assocs->list);
}

void
new_association (struct o_assoc_list *assocs, tree val)
{
  ortho_list_append (&assocs->list, val);
}

tree
new_function_call (struct o_assoc_list *assocs)
{
  return build3 (CALL_EXPR,
		 TREE_TYPE (TREE_TYPE (assocs->subprg)),
		 build_function_ptr (assocs->subprg),
		 assocs->list.first, NULL_TREE);
}

void
new_procedure_call (struct o_assoc_list *assocs)
{
  tree res;

  res = build3 (CALL_EXPR,
		TREE_TYPE (TREE_TYPE (assocs->subprg)),
		build_function_ptr (assocs->subprg),
		assocs->list.first, NULL_TREE);
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
  stmt = build1 (RETURN_EXPR, TREE_TYPE (value), assign);
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


struct o_if_block
{
  tree stmt;
};

void
start_if_stmt (struct o_if_block *block, tree cond)
{
  tree stmt;
  tree stmts;

  stmts = alloc_stmt_list ();
  stmt = build3 (COND_EXPR, void_type_node, cond, stmts, NULL_TREE);
  block->stmt = stmt;
  append_stmt (stmt);
  push_stmts (stmts);
}

void
new_elsif_stmt (struct o_if_block *block, tree cond)
{
  tree stmts;
  tree stmt;

  pop_stmts ();
  stmts = alloc_stmt_list ();
  stmt = build3 (COND_EXPR, void_type_node, cond, stmts, NULL_TREE);
  COND_EXPR_ELSE (block->stmt) = stmt;
  block->stmt = stmt;
  push_stmts (stmts);
}

void
new_else_stmt (struct o_if_block *block)
{
  tree stmts;

  pop_stmts ();
  stmts = alloc_stmt_list ();
  COND_EXPR_ELSE (block->stmt) = stmts;
  push_stmts (stmts);
}

void
finish_if_stmt (struct o_if_block *block)
{
  pop_stmts ();
}


struct o_loop_block
{
  tree beg_label;
  tree end_label;
};

struct o_snode
{
  tree beg_label;
  tree end_label;
};

/* Create an artificial label.  */
static tree
build_label (void)
{
  tree res;

  res = build_decl (LABEL_DECL, NULL_TREE, void_type_node);
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

struct o_case_block
{
  tree end_label;
  int add_break;
};

void
start_case_stmt (struct o_case_block *block, tree value)
{
  tree stmt;
  tree stmts;

  block->end_label = build_label ();
  block->add_break = 0;
  stmts = alloc_stmt_list ();
  stmt = build3 (SWITCH_EXPR, void_type_node, value, stmts, NULL_TREE);
  append_stmt (stmt);
  push_stmts (stmts);
}

void
start_choice (struct o_case_block *block)
{
  tree stmt;

  if (block->add_break)
    {
      stmt = build1 (GOTO_EXPR, void_type_node, block->end_label);
      append_stmt (stmt);

      block->add_break = 0;
    }
}

void
new_expr_choice (struct o_case_block *block, tree expr)
{
  tree stmt;
  
  stmt = build3 (CASE_LABEL_EXPR, void_type_node,
		 expr, NULL_TREE, create_artificial_label ());
  append_stmt (stmt);
}

void
new_range_choice (struct o_case_block *block, tree low, tree high)
{
  tree stmt;

  stmt = build3 (CASE_LABEL_EXPR, void_type_node,
		 low, high, create_artificial_label ());
  append_stmt (stmt);
}

void
new_default_choice (struct o_case_block *block)
{
  tree stmt;

  stmt = build3 (CASE_LABEL_EXPR, void_type_node,
		 NULL_TREE, NULL_TREE, create_artificial_label ());
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

  pop_stmts ();
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

#include "debug.h"
#include "gt-vhdl-ortho-lang.h"
#include "gtype-vhdl.h"
