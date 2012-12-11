--  Error message handling.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Types; use Types;
with Iirs; use Iirs;

package Errorout is
   Option_Error: exception;
   Parse_Error: exception;
   Compilation_Error: exception;
   Simulation_Error: exception;
   Elaboration_Error : exception;

   -- This exception is raised when a constraint error is detected during
   -- an evaluation of an expression.
   Execution_Constraint_Error: exception;

   -- This kind can't be handled.
   --procedure Error_Kind (Msg: String; Kind: Iir_Kind);
   procedure Error_Kind (Msg: String; An_Iir: in Iir);
   procedure Error_Kind (Msg: String; Def : Iir_Predefined_Functions);
   procedure Error_Kind (Msg : String; N : PSL_Node);
   pragma No_Return (Error_Kind);

   -- Raise when an assertion of failure severity error fails.
   Assertion_Failure: exception;

   -- The number of errors (ie, number of calls to error_msg*).
   Nbr_Errors: Natural := 0;

   -- Disp an error, prepended with program name.
   procedure Error_Msg (Msg: String);

   -- Disp an error, prepended with program name, and raise option_error.
   -- This is used for errors before initialisation, such as bad option or
   -- bad filename.
   procedure Error_Msg_Option (Msg: String);
   pragma No_Return (Error_Msg_Option);

   --  Same as Error_Msg_Option but do not raise Option_Error.
   procedure Error_Msg_Option_NR (Msg: String);

   -- Disp an error location (using AN_IIR location) using the standard
   -- format `file:line:col: '.
   procedure Disp_Iir_Location (An_Iir: Iir);

   -- Disp a warning.
   procedure Warning_Msg (Msg: String);
   procedure Warning_Msg_Parse (Msg: String);
   procedure Warning_Msg_Sem (Msg: String; Loc : Iir);
   procedure Warning_Msg_Elab (Msg: String; Loc : Iir);
   procedure Warning_Msg_Sem (Msg: String; Loc : Location_Type);

   -- Disp a message during scan.
   -- The current location is automatically displayed before the message.
   procedure Error_Msg_Scan (Msg: String);
   procedure Warning_Msg_Scan (Msg: String);

   -- Disp a message during parse
   -- The location of the current token is automatically displayed before
   -- the message.
   procedure Error_Msg_Parse (Msg: String);
   procedure Error_Msg_Parse (Msg: String; Loc : Iir);
   procedure Error_Msg_Parse (Msg: String; Loc : Location_Type);

   -- Disp a message during semantic analysis.
   -- an_iir is used for location and current token.
   procedure Error_Msg_Sem (Msg: String; Loc: Iir);
   procedure Error_Msg_Sem (Msg: String; Loc: PSL_Node);
   procedure Error_Msg_Sem (Msg: String; Loc: Location_Type);

   -- Disp a message during elaboration.
   procedure Error_Msg_Elab (Msg: String);
   procedure Error_Msg_Elab (Msg: String; Loc: Iir);

   -- Disp a message during execution.
   procedure Error_Msg_Exec (Msg: String; Loc: Iir);
   pragma No_Return (Error_Msg_Exec);

   procedure Warning_Msg_Exec (Msg: String; Loc: Iir);

   -- Disp a message for a constraint error.
   -- And raise the exception execution_constraint_error.
   procedure Error_Msg_Constraint (Expr: Iir);

   -- Disp a bug message.
   procedure Error_Internal (Expr: Iir; Msg: String := "");
   pragma No_Return (Error_Internal);

   -- Disp a node.
   -- Used for output of message.
   function Disp_Node (Node: Iir) return String;

   -- Disp a node location.
   -- Used for output of message.
   function Disp_Location (Node: Iir) return String;
   function Get_Location_Str (Loc : Location_Type; Filename : Boolean := True)
     return String;

   -- Disp non-terminal name from KIND.
   function Disp_Name (Kind : Iir_Kind) return String;

   --  SUBPRG must be a subprogram declaration or an enumeration literal
   --  declaration.
   --  Returns:
   --   "enumeration literal XX [ return TYPE ]"
   --   "function XXX [ TYPE1, TYPE2 return TYPE ]"
   --   "procedure XXX [ TYPE1, TYPE2 ]"
   --   "implicit function XXX [ TYPE1, TYPE2 return TYPE ]"
   --   "implicit procedure XXX [ TYPE1, TYPE2 ]"
   function Disp_Subprg (Subprg : Iir) return String;

   --  Print element POS of discrete type DTYPE.
   function Disp_Discrete (Dtype : Iir; Pos : Iir_Int64) return String;

   --  Disp the name of the type of NODE if known.
   --  Disp "unknown" if it is not known.
   --  Disp all possible types if it is an overload list.
   function Disp_Type_Of (Node : Iir) return String;

   --  Disp an error message when a pure function CALLER calls impure CALLEE.
   procedure Error_Pure (Caller : Iir; Callee : Iir; Loc : Iir);

   --  Report an error message as type of EXPR does not match A_TYPE.
   --  Location is LOC.
   procedure Error_Not_Match (Expr: Iir; A_Type: Iir; Loc : Iir);


end Errorout;
