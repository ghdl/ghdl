--  Error message handling for vhdl.
--  Copyright (C) 2002-2019 Tristan Gingold
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <gnu.org/licenses>.
with Types; use Types;
with Errorout; use Errorout;
with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Tokens;

package Vhdl.Errors is
   --  Register handlers so that errors can be handled.
   procedure Initialize;

   --  This kind can't be handled.
   procedure Error_Kind (Msg: String; N : Iir);
   procedure Error_Kind (Msg: String; Def : Iir_Predefined_Functions);
   pragma No_Return (Error_Kind);

   --  Conversions
   function "+" (V : Iir) return Earg_Type;
   function "+" (V : Vhdl.Tokens.Token_Type) return Earg_Type;

   --  Convert location.
   function "+" (L : Iir) return Location_Type;
   function "+" (L : Iir) return Source_Coord_Type;

      -- Disp a message during semantic analysis.
   procedure Warning_Msg_Sem (Id : Msgid_Warnings;
                              Loc : Location_Type;
                              Msg: String;
                              Args : Earg_Arr := No_Eargs);
   procedure Warning_Msg_Sem (Id : Msgid_Warnings;
                              Loc : Location_Type;
                              Msg: String;
                              Arg1 : Earg_Type);

   procedure Error_Msg_Sem (Loc: Location_Type;
                            Msg: String;
                            Args : Earg_Arr := No_Eargs);
   procedure Error_Msg_Sem
     (Loc: Location_Type; Msg: String; Arg1 : Earg_Type);

   --  Like Error_Msg_Sem, but a warning if -frelaxed or --std=93c.
   procedure Error_Msg_Sem_Relaxed (Loc : Iir;
                                    Id : Msgid_Warnings;
                                    Msg : String;
                                    Args : Earg_Arr := No_Eargs);

   -- Disp a message during elaboration (or configuration).
   procedure Error_Msg_Elab
     (Msg: String; Args : Earg_Arr := No_Eargs);
   procedure Error_Msg_Elab
     (Msg: String; Arg1 : Earg_Type);
   procedure Error_Msg_Elab
     (Loc: Iir; Msg: String; Args : Earg_Arr := No_Eargs);
   procedure Error_Msg_Elab
     (Loc: Iir; Msg: String; Arg1 : Earg_Type);

   --  Like Error_Msg_Elab, but a warning if -frelaxed or --std=93c.
   procedure Error_Msg_Elab_Relaxed (Loc : Iir;
                                     Id : Msgid_Warnings;
                                     Msg : String;
                                     Args : Earg_Arr := No_Eargs);

   --  Disp a warning durig elaboration (or configuration).
   procedure Warning_Msg_Elab (Id : Msgid_Warnings;
                               Loc : Iir;
                               Msg: String;
                               Arg1 : Earg_Type);
   procedure Warning_Msg_Elab (Id : Msgid_Warnings;
                               Loc : Iir;
                               Msg: String;
                               Args : Earg_Arr := No_Eargs);

   -- Disp a bug message.
   procedure Error_Internal (Expr: Iir; Msg: String := "");
   pragma No_Return (Error_Internal);

   -- Disp a node.
   -- Used for output of message.
   function Disp_Node (Node: Iir) return String;

   -- Disp a node location.
   -- Used for output of message.
   function Disp_Location (Node: Iir) return String;

   --  Disp non-terminal name from KIND.
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
   function Disp_Discrete (Dtype : Iir; Pos : Int64) return String;

   --  Disp the name of the type of NODE if known.
   --  Disp "unknown" if it is not known.
   --  Disp all possible types if it is an overload list.
   function Disp_Type_Of (Node : Iir) return String;

   --  Disp an error message when a pure function CALLER calls impure CALLEE.
   procedure Error_Pure
     (Origin : Report_Origin; Caller : Iir; Callee : Iir; Loc : Iir);

   --  Report an error message as type of EXPR does not match A_TYPE.
   --  Location is EXPR.
   procedure Error_Not_Match (Expr: Iir; A_Type: Iir);

   --  Disp interface mode MODE.
   function Get_Mode_Name (Mode : Iir_Mode) return String;

end Vhdl.Errors;
