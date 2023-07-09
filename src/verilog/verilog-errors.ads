--  Verilog error routines
--  Copyright (C) 2023 Tristan Gingold
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
with Verilog.Tokens; use Verilog.Tokens;
with Verilog.Nodes; use Verilog.Nodes;

package Verilog.Errors is
   procedure Error_Kind (Msg : String; N : Node);
   pragma No_Return (Error_Kind);

   function "+" (N : Node) return Earg_Type;
   function "+" (N : Token_Type) return Earg_Type;

   function "+" (N : Node) return Location_Type;

   procedure Error_Msg_Sem
     (Loc : Location_Type; Msg : String; Args : Earg_Arr := No_Eargs);
   procedure Error_Msg_Sem
     (Loc : Location_Type; Msg : String; Arg : Earg_Type);
   procedure Warning_Msg_Sem
     (Loc : Location_Type; Msg : String; Args : Earg_Arr := No_Eargs);

   procedure Error_Msg_Elab
     (Msg: String; Arg1 : Earg_Type);

   procedure Error_Msg_Exec
     (Loc : Location_Type; Msg : String; Args : Earg_Arr := No_Eargs);

   --  Emit error message MSG as N is a non-constant expression.
   --  Use N as a location, and try to explain why.
   procedure Error_Msg_Sem_Non_Constant (N : Node; Msg : String);

   procedure Initialize;
end Verilog.Errors;
