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

with Logging; use Logging;
with Name_Table; use Name_Table;

package body Verilog.Errors is
   function "+" (N : Node) return Location_Type is
   begin
      return Get_Location (N);
   end "+";

   procedure Error_Msg_Sem
     (Loc : Location_Type; Msg : String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Errorout.Semantic, +Loc, Msg, Args);
   end Error_Msg_Sem;

   procedure Error_Msg_Sem
     (Loc : Location_Type; Msg : String; Arg : Earg_Type) is
   begin
      Report_Msg (Msgid_Error, Errorout.Semantic, +Loc, Msg, (1 => Arg));
   end Error_Msg_Sem;

   procedure Warning_Msg_Sem
     (Loc : Location_Type; Msg : String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Warning, Errorout.Semantic, +Loc, Msg, Args);
   end Warning_Msg_Sem;

   procedure Error_Msg_Elab
     (Msg: String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Elaboration, No_Source_Coord, Msg, Args);
   end Error_Msg_Elab;

   procedure Error_Msg_Elab
     (Msg: String; Arg1 : Earg_Type) is
   begin
      Error_Msg_Elab (Msg, Earg_Arr'(1 => Arg1));
   end Error_Msg_Elab;

   procedure Error_Msg_Exec
     (Loc : Location_Type; Msg : String; Args : Earg_Arr := No_Eargs) is
   begin
      Report_Msg (Msgid_Error, Errorout.Elaboration, +Loc, Msg, Args);
      raise Runtime_Error;
   end Error_Msg_Exec;

   procedure Error_Kind (Msg : String; N : Node) is
   begin
      Log (Msg);
      Log (": cannot handle ");
      Log_Line (Nkind'Image (Get_Kind (N)));
      raise Internal_Error;
   end Error_Kind;

   --  Return True when explained.
   function Explain_Non_Constant (N : Node) return Boolean is
   begin
      if Get_Is_Constant (N) then
         return False;
      end if;

      case Get_Kind (N) is
         when N_Binary_Op
           |  N_Short_Circuit_Op =>
            return Explain_Non_Constant (Get_Left (N))
              or else Explain_Non_Constant (Get_Right (N));
         when N_Name =>
            Error_Msg_Sem (+N, "%i is not constant", +Get_Identifier (N));
            return True;
         when others =>
            Error_Kind ("explain_non_constant", N);
      end case;
   end Explain_Non_Constant;

   procedure Error_Msg_Sem_Non_Constant (N : Node; Msg : String)
   is
      Status : Boolean;
   begin
      pragma Assert (not Get_Is_Constant (N));

      Report_Start_Group;
      Error_Msg_Sem (+N, Msg);

      Status := Explain_Non_Constant (N);

      --  Must have been explained.
      pragma Assert (Status);

      Report_End_Group;
   end Error_Msg_Sem_Non_Constant;

   function "+" (N : Node) return Earg_Type is
   begin
      return Make_Earg_Verilog_Node (Uns32 (N));
   end "+";

   function "+" (N : Token_Type) return Earg_Type is
   begin
      return Make_Earg_Verilog_Token (Token_Type'Pos (N));
   end "+";

   function Disp_Node (N : Node) return String
   is
      function Disp_Identifier (N : Node; Str : String) return String
      is
         Id : Name_Id;
      begin
         Id := Get_Identifier (N);
         return Str & " """ & Name_Table.Image (Id) & """";
      end Disp_Identifier;
   begin
      case Get_Kind (N) is
         when N_Wire
            | N_Wire_Direct =>
            return Disp_Identifier (N, "wire");
         when N_Input =>
            return Disp_Identifier (N, "input port");
         when N_Output =>
            return Disp_Identifier (N, "output port");
         when N_Var =>
            return Disp_Identifier (N, "variable");
         when others =>
            return "*node*";
      end case;
   end Disp_Node;

   procedure Verilog_Node_Handler
     (Format : Character; Err : Error_Record; Val : Uns32)
   is
      N : constant Node := Node (Val);
   begin
      case Format is
         when 'i' =>
            Output_Identifier (Get_Identifier (N));
         when 'l' =>
            Output_Location (Err, Get_Location (N));
         when 'n' =>
            Output_Message (Disp_Node (N));
         when others =>
            raise Internal_Error;
      end case;
   end Verilog_Node_Handler;

   procedure Verilog_Token_Handler
     (Format : Character; Err : Error_Record; Val : Uns32)
   is
      pragma Unreferenced (Err);
      Tok : constant Token_Type := Token_Type'Val (Val);
   begin
      case Format is
         when 't' =>
            case Tok is
               when Tok_Identifier =>
                  Output_Message ("an identifier");
               when Tok_Eof =>
                  Output_Message ("end of file");
               when others =>
                  Output_Message ("'");
                  Output_Message (Image (Tok));
                  Output_Message ("'");
            end case;
         when others =>
            raise Internal_Error;
      end case;
   end Verilog_Token_Handler;

   procedure Initialize is
   begin
      Register_Earg_Handler (Earg_Verilog_Node, Verilog_Node_Handler'Access);
      Register_Earg_Handler (Earg_Verilog_Token, Verilog_Token_Handler'Access);
   end Initialize;
end Verilog.Errors;
