--  Verilog preprocessor macro structures
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

with Ada.Unchecked_Deallocation;
with Dyn_Tables;
with Types; use Types;
with Verilog.Tokens; use Verilog.Tokens;

package Verilog.Macros is
   type Token_Data_Kind is
     (
      Token_Data_None,
      Token_Data_Id,
      Token_Data_Number_32,
      Token_Data_String,
      Token_Data_Arg,
      Token_Data_Macro,
      Token_Data_Pp_String
     );

   type Macro_Type;
   type Macro_Acc is access Macro_Type;

   type Token_Data_Type (Kind : Token_Data_Kind := Token_Data_None) is record
      case Kind is
         when Token_Data_None =>
            null;
         when Token_Data_Id =>
            Id : Name_Id;

         when Token_Data_Number_32 =>
            Number_32 : Logic_32;

         when Token_Data_String =>
            Str_Id : String8_Id;
            Str_Len : Natural;

         when Token_Data_Arg =>
            Arg_Idx : Natural;

         when Token_Data_Macro =>
            Macro_Id : Name_Id;
            Macro : Macro_Acc;

         when Token_Data_Pp_String =>
            Pp_End : Source_Ptr;
            Pp_Arg : Integer;
      end case;
   end record;

   type Token_Element is record
      Token : Token_Type;

      --  True if the token was preceeded by spaces.
      Spaces : Boolean;

      Loc : Location_Type;

      Data : Token_Data_Type;
   end record;

   package Token_Table is new Dyn_Tables
     (Table_Component_Type => Token_Element,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1);

   subtype Token_Element_Array is Token_Table.Table_Type;
   type Token_Array_Acc is access Token_Element_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Token_Element_Array, Token_Array_Acc);

   type Macro_Arg_Type is record
      Id : Name_Id;
      Default : Token_Array_Acc;
   end record;

   package Macro_Args_Table is new Dyn_Tables
     (Table_Component_Type => Macro_Arg_Type,
      Table_Index_Type => Natural,
      Table_Low_Bound => 1);

   subtype Macro_Args_Array is Macro_Args_Table.Table_Type;
   type Macro_Args_Arr_Acc is access Macro_Args_Array;

   procedure Free is new Ada.Unchecked_Deallocation
     (Macro_Args_Array, Macro_Args_Arr_Acc);

   type Macro_Type is record
      --  Name of the macro.
      Id : Name_Id;

      --  The definition of the macro is tokenized.
      Toks : Token_Array_Acc;

      --  Macro formal arguments.  Must be null if there is no arguments.
      Args : Macro_Args_Arr_Acc;

      --  Position of the macro (first character of text).
      File : Source_File_Entry;
      Pos : Source_Ptr;

      --  Double link to brothers.
      Next : Macro_Acc;
      Prev : Macro_Acc;
   end record;
end Verilog.Macros;
