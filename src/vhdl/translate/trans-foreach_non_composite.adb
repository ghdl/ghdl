--  Iir to ortho translator.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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

with Vhdl.Errors; use Vhdl.Errors;
with Trans.Chap3;
with Trans.Chap6;

procedure Trans.Foreach_Non_Composite (Targ      : Mnode;
                                       Targ_Type : Iir;
                                       Data      : Data_Type)
is
   use Trans.Helpers;

   Type_Info : constant Type_Info_Acc := Get_Info (Targ_Type);
begin
   case Type_Info.Type_Mode is
      when Type_Mode_Scalar =>
         Do_Non_Composite (Targ, Targ_Type, Data);
      when Type_Mode_Arrays =>
         declare
            El_Type : constant Iir := Get_Element_Subtype (Targ_Type);
            Var_El         : Mnode;
            El_Base        : Mnode;
            Var_Array      : Mnode;
            Var_Length     : O_Dnode;
            Var_I          : O_Dnode;
            Label          : O_Snode;
            Sub_Data       : Data_Type;
            Composite_Data : Composite_Data_Type;
         begin
            Open_Temp;
            Var_Array := Stabilize (Targ);
            Var_Length := Create_Temp (Ghdl_Index_Type);
            New_Assign_Stmt
              (New_Obj (Var_Length),
               Chap3.Get_Array_Length (Var_Array, Targ_Type));
            Composite_Data :=
              Prepare_Data_Array (Var_Array, Targ_Type, Data);
            if True then
               Var_I := Create_Temp (Ghdl_Index_Type);
            else
               New_Var_Decl
                 (Var_I, Wki_I, O_Storage_Local, Ghdl_Index_Type);
            end if;
            Var_El :=
              Chap3.Create_Maybe_Fat_Array_Element (Var_Array, Targ_Type);
            Init_Var (Var_I);
            Start_Loop_Stmt (Label);
            Gen_Exit_When
              (Label, New_Compare_Op (ON_Ge,
                                      New_Value (New_Obj (Var_I)),
                                      New_Value (New_Obj (Var_Length)),
                                      Ghdl_Bool_Type));
            Sub_Data := Update_Data_Array
              (Composite_Data, Targ_Type, Var_I);
            El_Base := Chap3.Index_Array (Var_Array, Targ_Type,
                                          New_Value (New_Obj (Var_I)));
            Foreach_Non_Composite
              (Chap3.Assign_Maybe_Fat_Array_Element (Var_El, El_Base),
               El_Type, Sub_Data);
            Inc_Var (Var_I);
            Finish_Loop_Stmt (Label);
            Finish_Data_Array (Composite_Data);
            Close_Temp;
         end;
      when Type_Mode_Records =>
         declare
            List           : constant Iir_Flist :=
              Get_Elements_Declaration_List (Targ_Type);
            Var_Record     : Mnode;
            Sub_Data       : Data_Type;
            Composite_Data : Composite_Data_Type;
            El             : Iir_Element_Declaration;
         begin
            Open_Temp;
            Var_Record := Stabilize (Targ);
            Composite_Data :=
              Prepare_Data_Record (Var_Record, Targ_Type, Data);
            for I in Flist_First .. Flist_Last (List) loop
               El := Get_Nth_Element (List, I);
               Sub_Data := Update_Data_Record (Composite_Data, Targ_Type, El);
               Foreach_Non_Composite
                 (Chap6.Translate_Selected_Element (Var_Record, El),
                  Get_Type (El),
                  Sub_Data);
            end loop;
            Finish_Data_Record (Composite_Data);
            Close_Temp;
         end;
      when others =>
         Error_Kind ("foreach_non_composite/"
                       & Type_Mode_Type'Image (Type_Info.Type_Mode),
                     Targ_Type);
   end case;
end Trans.Foreach_Non_Composite;
