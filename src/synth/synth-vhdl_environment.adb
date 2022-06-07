--  Environment definition for synthesis.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
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

with Name_Table;
with Errorout; use Errorout;

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Utils;

with Synth.Errors; use Synth.Errors;
with Synth.Vhdl_Context; use Synth.Vhdl_Context;

package body Synth.Vhdl_Environment is
   function Get_Bitwidth (Val : Memtyp) return Uns32 is
   begin
      return Val.Typ.W;
   end Get_Bitwidth;

   function Memtyp_To_Net (Ctxt : Builders.Context_Acc; Val : Memtyp)
                          return Net is
   begin
      return Get_Memtyp_Net (Ctxt, Val);
   end Memtyp_To_Net;

   function Partial_Memtyp_To_Net
     (Ctxt : Builders.Context_Acc; Val : Memtyp; Off : Uns32; Wd : Uns32)
     return Net is
   begin
      return Get_Partial_Memtyp_Net (Ctxt, Val, Off, Wd);
   end Partial_Memtyp_To_Net;

   procedure Warning_No_Assignment
     (Decl : Decl_Type; First_Off : Uns32; Last_Off : Uns32) is
   begin
      if Last_Off < First_Off then
         Warning_Msg_Synth
           (Warnid_Nowrite, +Decl.Obj, "no assignment for %n", +Decl.Obj);
      elsif Last_Off = First_Off then
         Warning_Msg_Synth (+Decl.Obj, "no assignment for offset %v of %n",
                            (1 => +First_Off, 2 => +Decl.Obj));
      else
         Warning_Msg_Synth (+Decl.Obj, "no assignment for offsets %v:%v of %n",
                            (+First_Off, +Last_Off, +Decl.Obj));
      end if;
   end Warning_No_Assignment;

   function Info_Subrange_Vhdl (Off : Width; Wd : Width; Bnd: Bound_Type)
                               return String
   is
      function Image (V : Int32) return String
      is
         Res : constant String := Int32'Image (V);
      begin
         if V >= 0 then
            return Res (2 .. Res'Last);
         else
            return Res;
         end if;
      end Image;
   begin
      case Bnd.Dir is
         when Dir_To =>
            if Wd = 1 then
               return Image (Bnd.Right - Int32 (Off));
            else
               return Image (Bnd.Left + Int32 (Bnd.Len - (Off + Wd)))
                 & " to "
                 & Image (Bnd.Right - Int32 (Off));
            end if;
         when Dir_Downto =>
            if Wd = 1 then
               return Image (Bnd.Right + Int32 (Off));
            else
               return Image (Bnd.Left - Int32 (Bnd.Len - (Off + Wd)))
                 & " downto "
                 & Image (Bnd.Right + Int32 (Off));
            end if;
      end case;
   end Info_Subrange_Vhdl;

   procedure Info_Subnet_Vhdl (Loc    : Location_Type;
                               Prefix : String;
                               Otype  : Vhdl.Nodes.Node;
                               Typ    : Type_Acc;
                               Off    : Width;
                               Wd     : Width) is
   begin
      case Typ.Kind is
         when Type_Bit
            | Type_Logic
            | Type_Discrete
            | Type_Float =>
            pragma Assert (Wd = Typ.W);
            pragma Assert (Off = 0);
            Info_Msg_Synth (+Loc, "  " & Prefix);
         when Type_File
            | Type_Protected
            | Type_Access
            | Type_Unbounded_Array
            | Type_Unbounded_Record
            | Type_Unbounded_Vector =>
            raise Internal_Error;
         when Type_Vector =>
            pragma Assert (Wd <= Typ.W);
            if Off = 0 and Wd = Typ.W then
               Info_Msg_Synth (+Loc, "  " & Prefix);
            else
               Info_Msg_Synth
                 (+Loc,
                  "  " & Prefix
                    & "(" & Info_Subrange_Vhdl (Off, Wd, Typ.Abound) & ")");
            end if;
         when Type_Slice
            | Type_Array =>
            Info_Msg_Synth (+Loc, "  " & Prefix & "(??)");
         when Type_Record =>
            declare
               Els : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Otype);
            begin
               for I in Typ.Rec.E'Range loop
                  declare
                     El : Rec_El_Type renames Typ.Rec.E (I);
                     Field : constant Vhdl.Nodes.Node :=
                       Get_Nth_Element (Els, Natural (I - 1));
                     Sub_Off : Uns32;
                     Sub_Wd : Width;
                  begin
                     if Off + Wd <= El.Offs.Net_Off then
                        --  Not covered anymore.
                        exit;
                     elsif Off >= El.Offs.Net_Off + El.Typ.W then
                        --  Not yet covered.
                        null;
                     elsif Off <= El.Offs.Net_Off
                       and then Off + Wd >= El.Offs.Net_Off + El.Typ.W
                     then
                        --  Fully covered.
                        Info_Msg_Synth
                          (+Loc,
                           "  " & Prefix & '.'
                             & Vhdl.Utils.Image_Identifier (Field));
                     else
                        --  Partially covered.
                        if Off < El.Offs.Net_Off then
                           Sub_Off := 0;
                           Sub_Wd := Wd - (El.Offs.Net_Off - Off);
                           Sub_Wd := Width'Min (Sub_Wd, El.Typ.W);
                        else
                           Sub_Off := Off - El.Offs.Net_Off;
                           Sub_Wd := El.Typ.W - (Off - El.Offs.Net_Off);
                           Sub_Wd := Width'Min (Sub_Wd, Wd);
                        end if;
                        Info_Subnet_Vhdl
                          (+Loc,
                           Prefix & '.' & Vhdl.Utils.Image_Identifier (Field),
                           Get_Type (Field), El.Typ, Sub_Off, Sub_Wd);
                     end if;
                  end;
               end loop;
            end;
      end case;
   end Info_Subnet_Vhdl;

   procedure Info_Subnet
     (Decl : Vhdl.Nodes.Node; Typ : Type_Acc; Off : Width; Wd : Width)
   is
      Loc : Location_Type;
   begin
      if Typ = null then
         --  Type is unknown, cannot display more infos.
         return;
      end if;

      if Off = 0 and Wd = Typ.W then
         --  Whole object, no need to give details.
         --  TODO: just say it ?
         return;
      end if;

      Loc := Vhdl.Nodes.Get_Location (Decl);
      Info_Msg_Synth (+Loc, " this concerns these parts of the signal:");
      Info_Subnet_Vhdl (Loc,
                        Name_Table.Image (Vhdl.Nodes.Get_Identifier (Decl)),
                        Vhdl.Nodes.Get_Type (Decl),
                        Typ, Off, Wd);
   end Info_Subnet;

   procedure Error_Multiple_Assignments
     (Decl : Decl_Type; First_Off : Uns32; Last_Off : Uns32) is
   begin
      Error_Msg_Synth (+Decl.Obj, "multiple assignments for %i offsets %v:%v",
                       (+Decl.Obj, +First_Off, +Last_Off));
      Info_Subnet (Decl.Obj, Decl.Typ, First_Off, Last_Off + 1 - First_Off);
   end Error_Multiple_Assignments;

end Synth.Vhdl_Environment;
