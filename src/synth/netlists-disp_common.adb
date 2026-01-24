--  Routine to disp net names.
--  Copyright (C) 2026 Tristan Gingold
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
with Outputs; use Outputs;
with Types_Utils;

with Grt.Types;
with Grt.Fcvt;

with Netlists.Gates; use Netlists.Gates;
with Netlists.Utils;

package body Netlists.Disp_Common is
   procedure Put_Id (N : Name_Id; Lang : Language_Range; Is_Extended : Boolean)
   is
      Img : constant String := Name_Table.Image (N);
   begin
      if Is_Extended then
         --  Name N is printed within an extended identifier.
         case Lang is
            when Language_Vhdl
              | Language_Verilog =>
               if Img (Img'First) = '\' then
                  Wr (Img (Img'First + 1 .. Img'Last - 1));
                  return;
               end if;
         end case;
      end if;

      Wr (Img);
   end Put_Id;

   procedure Put_Name_Version (N : Sname) is
   begin
      Wr_Uns32 (Get_Sname_Version (N));
   end Put_Name_Version;

   --  Return True IFF N is an extended identifier.
   function Is_Extended_Sname (N : Sname) return Boolean is
   begin
      if N = No_Sname then
         return False;
      end if;

      case Get_Sname_Kind (N) is
         when Sname_User
           | Sname_Version =>
            return Is_Extended_Sname (Get_Sname_Prefix (N));
         when Sname_System =>
            return False;
         when Sname_Field =>
            return True;
         when Sname_Unique =>
            return False;
      end case;
   end Is_Extended_Sname;

   procedure Put_Name_Inner
     (N : Sname; Lang : Language_Range; Is_Extended : Boolean)
   is
      Kind : constant Sname_Kind := Get_Sname_Kind (N);
      Prefix : Sname;
   begin
      --  Do not crash on No_Name.
      if N = No_Sname then
         Wr ("*nil*");
         return;
      end if;

      if Kind in Sname_Kind_Prefix then
         Prefix := Get_Sname_Prefix (N);
         if Prefix /= No_Sname then
            Put_Name_Inner (Prefix, Lang, Is_Extended);
            if Kind /= Sname_Field then
               Wr ("_");
            end if;
         end if;
      end if;

      case Kind is
         when Sname_User =>
            Put_Id (Get_Sname_Suffix (N), Lang, Is_Extended);
         when Sname_System =>
            Put_Id (Get_Sname_Suffix (N), Lang, Is_Extended);
         when Sname_Field =>
            Wr ("[");
            Put_Id (Get_Sname_Suffix (N), Lang, Is_Extended);
            Wr ("]");
         when Sname_Version
           | Sname_Unique =>
            Wr ("n");
            Put_Name_Version (N);
      end case;
   end Put_Name_Inner;

   procedure Put_Extended_End (Lang : Language_Range) is
   begin
      case Lang is
         when Language_Vhdl =>
            Wr ("\");
         when Language_Verilog =>
            Wr (" ");
      end case;
   end Put_Extended_End;

   procedure Put_Name (N : Sname; Lang : Language_Range)
   is
      Is_Extended : constant Boolean := Is_Extended_Sname (N);
   begin
      if Is_Extended then
         Wr ("\");
      end if;

      Put_Name_Inner (N, Lang, Is_Extended);

      if Is_Extended then
         Put_Extended_End (Lang);
      end if;
   end Put_Name;

   procedure Put_Interface_Name (N : Sname; Lang : Language_Range) is
   begin
      --  Do not crash on No_Name.
      if N = No_Sname then
         Wr ("*nil*");
         return;
      end if;

      --  Interface names are not versionned.
      if Get_Sname_Kind (N) in Sname_System .. Sname_Field then
         Put_Name (N, Lang);
      else
         Wr ("*err*");
      end if;
   end Put_Interface_Name;

   procedure Disp_Net_Name (N : Net; Lang : Language_Range) is
   begin
      if N = No_Net then
         Wr ("<unassigned>");
         return;
      end if;

      declare
         Inst : constant Instance := Get_Net_Parent (N);
         Idx : constant Port_Idx := Get_Port_Idx (N);
         M : Module;
         Id : Module_Id;
         Inst_Name : Sname;
      begin
         if Is_Self_Instance (Inst) then
            --  For ports of the current module, simply use the port name.
            Put_Name (Get_Input_Desc (Get_Module (Inst), Idx).Name, Lang);
         else
            Inst_Name := Get_Instance_Name (Inst);
            M := Get_Module (Inst);
            Id := Get_Id (M);
            case Id is
               when Id_Inout
                 | Id_Iinout
                 | Id_User_None .. Module_Id'Last =>
                  --  Gates with multiple outputs.
                  Wr ("\");
                  Put_Name_Inner (Inst_Name, Lang, True);
                  Wr (".");
                  Put_Name_Inner (Get_Output_Desc (M, Idx).Name, Lang, True);
                  Put_Extended_End (Lang);
               when others =>
                  Put_Name (Inst_Name, Lang);
            end case;
         end if;
      end;
   end Disp_Net_Name;

   procedure Put_Instance_Name (Name : Sname; Lang : Language_Range) is
   begin
      if Get_Sname_Kind (Name) = Sname_Unique then
         --  The name of the instance has the same name as a signal.
         Wr ("inst_");
         Put_Name_Version (Name);
      else
         Put_Name (Name, Lang);
      end if;
   end Put_Instance_Name;

   function Need_Name (Inst : Instance) return Boolean
   is
      use Netlists.Utils;
      Id : constant Module_Id := Get_Id (Inst);
   begin
      case Id is
         when Id_Extract
           | Id_Dyn_Extract
           | Id_Dyn_Insert
           | Id_Bmux
           | Id_Utrunc
           | Id_Strunc =>
            return True;
         when Id_User_None .. Module_Id'Last =>
            return True;
         when others =>
            return False;
      end case;
   end Need_Name;

   function Need_Edge (Inst : Instance) return Boolean
   is
      use Netlists.Utils;
      I : Input;
      Parent : Instance;
   begin
      I := Get_First_Sink (Get_Output (Inst, 0));
      while I /= No_Input loop
         Parent := Get_Input_Parent (I);
         case Get_Id (Parent) is
            when Id_Dff
              | Id_Adff
              | Id_Idff
              | Id_Iadff =>
               if I /= Get_Input (Parent, 0) then
                  return True;
               end if;
            when Id_Mem_Rd_Sync
              | Id_Mem_Wr_Sync =>
               if I /= Get_Input (Parent, 2) then
                  return True;
               end if;
            when others =>
               return True;
         end case;
         I := Get_Next_Sink (I);
      end loop;
      return False;
   end Need_Edge;

   procedure Put_Id (N : Name_Id) is
   begin
      Wr (Name_Table.Image (N));
   end Put_Id;

   procedure Disp_Binary_Digit (Va : Uns32; Zx : Uns32; I : Natural) is
   begin
      Wr (Bchar (((Va / 2**I) and 1) + ((Zx / 2**I) and 1) * 2));
   end Disp_Binary_Digit;

   procedure Disp_Binary_Digits (Va : Uns32; Zx : Uns32; W : Natural) is
   begin
      for I in 1 .. W loop
         Disp_Binary_Digit (Va, Zx, W - I);
      end loop;
   end Disp_Binary_Digits;

   procedure Disp_Pval_Binary_Digits (Pv : Pval)
   is
      Len : constant Uns32 := Get_Pval_Length (Pv);
      V   : Logic_32;
      Off : Uns32;
   begin
      if Len = 0 then
         return;
      end if;

      V := Read_Pval (Pv, (Len - 1) / 32);
      for I in reverse 0 .. Len - 1 loop
         Off := I mod 32;
         if Off = 31 then
            V := Read_Pval (Pv, I / 32);
         end if;
         Disp_Binary_Digit (V.Val, V.Zx, Natural (Off));
      end loop;
   end Disp_Pval_Binary_Digits;

   procedure Disp_Pval_Binary (Pv : Pval) is
   begin
      Wr ('"');
      Disp_Pval_Binary_Digits (Pv);
      Wr ('"');
   end Disp_Pval_Binary;

   procedure Disp_Pval_Integer (Pv : Pval)
   is
      use Types_Utils;
      Len : constant Uns32 := Get_Pval_Length (Pv);
      pragma Assert (Len <= 64);
      Lg : Logic_32;
      V : Uns64;
      Res : Int64;
   begin
      Lg := Read_Pval (Pv, 0);
      pragma Assert (Lg.Zx = 0);
      V := Uns64 (Lg.Val);
      if Len > 32 then
         Lg := Read_Pval (Pv, 1);
         pragma Assert (Lg.Zx = 0);
         V := V or Shift_Left (Uns64 (Lg.Val), 32);
      end if;

      --  Sign extend.
      V := Shift_Left (V, Natural (64 - Len));
      V := Shift_Right_Arithmetic (V, Natural (64 - Len));

      Res := To_Int64 (V);
      Wr_Trim (Int64'Image (Res));
   end Disp_Pval_Integer;

   procedure Disp_Pval_String (Pv : Pval)
   is
      Len : constant Uns32 := Get_Pval_Length (Pv);
      pragma Assert (Len rem 8 = 0);
      V   : Logic_32;
      Off : Uns32;
      C   : Uns32;
   begin
      Wr ('"');
      if Len > 0 then
         V := Read_Pval (Pv, (Len - 1) / 32);
         for I in reverse 0 .. (Len / 8) - 1 loop
            Off := I mod 4;
            if Off = 3 then
               V := Read_Pval (Pv, I / 4);
            end if;
            pragma Assert (V.Zx = 0);
            C := Shift_Right (V.Val, Natural (8 * Off)) and 16#ff#;
            Wr (Character'Val (C));
         end loop;
      end if;
      Wr ('"');
   end Disp_Pval_String;

   procedure Disp_Pval_Fp64 (Pv : Pval)
   is
      use Types_Utils;
      pragma Assert (Get_Pval_Length (Pv) = 64);
      Lg : Logic_32;
      F : Uns64;
      Res : Fp64;
      Last : Natural;
      Img : String (1 .. 24);
   begin
      F := 0;
      for I in 0 .. 1 loop
         Lg := Read_Pval (Pv, Uns32 (I));
         pragma Assert (Lg.Zx = 0);
         F := F or Shift_Left (Uns64 (Lg.Val), I * 32);
      end loop;

      Res := To_Fp64 (F);
      Grt.Fcvt.Format_Image (Img, Last, Grt.Types.Ghdl_F64 (Res));
      Wr_Trim (Img (1 .. Last));
   end Disp_Pval_Fp64;
end Netlists.Disp_Common;
