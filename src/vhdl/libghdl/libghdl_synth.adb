--  Library interface for the synthesizer.
--  Copyright (C) 2026 Tristan Gingold
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

with Options; use Options;
with Errorout;
with Errorout.Console;
with Libraries;
with Outputs;
with Name_Table;
with Types_Utils;

with Bug;

with Netlists.Errors;
with Netlists.Disp_Common;

with Vhdl.Std_Package;
with Vhdl.Configuration;
with Vhdl.Errors;
with Vhdl.Chars;

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Insts;
with Elab.Vhdl_Annotations;
with Elab.Vhdl_Types;

with Synth;
with Synth.Flags;
with Synth.Vhdl_Foreign;
with Synth.Context; use Synth.Context;
with Synth.Vhdl_Expr;
with Synth.Vhdl_Insts;

with Synthesis;

with Ghdlsynth; use Ghdlsynth;
with Ghdlmain; use Ghdlmain;

package body Libghdl_Synth is
   procedure Init_For_Ghdl_Synth is
   begin
      Ghdlsynth.Register_Commands;
      Errorout.Console.Install_Handler;
      Options.Initialize;
      Netlists.Errors.Initialize;
      Synth.Vhdl_Foreign.Initialize;
   end Init_For_Ghdl_Synth;

   function Ghdl_Synth
     (Init : Natural; Argc : Natural; Argv : C_String_Array_Acc)
     return Module
   is
      use Vhdl.Configuration;
      Args : String_Acc_Array (1 .. Argc);
      Res : Base_Instance_Acc;
      Cmd : Command_Synth;
      First_Arg : Natural;
      Config : Node;
      Inst : Synth_Instance_Acc;
   begin
      --  Create arguments list.
      for I in 0 .. Argc - 1 loop
         declare
            Arg : constant Ghdl_C_String := Argv (I);
         begin
            Args (I + 1) := new String'(Arg (1 .. strlen (Arg)));
         end;
      end loop;

      --  Forget any previous errors.
      Errorout.Nbr_Errors := 0;

      --  Decode the options.  This is a little bit convoluted...
      Decode_Command_Options (Cmd, Args, First_Arg);

      --  Do the real work!
      Config := Ghdl_Synth_Configure
        (Init /= 0,
         Cmd.Vendor_Libraries (1 .. Cmd.Nbr_Vendor_Libraries),
         Args (First_Arg .. Args'Last));
      if Config = Null_Iir then
         return No_Module;
      end if;

      Inst := Elab.Vhdl_Insts.Elab_Top_Unit (Get_Library_Unit (Config));

      pragma Assert (Is_Expr_Pool_Empty);

      Res := Synthesis.Synth_Design (Config, Inst, Cmd.Top_Encoding);
      if Res = null then
         return No_Module;
      end if;

      pragma Assert (Is_Expr_Pool_Empty);

      Disp_Design (Cmd, Format_None, Res, Config, Inst);

      --  De-elaborate all packages, so that they could be re-used for
      --  synthesis of a second design.
      --  FIXME: move to vhdl.configure ?
      for I in Design_Units.First .. Design_Units.Last loop
         Set_Elab_Flag (Design_Units.Table (I), False);
      end loop;
      Set_Elab_Flag (Vhdl.Std_Package.Std_Standard_Unit, False);

      pragma Assert (Is_Expr_Pool_Empty);

      Elab.Vhdl_Annotations.Finalize_Annotate;
      Synth.Vhdl_Insts.Free_Base_Instance;
      return Res.Top_Module;

   exception
      when Option_Error
        | Errorout.Compilation_Error =>
         return No_Module;
      when E: others =>
         --  Avoid possible issues with exceptions...
         Bug.Disp_Bug_Box (E);
         return No_Module;
   end Ghdl_Synth;

   Global_Base : Base_Instance_Acc;
   pragma Unreferenced (Global_Base);

   function Ghdl_Synth_Read (Init : Natural;
                             Argc : Natural;
                             Argv : C_String_Array_Acc;
                             Cb : Ghdl_Synth_Read_Cb;
                             Cb_Arg : Pointer_Generic) return Integer
   is
      use Errorout;
      use Vhdl.Errors;
      Args : String_Acc_Array (1 .. Argc);
      Cmd : Command_Synth;
      First_Arg : Natural;
      File, Design, Unit : Iir;
   begin
      --  Create arguments list.
      for I in 0 .. Argc - 1 loop
         declare
            Arg : constant Ghdl_C_String := Argv (I);
         begin
            Args (I + 1) := new String'(Arg (1 .. strlen (Arg)));
         end;
      end loop;

      --  Forget any previous errors.
      Errorout.Nbr_Errors := 0;

      --  Decode the options.  This is a little bit convoluted...
      Decode_Command_Options (Cmd, Args, First_Arg);

      if Init /= 0 then
         Synth_Compile_Init (First_Arg <= Args'Last);
         Elab.Vhdl_Insts.Elab_Top_Init;
         Make_Root_Instance;
         Global_Base := Synthesis.Make_Base_Instance;
      end if;

      Mark_Vendor_Libraries
        (Cmd.Vendor_Libraries (1 .. Cmd.Nbr_Vendor_Libraries));
      if Errorout.Nbr_Errors > 0 then
         return -1;
      end if;

      if First_Arg <= Args'Last then
         if not Synth_Load_Files (Args (First_Arg .. Args'Last)) then
            return -1;
         end if;
      end if;

      File := Get_Design_File_Chain (Libraries.Work_Library);
      while File /= Null_Iir loop
         Design := Get_First_Design_Unit (File);
         while Design /= Null_Iir loop
            Unit := Get_Library_Unit (Design);
            if Get_Kind (Unit) = Iir_Kind_Entity_Declaration then
               Report_Msg (Msgid_Note, Option, No_Source_Coord,
                 "import %n", (1 => +Unit));

               Cb.all (Get_Identifier (Unit), Unit, Cb_Arg);

            end if;
            Design := Get_Chain (Design);
         end loop;
         File := Get_Chain (File);
      end loop;

      return 0;
   end Ghdl_Synth_Read;

   function Convert_Pval_To_Val (V : Pval; Typ : Type_Acc) return Valtyp
   is
      use Types_Utils;
      W : constant Uns32 := Get_Pval_Length (V);
      W32 : constant Uns32 := (W + 31) / 32;
      Res : Valtyp;
   begin
      case Typ.Kind is
         when Type_Discrete =>
            declare
               Val : Uns64;
               L : Logic_32;
               Msb : Uns32;
            begin
               --  Check no Z/X.
               for I in 0 .. W32 - 1 loop
                  L := Read_Pval (V, I);
                  if L.Zx /= 0 then
                     return No_Valtyp;
                  end if;
               end loop;

               L := Read_Pval (V, 0);
               Val := Uns64 (L.Val);
               if W > 32 then
                  L := Read_Pval (V, 1);
                  Val := Val + Shift_Left (Uns64(L.Val), 32);
               end if;
               if W32 > 2 then
                  if Typ.Drange.Is_Signed then
                     Msb := Uns32 (Shift_Right_Arithmetic (Val, 64)
                       and 16#ffff_ffff#);
                  else
                     Msb := 0;
                  end if;
                  for I in 2 .. W32 - 1 loop
                     if Read_Pval (V, I).Val /= Msb then
                        return No_Valtyp;
                     end if;
                  end loop;
               end if;
               Res := Create_Value_Memory (Typ, Expr_Pool'Access);
               Write_Discrete (Res, To_Int64 (Val));
               return Res;
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Convert_Pval_To_Val;

   function Name_To_Idx (Name : String) return Integer
   is
      Res : Integer;
   begin
      Res := 0;
      for I in Name'Range loop
         if Name (I) in '0' .. '9' then
            Res := Res * 10 + Character'Pos (Name (I)) - Character'Pos ('0');
         else
            return -1;
         end if;
      end loop;
      return Res;
   end Name_To_Idx;

   function Ghdl_Synth_With_Params (Entity_Decl : Node;
                                    Params : Pval_Cstring_Array_Acc;
                                    Nparams : Natural)
                                   return Synth_Instance_Acc
   is
      use Vhdl.Configuration;
      use Vhdl.Errors;
      use Errorout;
      use Elab.Vhdl_Insts;
      use Elab.Vhdl_Types;
      use Synth.Vhdl_Expr;
      use Synth.Flags;
      type Integer_Array is array (Natural range <>) of Integer;
      type Boolean_Array is array (Natural range <>) of Boolean;
      Conf : Node;
      Entity : Node;
      Arch : Node;
      Top_Inst : Synth_Instance_Acc;
      Inter : Node;
      Inter_Pos : Natural;
      --  Name for each PARAMS (or Null_Identifier if not a name).
      Names : Name_Id_Array (0 .. Nparams - 1);
      --  Index of each PARAMS (or -1 if a name).
      Idxes : Integer_Array (0 .. Nparams - 1);
      --  Parameters used.
      Used : Boolean_Array (0 .. Nparams - 1);
      Res : Synth_Instance_Acc;
   begin
      Idxes := (others => -1);
      Used := (others => False);

      for I in 1 .. Nparams loop
         declare
            use Outputs;
            use Netlists.Disp_Common;
            Cname : constant Ghdl_C_String := Params (I - 1).Str;
            Name_Len : constant Natural := strlen (Cname);
            Name : String (1 .. Name_Len);
            Idx : Integer;
            Err: Boolean;
         begin
            Name := Cname (1 .. Name_Len);

            if Boolean'(False) then
               Wr (Name);
               Wr (": ");
               Disp_Pval_Binary_Digits (Params (I - 1).Val);
               Wr_Line;
            end if;

            Idx := Name_To_Idx (Name);
            if Idx > 0 then
               Idxes (Idx - 1) := I - 1;
               Names (I - 1) := Null_Identifier;
            else
               Vhdl.Chars.Convert_Identifier (Name, Err);
               Names (I - 1) := Name_Table.Get_Identifier (Name);
               if Err then
                  Error_Msg_Elab
                    ("parameter %i is not a valid vhdl name",
                    (1 => +Names (I - 1)));
                  return null;
               end if;
            end if;
         end;
      end loop;

      --  TODO:
      --  1. call a modified configure to analyze files
      Conf := Configure_From_Entity
        (Get_Design_Unit (Entity_Decl), Null_Identifier);

      --  2. Create synth_instance
      Elab_Top_Create (Get_Library_Unit (Conf), Entity, Arch, Top_Inst);

      --  3. Set generics.
      Inter := Get_Generic_Chain (Entity);
      Inter_Pos := 0;
      while Is_Valid (Inter) loop
         declare
            Inter_Id : constant Name_Id := Get_Identifier (Inter);
            Em : Mark_Type;
            Val : Valtyp;
            Inter_Typ : Type_Acc;
            Defval : Node;
            Over_Pos : Integer;
         begin
            Mark_Expr_Pool (Em);
            Inter_Typ := Elab_Declaration_Type (Top_Inst, Inter);

            Over_Pos := -1;
            for I in Names'Range loop
               if Names (I) = Inter_Id then
                  Over_Pos := I;
                  exit;
               end if;
            end loop;
            if Idxes (Inter_Pos) /= -1 then
               if Over_Pos /= -1 then
                  Error_Msg_Elab
                    ("parameter %i is both set by name and by position",
                    (1 => +Inter_Id));
                  return null;
               end if;
               Over_Pos := Idxes (Inter_Pos);
            end if;
            if Over_Pos >= 0 then
               Used (Over_Pos) := True;
               Val := Convert_Pval_To_Val (Params (Over_Pos).Val, Inter_Typ);
            else
               Defval := Get_Default_Value (Inter);
               if Defval /= Null_Node then
                  Val := Synth_Expression_With_Type
                    (Top_Inst, Defval, Inter_Typ);
               else
                  --  Only for simulation, expect override.
                  Val := Create_Value_Default (Inter_Typ);
               end if;
            end if;
            if Val /= No_Valtyp then
               pragma Assert (Is_Static (Val.Val));
               Val := Unshare (Val, Instance_Pool);
               Val.Typ := Unshare_Type_Instance (Val.Typ, Inter_Typ);
               Create_Object (Top_Inst, Inter, Val);
            end if;
            Release_Expr_Pool (Em);
         end;
         Inter := Get_Chain (Inter);
         Inter_Pos := Inter_Pos + 1;
      end loop;

      pragma Assert (Is_Expr_Pool_Empty);

      for I in Used'Range loop
         if not Used (I) then
            if Names (I) = Null_Identifier then
               Error_Msg_Elab ("positional parameter %v not used",
                               +Uns32 (Idxes (I) + 1));
            else
               Error_Msg_Elab ("parameter %i not present", (1 => +Names (I)));
            end if;
            return null;
         end if;
      end loop;

      --  4. Elab ports
      Elab_Top_Ports (Entity, Top_Inst);

      --  5. Get existing index.
      Res := Synth.Vhdl_Insts.Synth_Top_Entity (Conf, Name_Hash, Top_Inst);

      if Res = Top_Inst then
         --  New instance.
         --  6. Elaborate it
         Elab_Top_Finish (Get_Library_Unit (Conf), Entity, Arch, Res);

         --  FIXME: RES is before its dependencies
         Synth.Vhdl_Insts.Synth_All_Instances;
      else
         --  Already synthesized.
         --  FIXME: free TOP_INST.
         null;
      end if;

      return Res;
   end Ghdl_Synth_With_Params;
end Libghdl_Synth;
