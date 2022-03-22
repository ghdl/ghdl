--  GHDL Run Time (GRT) - FST generator.
--  Copyright (C) 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.

-------------------------------------------------------------------------------

-- TODO:
-- * Fix the following issues :
--    + Currently both the top level signals and signals in packages aren't
--      visible on the tree view (SST) of gtkwave, but both of them are visible
--      when no item is selected in the tree view and are mixed together.
--      (Same issue with VCD waves.)
--    + After calling FST_Put_Hierarchy (Pack, Match_List), Avhpi_Error is
--      raised several times when no signal paths are provided in a wave option
--      file. It has no consequences other than a printed message.
--      (Same issue with VCD waves.)
--    + Integer signals aren't displayed correctly but only their lowest bit is
--      shown.

with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with Interfaces.C;
with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Fst_Api; use Grt.Fst_Api;
with Grt.Vcd; use Grt.Vcd;
with Grt.Avhpi; use Grt.Avhpi;
with Grt.Errors; use Grt.Errors;
with Grt.Signals; use Grt.Signals;
with Grt.Table;
with Grt.Astdio; use Grt.Astdio;
with Grt.Hooks; use Grt.Hooks;
with Grt.Rtis; use Grt.Rtis;
with Grt.Rtis_Types; use Grt.Rtis_Types;
with Grt.To_Strings;
with Grt.Wave_Opt; use Grt.Wave_Opt;
with Grt.Wave_Opt.Design; use Grt.Wave_Opt.Design;
with Grt.Options;
pragma Elaborate_All (Grt.Table);

package body Grt.Fst is
   --  FST format has a mechanism to declare signal aliases (if two signals
   --  in the hierarchy are the same).  Enabling this reduce the number of
   --  signals dumped, but weirdly it makes the FST file slightly bigger.
   Flag_Aliases : constant Boolean := False;

   --  Global FST context.  Set to non-NULL iff dumping signals to an FST file.
   Context : fstContext := Null_fstContext;

   --  Index type of the table of vcd variables to dump.
   type Fst_Index_Type is new Integer;

   --  Return TRUE if OPT is an option for FST.
   function Fst_Option (Opt : String) return Boolean
   is
      F : constant Natural := Opt'First;
      Fst_Filename : String_Access;
   begin
      if Opt'Length < 6 or else Opt (F .. F + 5) /= "--fst=" then
         return False;
      end if;
      if Context /= Null_fstContext then
         Error ("--fst: file already set");
         return True;
      end if;

      --  Add an extra NUL character.
      Fst_Filename := new String (1 .. Opt'Length - 6 + 1);
      Fst_Filename (1 .. Opt'Length - 6) := Opt (F + 6 .. Opt'Last);
      Fst_Filename (Fst_Filename'Last) := NUL;

      Context := fstWriterCreate
        (To_Ghdl_C_String (Fst_Filename.all'Address), 1);
      if Context = Null_fstContext then
         Error_S ("fst: cannot open ");
         Error_E (Fst_Filename (Fst_Filename'First .. Fst_Filename'Last - 1));
      end if;
      return True;
   end Fst_Option;

   procedure Fst_Help is
   begin
      Put_Line (" --fst=FILENAME     dump signal values into an FST file");
   end Fst_Help;

   --  Called before elaboration.
   procedure Fst_Init
   is
      Version : constant String := "GHDL FST v0" & NUL;
   begin
      if Context = Null_fstContext then
         return;
      end if;

      fstWriterSetFileType (Context, FST_FT_VHDL);
      fstWriterSetPackType (Context, FST_WR_PT_LZ4);
      fstWriterSetTimescale (Context, -3 * Options.Time_Resolution_Scale);
      fstWriterSetVersion (Context, To_Ghdl_C_String (Version'Address));
      fstWriterSetRepackOnClose (Context, 1);
      fstWriterSetParallelMode (Context, 0);
   end Fst_Init;

   type Fst_Sig_Info is record
      Wire : Verilog_Wire_Info;
      Hand : fstHandle;
   end record;

   package Fst_Table is new Grt.Table
     (Table_Component_Type => Fst_Sig_Info,
      Table_Index_Type => Fst_Index_Type,
      Table_Low_Bound => 0,
      Table_Initial => 32);

   procedure Avhpi_Error (Err : AvhpiErrorT)
   is
      pragma Unreferenced (Err);
   begin
      Put_Line ("Fst.Avhpi_Error!");
   end Avhpi_Error;

   function Equal (Left, Right : Verilog_Wire_Info) return Boolean
   is
      Len : Ghdl_Index_Type;
   begin
      if Left.Vtype /= Right.Vtype
        or else Left.Val /= Right.Val
      then
         return False;
      end if;

      --  Get length.
      Len := Get_Wire_Length (Left);
      if Len /= Get_Wire_Length (Right) then
         return False;
      end if;

      --  Compare signals.
      for I in 1 .. Len loop
         if To_Signal_Arr_Ptr (Left.Ptr)(I - 1)
           /= To_Signal_Arr_Ptr (Right.Ptr)(I - 1)
         then
            return False;
         end if;
      end loop;
      return True;
   end Equal;

   function Hash (El : Verilog_Wire_Info) return Ghdl_Index_Type
   is
      Len : constant Ghdl_Index_Type := Get_Wire_Length (El);
      Res : Ghdl_Index_Type;
      Iaddr : Integer_Address;
   begin
      Res := Vcd_Var_Type'Pos (El.Vtype) * 2 + Vcd_Value_Kind'Pos (El.Val);
      Res := Res + Len * 29;
      for I in 1 .. Len loop
         Iaddr := To_Integer (To_Signal_Arr_Ptr (El.Ptr)(I - 1).all'Address);
         Res := Res +
           Ghdl_Index_Type (Iaddr mod Integer_Address (Ghdl_Index_Type'Last));
      end loop;
      return Res;
   end Hash;

   --  Very simple hash table to detect aliases.
   type Bucket_Type;
   type Bucket_Acc is access Bucket_Type;

   type Bucket_Type is record
      El : Fst_Index_Type;
      Next : Bucket_Acc;
   end record;

   type Hash_Table is array (Ghdl_Index_Type range <>) of Bucket_Acc;
   type Hash_Table_Acc is access Hash_Table;

   Hash_Tab : Hash_Table_Acc;

   procedure Free_Hash_Tab
   is
      procedure Free_Hash_Table is new
        Ada.Unchecked_Deallocation (Hash_Table, Hash_Table_Acc);
      procedure Free_Bucket_Type is new
        Ada.Unchecked_Deallocation (Bucket_Type, Bucket_Acc);
      Ent, Nent : Bucket_Acc;
   begin
      for I in Hash_Tab'Range loop
         Ent := Hash_Tab (I);
         while Ent /= null loop
            Nent := Ent.Next;
            Free_Bucket_Type (Ent);
            Ent := Nent;
         end loop;
      end loop;
      Free_Hash_Table (Hash_Tab);
   end Free_Hash_Tab;

   procedure Fst_Add_Signal (Sig : VhpiHandleT)
   is
      Sig_Typemark, Sig_Subtype, Sig_Base_Type : VhpiHandleT;
      Err : AvhpiErrorT;
      Vcd_El : Verilog_Wire_Info;
      Vt : fstVarType;
      Sdt : fstSupplementalDataType;
      Dir : fstVarDir;
      Len : Interfaces.C.unsigned;
      Name : String (1 .. 128);
      Name_Len : Natural;
      Type_Name : String (1 .. 32);
      Type_Name_Len : Natural;
      Type_C_Name : Ghdl_C_String;
      Hand : fstHandle;
      Alias : fstHandle;
      H : Ghdl_Index_Type;
   begin
      Get_Verilog_Wire (Sig, Vcd_El);

      case Vcd_El.Vtype is
         when Vcd_Bad
            | Vcd_Array
            | Vcd_Struct =>
            --  Not handled.
            return;
         when Vcd_Enum8 =>
            Vt := FST_VT_GEN_STRING;
            Len := 1;
            Sdt := FST_SDT_NONE;
         when Vcd_Bool =>
            Vt := FST_VT_VCD_REG;
            Len := 1;
            Sdt := FST_SDT_VHDL_BOOLEAN;
         when Vcd_Integer32 =>
            Vt := FST_VT_VCD_INTEGER;
            Len := 1;
            Sdt := FST_SDT_VHDL_INTEGER;
         when Vcd_Float64 =>
            Vt := FST_VT_VCD_REAL;
            Len := 1;
            Sdt := FST_SDT_VHDL_REAL;
         when Vcd_Bit =>
            Vt := FST_VT_VCD_REG;
            Len := 1;
            Sdt := FST_SDT_VHDL_BIT;
         when Vcd_Stdlogic =>
            Vt := FST_VT_VCD_REG;
            Len := 1;
            Sdt := FST_SDT_VHDL_STD_LOGIC;
         when Vcd_Bitvector =>
            Vt := FST_VT_VCD_REG;
            Len := Interfaces.C.unsigned (Vcd_El.Vec_Range.I32.Len);
            Sdt := FST_SDT_VHDL_BIT_VECTOR;
         when Vcd_Stdlogic_Vector =>
            Vt := FST_VT_VCD_REG;
            Len := Interfaces.C.unsigned (Vcd_El.Vec_Range.I32.Len);
            Sdt := FST_SDT_VHDL_STD_LOGIC_VECTOR;
      end case;

      if Vhpi_Get_Kind (Sig) = VhpiPortDeclK then
         case Vhpi_Get_Mode (Sig) is
            when VhpiInMode =>
               Dir := FST_VD_INPUT;
            when VhpiInoutMode =>
               Dir := FST_VD_INOUT;
            when VhpiBufferMode =>
               Dir := FST_VD_BUFFER;
            when VhpiLinkageMode =>
               Dir := FST_VD_LINKAGE;
            when VhpiOutMode =>
               Dir := FST_VD_OUTPUT;
            when VhpiErrorMode =>
               Dir := FST_VD_IMPLICIT;
         end case;
      else
         Dir := FST_VD_IMPLICIT;
      end if;

      --  Try to find an alias.
      Alias := Null_fstHandle;
      if Flag_Aliases then
         declare
            Ent : Bucket_Acc;
         begin
            H := Hash (Vcd_El) mod (Hash_Tab'Last + 1);
            Ent := Hash_Tab (H);
            while Ent /= null loop
               if Equal (Fst_Table.Table (Ent.El).Wire, Vcd_El) then
                  Alias := Fst_Table.Table (Ent.El).Hand;
                  exit;
               else
                  Ent := Ent.Next;
               end if;
            end loop;
         end;
      end if;

      --  Source (for instances ?)
      if Boolean'(False) then
         declare
            Filename : Ghdl_C_String;
            Line : VhpiIntT;
         begin
            Vhpi_Get_Str (VhpiFileNameP, Sig, Filename);
            Vhpi_Get (VhpiLineNoP, Sig, Line, Err);
            if Filename /= null and then Err = AvhpiErrorOk then
               fstWriterSetSourceStem
                 (Context, Filename, Interfaces.C.unsigned (Line), 0);
            end if;
         end;
      end if;

      --  Extract type name.
      Vhpi_Handle (VhpiSubtype, Sig, Sig_Subtype, Err);
      if Err /= AvhpiErrorOk then
         Avhpi_Error (Err);
      end if;
      Vhpi_Handle (VhpiTypeMark, Sig_Subtype, Sig_Typemark, Err);
      if Err /= AvhpiErrorOk then
         Avhpi_Error (Err);
      end if;
      Vhpi_Get_Str (VhpiNameP, Sig_Typemark, Type_Name, Type_Name_Len);
      if Type_Name_Len = 0 then
         --  Try with the base type.
         Vhpi_Handle (VhpiBaseType, Sig_Subtype, Sig_Base_Type, Err);
         if Err /= AvhpiErrorOk then
            Avhpi_Error (Err);
         end if;
         Vhpi_Get_Str (VhpiNameP, Sig_Base_Type, Type_Name, Type_Name_Len);
      end if;
      if Type_Name_Len = 0 then
         Type_C_Name := null;
      else
         if Type_Name_Len >= Type_Name'Last then
            --  Truncate name.
            Type_Name_Len := Type_Name'Last - 1;
         end if;
         Type_Name (Type_Name_Len + 1) := NUL;
         Type_C_Name := To_Ghdl_C_String (Type_Name'Address);
      end if;

      --  Extract name (avoid truncation, append verilog range for arrays).
      Vhpi_Get_Str (VhpiNameP, Sig, Name, Name_Len);
      if Name_Len >= Name'Length
        or else Vcd_El.Vtype in Vcd_Var_Vectors
      then
         declare
            Name2 : String (1 .. Name_Len + 3 + 2 * 11 + 1);

            procedure Append (N : Ghdl_I32)
            is
               Num : String (1 .. 11);
               Num_First : Natural;
               Num_Len : Natural;
            begin
               Grt.To_Strings.To_String (Num, Num_First, N);
               Num_Len := Num'Last - Num_First + 1;
               Name2 (Name_Len + 1 .. Name_Len + Num_Len) :=
                 Num (Num_First .. Num'Last);
               Name_Len := Name_Len + Num_Len;
            end Append;
         begin
            Vhpi_Get_Str (VhpiNameP, Sig, Name2, Name_Len);
            if Vcd_El.Vec_Range /= null then
               Name2 (Name_Len + 1) := '[';
               Name_Len := Name_Len + 1;
               Append (Vcd_El.Vec_Range.I32.Left);
               Name2 (Name_Len + 1) := ':';
               Name_Len := Name_Len + 1;
               Append (Vcd_El.Vec_Range.I32.Right);
               Name2 (Name_Len + 1) := ']';
               Name_Len := Name_Len + 1;
            end if;
            Name2 (Name_Len + 1) := NUL;
            Name_Len := Name_Len + 1;

            Hand := fstWriterCreateVar2
              (Context, Vt, Dir, Len, To_Ghdl_C_String (Name2'Address), Alias,
               Type_C_Name, FST_SVT_VHDL_SIGNAL, Sdt);
         end;
      else
         Name (Name_Len + 1) := NUL;
         Hand := fstWriterCreateVar2
           (Context, Vt, Dir, Len, To_Ghdl_C_String (Name'Address),
            Alias, Type_C_Name, FST_SVT_VHDL_SIGNAL, Sdt);
      end if;

      --  Do not put aliases in the table.
      if Flag_Aliases and then Interfaces.C."/=" (Alias, Null_fstHandle) then
         return;
      end if;

      Fst_Table.Append (Fst_Sig_Info'(Wire => Vcd_El, Hand => Hand));

      if Flag_Aliases then
         Hash_Tab (H) := new Bucket_Type'(El => Fst_Table.Last,
                                          Next => Hash_Tab (H));
      end if;
   end Fst_Add_Signal;

   procedure Fst_Put_Hierarchy
     (Inst : VhpiHandleT; Match_List : Design.Match_List);

   procedure Fst_Put_Scope
     (Scope : fstScopeType; Decl : VhpiHandleT; Match_List : Design.Match_List)
   is
      Name : String (1 .. 128);
      Name_Len : Integer;
      Err : AvhpiErrorT;
   begin
      --  Source file and line.
      declare
         Filename : Ghdl_C_String;
         Line : VhpiIntT;
         Arch : VhpiHandleT;
      begin
         Vhpi_Get_Str (VhpiFileNameP, Decl, Filename);
         Vhpi_Get (VhpiLineNoP, Decl, Line, Err);
         if Filename /= null and then Err = AvhpiErrorOk then
            if Vhpi_Get_Kind (Decl) /= VhpiCompInstStmtK then
               --  For a block, a generate block: source location.
               fstWriterSetSourceStem
                 (Context, Filename, Interfaces.C.unsigned (Line), 0);
            else
               --  For a component instantiation: instance location
               fstWriterSetSourceInstantiationStem
                 (Context, Filename, Interfaces.C.unsigned (Line), 0);
               --  Request DesignUnit => arch
               Vhpi_Handle (VhpiDesignUnit, Decl, Arch, Err);
               if Err /= AvhpiErrorOk then
                  Avhpi_Error (Err);
               elsif Arch /= Null_Handle then
                  --  Request filename and line.
                  Vhpi_Get_Str (VhpiFileNameP, Arch, Filename);
                  Vhpi_Get (VhpiLineNoP, Arch, Line, Err);
                  if Filename /= null and then Err = AvhpiErrorOk then
                     --  And source location.
                     fstWriterSetSourceStem
                       (Context, Filename, Interfaces.C.unsigned (Line), 0);
                  end if;
               end if;
            end if;
         end if;
      end;

      Vhpi_Get_Str (VhpiNameP, Decl, Name, Name_Len);
      if Name_Len < Name'Last then
         Name (Name_Len + 1) := NUL;
      else
         --  Truncate
         Name (Name'Last) := NUL;
      end if;

      fstWriterSetScope
        (Context, Scope, To_Ghdl_C_String (Name'Address), null);
      Fst_Put_Hierarchy (Decl, Match_List);
      fstWriterSetUpscope (Context);
   end Fst_Put_Scope;

   procedure Fst_Put_Hierarchy
     (Inst : VhpiHandleT; Match_List : Design.Match_List)
   is
      Decl_It : VhpiHandleT;
      Decl : VhpiHandleT;
      Error : AvhpiErrorT;
      Match_List_Child : Design.Match_List;
   begin
      Vhpi_Iterator (VhpiDecls, Inst, Decl_It, Error);
      if Error /= AvhpiErrorOk then
         Avhpi_Error (Error);
         return;
      end if;

      --  Extract signals.
      loop
         Vhpi_Scan (Decl_It, Decl, Error);
         exit when Error = AvhpiErrorIteratorEnd;
         if Error /= AvhpiErrorOk then
            Avhpi_Error (Error);
            return;
         end if;

         case Vhpi_Get_Kind (Decl) is
            when VhpiPortDeclK
              | VhpiSigDeclK =>
               Match_List_Child := Get_Cursor
                 (Match_List, Avhpi_Get_Base_Name (Decl), Is_Signal => True);
               if Is_Displayed (Match_List_Child) then
                  Fst_Add_Signal (Decl);
               end if;
            when others =>
               null;
         end case;
      end loop;

      --  Extract sub-scopes.
      if Vhpi_Get_Kind (Inst) = VhpiPackInstK then
         --  Except for packages
         return;
      end if;

      Vhpi_Iterator (VhpiInternalRegions, Inst, Decl_It, Error);
      if Error /= AvhpiErrorOk then
         Avhpi_Error (Error);
         return;
      end if;

      loop
         Vhpi_Scan (Decl_It, Decl, Error);
         exit when Error = AvhpiErrorIteratorEnd;
         if Error /= AvhpiErrorOk then
            Avhpi_Error (Error);
            return;
         end if;

         Match_List_Child := Get_Cursor
           (Match_List, Avhpi_Get_Base_Name (Decl));
         if Is_Displayed (Match_List_Child) then
            case Vhpi_Get_Kind (Decl) is
               when VhpiIfGenerateK =>
                  Fst_Put_Scope
                    (FST_ST_VHDL_IF_GENERATE, Decl, Match_List_Child);
               when VhpiForGenerateK =>
                  Fst_Put_Scope
                    (FST_ST_VHDL_FOR_GENERATE, Decl, Match_List_Child);
               when  VhpiBlockStmtK =>
                  Fst_Put_Scope (FST_ST_VHDL_BLOCK, Decl, Match_List_Child);
               when VhpiCompInstStmtK =>
                  Fst_Put_Scope
                    (FST_ST_VHDL_ARCHITECTURE, Decl, Match_List_Child);
               when others =>
                  null;
            end case;
         end if;
      end loop;
   end Fst_Put_Hierarchy;

   procedure Fst_Put_Integer32 (Hand : fstHandle; V : Ghdl_U32)
   is
      Str : String (1 .. 32);
      Val : Ghdl_U32;
   begin
      Val := V;
      for I in Str'Range loop
         Str (I) := Character'Val (Character'Pos ('0') + (Val and 1));
         Val := Val / 2;
      end loop;
      fstWriterEmitValueChange (Context, Hand, Str'Address);
   end Fst_Put_Integer32;

   procedure Fst_Put_Enum8
     (Hand : fstHandle; V : Ghdl_E8; Rti : Ghdl_Rti_Access)
   is
      Enum_Rti : constant Ghdl_Rtin_Type_Enum_Acc :=
        To_Ghdl_Rtin_Type_Enum_Acc (Rti);
      Str : constant Ghdl_C_String := Enum_Rti.Names (Ghdl_Index_Type (V));
   begin
      fstWriterEmitVariableLengthValueChange
        (Context, Hand, To_Address (Str),
         Interfaces.C.unsigned (strlen (Str)));
   end Fst_Put_Enum8;

   procedure Fst_Put_Var (I : Fst_Index_Type)
   is
      From_Bit : constant array (Ghdl_B1) of Character := "01";
      type Map_Type is array (Ghdl_E8 range 0 .. 8) of Character;
      From_Std : constant Map_Type := "UX01ZWLH-";
      V : Fst_Sig_Info renames Fst_Table.Table (I);
      Len : constant Ghdl_Index_Type := Get_Wire_Length (V.Wire);
      Hand : constant fstHandle := V.Hand;
   begin
      case V.Wire.Vtype is
         when Vcd_Bit
           | Vcd_Bool
           | Vcd_Bitvector =>
            declare
               Str : Std_String_Uncons (0 .. Len - 1);
            begin
               for I in Str'Range loop
                  Str (I) := From_Bit (Verilog_Wire_Val (V.Wire, I).B1);
               end loop;
               fstWriterEmitValueChange (Context, Hand, Str'Address);
            end;
         when Vcd_Stdlogic
           | Vcd_Stdlogic_Vector =>
            declare
               Str : Std_String_Uncons (0 .. Len - 1);
            begin
               for I in Str'Range loop
                  Str (I) := From_Std (Verilog_Wire_Val (V.Wire, I).E8);
               end loop;
               fstWriterEmitValueChange (Context, Hand, Str'Address);
            end;
         when Vcd_Integer32 =>
            Fst_Put_Integer32 (Hand, Verilog_Wire_Val (V.Wire).E32);
         when Vcd_Float64 =>
            null;
         when Vcd_Enum8 =>
            Fst_Put_Enum8 (Hand, Verilog_Wire_Val (V.Wire).E8, V.Wire.Rti);
         when Vcd_Bad
           | Vcd_Array
           | Vcd_Struct =>
            null;
      end case;
   end Fst_Put_Var;

   procedure Fst_Cycle;

   --  Called after elaboration.
   procedure Fst_Start
   is
      Pack_It : VhpiHandleT;
      Pack : VhpiHandleT;
      Error : AvhpiErrorT;
      Root : VhpiHandleT;
      Match_List : Design.Match_List;
   begin
      --  Do nothing if there is no VCD file to generate.
      if Context = Null_fstContext then
         return;
      end if;

      --  Be sure the RTI of std_ulogic is set.
      Search_Types_RTI;

      if Flag_Aliases then
         Hash_Tab :=
           new Hash_Table (0 .. Ghdl_Index_Type (Sig_Table.Last / 17));
      end if;

      --  Put hierarchy.

      --  First packages.
      Get_Package_Inst (Pack_It);
      loop
         Vhpi_Scan (Pack_It, Pack, Error);
         exit when Error = AvhpiErrorIteratorEnd;
         if Error /= AvhpiErrorOk then
            Avhpi_Error (Error);
            return;
         end if;
         Match_List := Get_Top_Cursor (Pkg, Avhpi_Get_Base_Name (Pack));
         if Is_Displayed (Match_List) then
            Fst_Put_Hierarchy (Pack, Match_List);
         end if;
      end loop;

      --  Then top entity.
      Get_Root_Inst (Root);
      Match_List := Get_Top_Cursor (Entity, Avhpi_Get_Base_Name (Root));
      if Is_Displayed (Match_List) then
         Fst_Put_Hierarchy (Root, Match_List);
      end if;
      Wave_Opt.Design.Last_Checks;

      if Flag_Aliases then
         Free_Hash_Tab;
      end if;

      Register_Cycle_Hook (Fst_Cycle'Access);
   end Fst_Start;

   --  Called before each non delta cycle.
   procedure Fst_Cycle is
   begin
      --  Disp values.
      fstWriterEmitTimeChange (Context, Unsigned_64 (Current_Time));

      if Current_Time = 0 then
         --  Disp all values.
         for I in Fst_Table.First .. Fst_Table.Last loop
            Fst_Put_Var (I);
         end loop;
      else
         --  Disp only values changed.
         for I in Fst_Table.First .. Fst_Table.Last loop
            if Verilog_Wire_Changed (Fst_Table.Table (I).Wire, Current_Time)
            then
               Fst_Put_Var (I);
            end if;
         end loop;
      end if;
   end Fst_Cycle;

   --  Called at the end of the simulation.
   procedure Fst_End is
   begin
      if Context /= Null_fstContext then
         fstWriterClose (Context);
         Context := Null_fstContext;
      end if;
   end Fst_End;

   Fst_Hooks : aliased constant Hooks_Type :=
     (Desc => new String'("fst: dump waveform in fst file format"),
      Option => Fst_Option'Access,
      Help => Fst_Help'Access,
      Init => Fst_Init'Access,
      Start => Fst_Start'Access,
      Finish => Fst_End'Access);

   procedure Register is
   begin
      Register_Hooks (Fst_Hooks'Access);
   end Register;
end Grt.Fst;
