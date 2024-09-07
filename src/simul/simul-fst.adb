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
--  wave_opt
--  time signal
--  stem file (for rtlbrowser)
--  Keep case
--  VCD format
--  enum: labels + reduce nbr of bits
--  generics
--  + asserts (attached to hierarchy / process) ?
--  + psl

with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;

with Types; use Types;
with Name_Table;
with Files_Map;
with Tables;
with Hash; use Hash;
with Dyn_Maps;

with Vhdl.Nodes; use Vhdl.Nodes;
with Vhdl.Utils;
with Vhdl.Std_Package;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Elab.Vhdl_Values; use Elab.Vhdl_Values;
with Elab.Vhdl_Context; use Elab.Vhdl_Context;

with Simul.Vhdl_Elab; use Simul.Vhdl_Elab;
with Simul.Vhdl_Simul;

with Grt.Types; use Grt.Types;
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.C; use Grt.C;
with Grt.Fst_Api; use Grt.Fst_Api;
with Grt.Errors;
with Grt.Signals; use Grt.Signals;
with Grt.Astdio; use Grt.Astdio;
with Grt.Hooks; use Grt.Hooks;
with Grt.To_Strings;
with Grt.Vstrings; use Grt.Vstrings;
-- with Grt.Wave_Opt; use Grt.Wave_Opt;
-- with Grt.Wave_Opt.Design; use Grt.Wave_Opt.Design;
with Grt.Options;
pragma Elaborate_All (Tables);

package body Simul.Fst is
   --  Global FST context.  Set to non-NULL iff dumping signals to an FST file.
   Context : fstContext := Null_fstContext;

   Num32_Len : constant Natural := 11;  -- 10 digits + sign.

   --  FST file name
   Fst_Filename : String_Access;

   --  If true, also add extra internal blocks for components.
   Flag_Components : constant Boolean := False;

   --  If set, use a fake date to make the output deterministic.
   Flag_Nodate : Boolean := False;

   procedure Fst_Cycle;

   procedure Free is new Ada.Unchecked_Deallocation
     (String, String_Access);

   --  Note: the STR is not null terminated!
   procedure Get_Id_From_Source (Name : Node;
                                 Str : out Ghdl_C_String;
                                 Len : out Natural)
   is
      use Name_Table;
      Id : constant Name_Id := Get_Identifier (Name);
      Loc : constant Location_Type := Get_Location (Name);
      pragma Assert (Id /= Null_Identifier);
      pragma Assert (not Is_Character (Id));
   begin
      Len := Get_Name_Length (Id);

      if Loc /= No_Location
        and then Loc /= Vhdl.Std_Package.Std_Location
      then
         declare
            use Files_Map;
            File : Source_File_Entry;
            Pos : Source_Ptr;
            Buf : File_Buffer_Acc;
         begin
            Location_To_File_Pos (Loc, File, Pos);
            Buf := Get_File_Source (File);
            Str := To_Ghdl_C_String (Buf (Pos)'Address);
         end;
      else
         Str := To_Ghdl_C_String (Name_Table.Get_Address (Id));
      end if;
   end Get_Id_From_Source;

   function Get_C_String (Id : Name_Id) return Ghdl_C_String is
   begin
      return To_Ghdl_C_String (Name_Table.Get_Address (Id));
   end Get_C_String;

   --  Map between enumeration type and fstEnumHandle to put literal names
   --  in FST.

   function Hash_Node (V : Node) return Hash_Value_Type is
   begin
      return Hash_Value_Type (V);
   end Hash_Node;

   function Build (N : Node) return Node is
   begin
      return N;
   end Build;

   function Build_Value (N : Node) return fstEnumHandle
   is
      type C_String_Array is array (Natural range <>) of Ghdl_C_String;
      type C_String_Array_Acc is access C_String_Array;

      procedure Free is new Ada.Unchecked_Deallocation
        (C_String_Array, C_String_Array_Acc);

      type String_Acc_Array is array (Natural range <>) of String_Access;
      type String_Acc_Array_Acc is access String_Acc_Array;

      procedure Free is new Ada.Unchecked_Deallocation
        (String_Acc_Array, String_Acc_Array_Acc);

      Lits : constant Iir_Flist := Get_Enumeration_Literal_List (N);
      Last_Lit : constant Natural := Flist_Last (Lits);
      Nbits : Natural;

      Vals : String_Acc_Array_Acc;
      Names : C_String_Array_Acc;
      Val_Addr : C_String_Array_Acc;
      Res : fstEnumHandle;
   begin
      --  If there is a character, forget.
      if Get_Is_Character_Type (N) then
         return 0;
      end if;

      if Get_Scalar_Size (N) = Scalar_8 then
         Nbits := 8;
      else
         Nbits := 32;
      end if;

      --  allocate the array
      Vals := new String_Acc_Array (0 .. Last_Lit);
      Names := new C_String_Array (0 .. Last_Lit);
      Val_Addr := new C_String_Array (0 .. Last_Lit);

      --  allocate and set each value
      for I in 0 .. Last_Lit loop
         declare
            Val : String_Access;
            V : Uns32;
            Id : Name_Id;
         begin
            Val := new String (1 .. Nbits + 1);

            --  Enum value (ASCII binary).
            V := Uns32 (I);
            for K in reverse 1 .. Nbits loop
               Val (K) := Character'Val (Character'Pos ('0') + (V and 1));
               V := V / 2;
            end loop;
            Val (Nbits + 1) := ASCII.NUL;

            --  Enum name.
            Id := Get_Identifier (Get_Nth_Element (Lits, I));
            Vals (I) := Val;
            Val_Addr (I) := To_Ghdl_C_String (Vals (I)(1)'Address);
            Names (I) := Get_C_String (Id);
         end;
      end loop;

      Res := fstWriterCreateEnumTable
        (Context,
         Get_C_String (Get_Identifier (Get_Type_Declarator (N))),
         unsigned (Last_Lit + 1),
         unsigned (Nbits),
         Names (0)'Address,
         Val_Addr (0)'Address);

      --  free
      for I in 0 .. Last_Lit loop
         Free (Vals (I));
      end loop;
      Free (Vals);
      Free (Val_Addr);
      Free (Names);

      return Res;
   end Build_Value;

   package Enum_Maps is new Dyn_Maps
     (Key_Type => Node,
      Object_Type => Node,
      Value_Type => fstEnumHandle,
      Hash => Hash_Node,
      Build => Build,
      Build_Value => Build_Value,
      Equal => "=");

   Enums : Enum_Maps.Instance;

   --  Return TRUE if OPT is an option for FST.
   function Fst_Option (Opt : String) return Boolean
   is
      F : constant Natural := Opt'First;
   begin
      if Opt'Length < 5 or else Opt (F .. F + 4) /= "--fst" then
         return False;
      end if;

      if Opt'Length > 5 and then Opt (F + 5) = '=' then
         if Fst_Filename /= null then
            Free (Fst_Filename);
         end if;

         --  Add an extra NUL character.
         Fst_Filename := new String (1 .. Opt'Length - 6 + 1);
         Fst_Filename (1 .. Opt'Length - 6) := Opt (F + 6 .. Opt'Last);
         Fst_Filename (Fst_Filename'Last) := NUL;
      elsif Opt (F + 5 .. Opt'Last) = "-nodate" then
         Flag_Nodate := True;
      else
         return False;
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
      use Grt.Errors;
      Version : constant String := "GHDL FST v0" & NUL;
      Nodate : constant String :=
        "Thu Jan  1 01:00:00 1970\n" & ASCII.LF & NUL;
   begin
      if Fst_Filename = null then
         return;
      end if;

      Context := fstWriterCreate
        (To_Ghdl_C_String (Fst_Filename.all'Address), 1);
      if Context = Null_fstContext then
         Error_S ("fst: cannot open ");
         Error_E (Fst_Filename (Fst_Filename'First .. Fst_Filename'Last - 1));
      end if;

      fstWriterSetFileType (Context, FST_FT_VHDL);
      fstWriterSetPackType (Context, FST_WR_PT_LZ4);
      fstWriterSetTimescale (Context, -3 * Grt.Options.Time_Resolution_Scale);
      fstWriterSetVersion (Context, To_Ghdl_C_String (Version'Address));
      fstWriterSetRepackOnClose (Context, 1);
      fstWriterSetParallelMode (Context, 0);
      if Flag_Nodate then
         fstWriterSetDate (Context, To_Ghdl_C_String (Nodate'Address));
      end if;
   end Fst_Init;

   type Vcd_Type is (Vcd_None,
                     Vcd_Bit, Vcd_Logic,
                     Vcd_Int, Vcd_Fp,
                     Vcd_Bitvect, Vcd_Logvect);

   subtype Vcd_Vector_Type is Vcd_Type range Vcd_Bitvect .. Vcd_Logvect;

   --  Important data used to dump a signal.
   type Fst_Sig_Info is record
      Typ : Vcd_Type;
      Is_Drv : Boolean;
      Seen : Boolean;
      W : Uns32;  --  Width (for fst).
      --  Sub-aliases
      Alias : Dump_Table_Index;
      Sig : Memory_Ptr;
      Hand : fstHandle;
   end record;

   package Fst_Table is new Tables
     (Table_Component_Type => Fst_Sig_Info,
      Table_Index_Type => Dump_Table_Index,
      Table_Low_Bound => 1,
      Table_Initial => 32);

   Max_Width : Uns32;
   Buffer : Ghdl_C_String;
   Num_Buffer : String (1 .. 12);

   procedure Append (Vstr : in out Vstring; N : Node)
   is
      Str : Ghdl_C_String;
      Len : Natural;
   begin
      Get_Id_From_Source (N, Str, Len);
      for I in 1 .. Len loop
         Append (Vstr, Str (I));
      end loop;
   end Append;

   procedure Append (Vstr : in out Vstring; Val : Int64)
   is
      Buf : String (1 .. 21);
      First : Natural;
   begin
      Grt.To_Strings.To_String (Buf, First, Ghdl_I64 (Val));
      Append (Vstr, Buf (First .. Buf'Last));
   end Append;

   function To_C_String (Decl : Node; Idx : Int32) return Ghdl_C_String is
   begin
      if Decl = Null_Node then
         declare
            First : Natural;
         begin
            Grt.To_Strings.To_String
              (Num_Buffer (1 .. 11), First, Ghdl_I32 (Idx));
            Num_Buffer (12) := NUL;
            return To_Ghdl_C_String (Num_Buffer (First)'Address);
         end;
      else
         return Get_C_String (Get_Identifier (Decl));
      end if;
   end To_C_String;

   function Get_Signal (S : Fst_Sig_Info; Idx : Uns32) return Ghdl_Signal_Ptr
   is
      use Simul.Vhdl_Simul;
   begin
      return Read_Sig (Sig_Index (S.Sig, Idx));
   end Get_Signal;

   function Get_Value (S : Fst_Sig_Info; Idx : Uns32) return Ghdl_Value_Ptr
   is
      Sig : Ghdl_Signal_Ptr;
   begin
      Sig := Get_Signal (S, Idx);
      if S.Is_Drv then
         return Sig.Driving_Value'Access;
      else
         return Sig.Value_Ptr;
      end if;
   end Get_Value;

   From_Bit : constant array (Ghdl_B1) of Character := "01";
   type Map_Type is array (Ghdl_E8 range 0 .. 8) of Character;
   From_Log : constant Map_Type := "UX01ZWLH-";

   procedure Fst_Put_Var_1 (S : Fst_Sig_Info) is
   begin
      case S.Typ is
         when Vcd_Bit =>
            Buffer (1) := From_Bit (Get_Value (S, 0).B1);
         when Vcd_Logic =>
            Buffer (1) := From_Log (Get_Value (S, 0).E8);
         when Vcd_Logvect =>
            for I in 1 .. S.W loop
               Buffer (Positive (I)) := From_Log (Get_Value (S, I - 1).E8);
            end loop;
         when Vcd_Bitvect =>
            for I in 1 .. S.W loop
               Buffer (Positive (I)) := From_Bit (Get_Value (S, I - 1).B1);
            end loop;
         when Vcd_Int =>
            declare
               V : Ghdl_U64;
            begin
               if S.W = 8 then
                  V := Ghdl_U64 (Get_Value (S, 0).E8);
               elsif S.W = 32 then
                  V := Ghdl_U64 (Get_Value (S, 0).E32);
               elsif S.W = 64 then
                  V := To_Ghdl_U64 (Get_Value (S, 0).I64);
               else
                  raise Internal_Error;
               end if;
               for I in reverse 1 .. Natural (S.W) loop
                  Buffer (I) :=
                    Character'Val (Character'Pos ('0') + (V and 1));
                  V := V / 2;
               end loop;
            end;
         when Vcd_Fp =>
            declare
               V : Ghdl_U64;
            begin
               if S.W = 64 then
                  V := To_Ghdl_U64 (Get_Value (S, 0).I64);
               else
                  raise Internal_Error;
               end if;
               fstWriterEmitValueChange (Context, S.Hand, V'Address);
               return;
            end;
         when Vcd_None =>
            return;
      end case;
      fstWriterEmitValueChange (Context, S.Hand, Buffer (1)'Address);
   end Fst_Put_Var_1;

   procedure Fst_Put_Var (S : Fst_Sig_Info)
   is
      Alias : Dump_Table_Index;
   begin
      Fst_Put_Var_1 (S);
      Alias := S.Alias;
      while Alias /= 0 loop
         Fst_Put_Var_1 (Fst_Table.Table (Alias));
         Alias := Fst_Table.Table (Alias).Alias;
      end loop;
   end Fst_Put_Var;

   procedure Get_File_Line (N : Node;
                            Path : out Ghdl_C_String;
                            Line : out unsigned)
   is
      use Files_Map;
      Loc, Nloc : Location_Type;
      Sfe : Source_File_Entry;
   begin
      Loc := Get_Location (N);
      --  Strip instantiation locations.
      loop
         Nloc := Files_Map.Location_Instance_To_Location (Loc);
         exit when Nloc = No_Location;
         Loc := Nloc;
      end loop;
      Sfe := Location_To_File (Loc);
      Path := Get_C_String (Get_File_Name (Sfe));
      Line := unsigned (Location_File_To_Line (Loc, Sfe));
   end Get_File_Line;

   procedure Fst_Put_Stem (Stem : Node; Istem : Node)
   is
      Path : Ghdl_C_String;
      Line : unsigned;
   begin
      Get_File_Line (Stem, Path, Line);
      fstWriterSetSourceStem (Context, Path, Line, 0);
      if Istem /= Null_Node then
         Get_File_Line (Istem, Path, Line);
         fstWriterSetSourceInstantiationStem (Context, Path, Line, 0);
      end if;
   end Fst_Put_Stem;

   procedure Push_Scope (Inst : Synth_Instance_Acc;
                         Last : Synth_Instance_Acc)
   is
      Src : constant Node := Get_Source_Scope (Inst);
      Parent : constant Synth_Instance_Acc := Get_Instance_Parent (Inst);
      Parent_Src : Node;
      St : fstScopeType;
      Name, Comp_Name : Node;
      Stem, Istem : Node;
      Str : Vstring (32);
      Comp_Str : Vstring (32);
   begin
      if Parent /= Last then
         Push_Scope (Parent, Last);
      end if;

      Stem := Src;
      Istem := Null_Node;

      --  Default
      Name := Src;
      Comp_Name := Null_Node;

      case Get_Kind (Src) is
         when Iir_Kind_If_Generate_Statement =>
            St := FST_ST_VHDL_IF_GENERATE;
         when Iir_Kind_For_Generate_Statement =>
            --  Merged with the generate body.
            return;
         when Iir_Kind_Generate_Statement_Body =>
            Parent_Src := Get_Parent (Src);

            case Iir_Kinds_Generate_Statement (Get_Kind (Parent_Src)) is
               when Iir_Kind_For_Generate_Statement =>
                  declare
                     It : constant Node :=
                       Get_Parameter_Specification (Parent_Src);
                     Val : constant Valtyp := Get_Value (Inst, It);
                  begin
                     Append (Str, Parent_Src);
                     Append (Str, '(');
                     Append (Str, Read_Discrete (Val));
                     Append (Str, ')');

                     St := FST_ST_VHDL_FOR_GENERATE;

                     Name := Null_Node;
                  end;
               when Iir_Kind_If_Generate_Statement =>
                  --  Continue with Id.
                  St := FST_ST_VHDL_IF_GENERATE;
                  Name := Parent_Src;
               when Iir_Kind_Case_Generate_Statement =>
                  --  Continue with Id.
                  St := FST_ST_VHDL_GENERATE;
                  Name := Parent_Src;
            end case;
         when Iir_Kind_Case_Generate_Statement =>
            St := FST_ST_VHDL_GENERATE;
         when Iir_Kind_Block_Statement =>
            St := FST_ST_VHDL_BLOCK;
         when Iir_Kind_Component_Instantiation_Statement =>
            St := FST_ST_VHDL_ARCHITECTURE;
         when Iir_Kind_Architecture_Body =>
            St := FST_ST_VHDL_ARCHITECTURE;
            if Flag_Components or else Parent = Root_Instance then
               Name := Vhdl.Utils.Get_Entity (Src);
            else
               --  Extract instance name.
               declare
                  Stmt : constant Node := Get_Statement_Scope (Inst);
               begin
                  pragma Assert
                    (Get_Kind (Stmt)
                       = Iir_Kind_Component_Instantiation_Statement);
                  Istem := Stmt;
                  Name := Stmt;
                  Comp_Name := Vhdl.Utils.Get_Entity (Src);
               end;
            end if;
         when Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration =>
            St := FST_ST_VHDL_PACKAGE;
         when Iir_Kind_Component_Declaration =>
            if not Flag_Components then
               return;
            end if;
            St := FST_ST_VHDL_BLOCK;
         when others =>
            raise Internal_Error;
      end case;

      if Name /= Null_Node then
         Append (Str, Name);
      end if;
      Append (Str, NUL);

      if Comp_Name /= Null_Node then
         Append (Comp_Str, Comp_Name);
      end if;
      Append (Comp_Str, NUL);

      Fst_Put_Stem (Stem, Istem);

      fstWriterSetScope
        (Context, St, Get_C_String (Str), Get_C_String (Comp_Str));

      Free (Str);
   end Push_Scope;

   function Is_Discarded_Scope (Inst : Synth_Instance_Acc) return Boolean
   is
      Kind : constant Iir_Kind := Get_Kind (Get_Source_Scope (Inst));
   begin
      if Kind = Iir_Kind_For_Generate_Statement then
         --  Those are merged with their generate body.
         return True;
      end if;

      if Flag_Components then
         return False;
      end if;
      return Kind = Iir_Kind_Component_Declaration;
   end Is_Discarded_Scope;

   procedure Adjust_Scope (Prev_Depth : in out Natural;
                           Prev_Inst : in out Synth_Instance_Acc;
                           Cur_Inst : Synth_Instance_Acc)
   is
      Cur_Depth : Natural;
      Inst : Synth_Instance_Acc;
   begin
      --  Compute depth of CUR_INST.
      Cur_Depth := 0;
      Inst := Cur_Inst;
      while Inst /= null loop
         Cur_Depth := Cur_Depth + 1;
         Inst := Get_Instance_Parent (Inst);
      end loop;

      --  Do as many UpScope to reach the same depth as the current one.
      while Prev_Depth > Cur_Depth loop
         if not Is_Discarded_Scope (Prev_Inst) then
            fstWriterSetUpscope (Context);
         end if;
         Prev_Depth := Prev_Depth - 1;
         Prev_Inst := Get_Instance_Parent (Prev_Inst);
      end loop;

      --  Find the common scope.
      Inst := Cur_Inst;
      for I in Prev_Depth .. Cur_Depth - 1 loop
         Inst := Get_Instance_Parent (Inst);
      end loop;

      while Inst /= Prev_Inst loop
         if not Is_Discarded_Scope (Prev_Inst) then
            fstWriterSetUpscope (Context);
         end if;
         Inst := Get_Instance_Parent (Inst);
         Prev_Inst := Get_Instance_Parent (Prev_Inst);
      end loop;

      --  And now call SetScope (if needed, the current scope can be inside
      --  the previous one).
      if Cur_Inst /= Prev_Inst then
         Push_Scope (Cur_Inst, Prev_Inst);
      end if;

      Prev_Depth := Cur_Depth;
      Prev_Inst := Cur_Inst;
   end Adjust_Scope;

   procedure Add_Enum (T : Node)
   is
      use Enum_Maps;
      Idx : Index_Type;
      Res : fstEnumHandle;
   begin
      Get_Index (Enums, T, Idx);
      Res := Get_Value (Enums, Idx);
      if Res /= 0 then
         fstWriterEmitEnumTableRef(Context, Res);
      end if;
   end Add_Enum;

   procedure Add_Signal_Scalar (Sig : Memory_Ptr;
                                Sig_Typ : Type_Acc;
                                Decl_Type : Node;
                                Vd : fstVarDir;
                                Is_Drv : Boolean;
                                Decl : Node;
                                Idx : Int32)
   is
      Vt : fstVarType;
      Len : Uns32;
      Nbr_Sig : Uns32;
      Name_Len : Natural;
      Name_Addr : Ghdl_C_String;
      Hand : fstHandle;
      Alias : fstHandle;
      Sub_Alias : Dump_Table_Index;
      Typ : Vcd_Type;
      Info : Fst_Sig_Info;
   begin
      Len := 1;
      Nbr_Sig := 1;

      case Sig_Typ.Kind is
         when Type_Bit =>
            Vt := FST_VT_SV_BIT;
            Typ := Vcd_Bit;
         when Type_Logic =>
            Vt := FST_VT_SV_LOGIC;
            Typ := Vcd_Logic;
         when Type_Vector =>
            if Sig_Typ.Arr_El.Kind = Type_Logic then
               Vt := FST_VT_SV_LOGIC;
               Typ := Vcd_Logvect;
            else
               Vt := FST_VT_SV_BIT;
               Typ := Vcd_Bitvect;
            end if;
            Len := Sig_Typ.Abound.Len;
            Nbr_Sig := Len;

            --  Avoid any issues with null signals.
            if Len = 0 then
               return;
            end if;
         when Type_Discrete =>
            --  Maybe add names for enums.
            declare
               Btype : constant Node := Vhdl.Utils.Get_Base_Type (Decl_Type);
            begin
               if Get_Kind (Btype) = Iir_Kind_Enumeration_Type_Definition then
                  Add_Enum (Btype);
                  Vt := FST_VT_SV_ENUM;
               else
                  Vt := FST_VT_VCD_INTEGER;
               end if;
            end;
            Typ := Vcd_Int;
            Len := Uns32 (Sig_Typ.Sz) * 8;
         when Type_Float =>
            Vt := FST_VT_VCD_REAL;
            Typ := Vcd_Fp;
            Len := Uns32 (Sig_Typ.Sz) * 8;
         when others =>
            raise Internal_Error;
      end case;

      if Decl = Null_Node then
         Name_Len := Num32_Len;
      else
         Get_Id_From_Source (Decl, Name_Addr, Name_Len);
      end if;

      declare
         use Simul.Vhdl_Simul;
         Sig_Ptr : constant Ghdl_Signal_Ptr := Read_Sig (Sig);
         Prev_Idx : constant Dump_Table_Index := Sig_Ptr.Dump_Table_Idx;
         Prev_Len : Uns32;
      begin
         Alias := 0;
         Sub_Alias := 0;

         if Prev_Idx /= 0 then
            Prev_Len := Fst_Table.Table (Prev_Idx).W;
            if Prev_Len = Len then
               --  FST can only alias the full vector (and not a part of it).
               Alias := Fst_Table.Table (Prev_Idx).Hand;
            else
               pragma Assert (Prev_Len > Len);
               Sub_Alias := Prev_Idx;
            end if;
         end if;
      end;

      declare
         Name2 : String (1 .. Name_Len + 3 + 2 * Num32_Len + 1);

         procedure Append (N : Int32)
         is
            Num : String (1 .. 11);
            Num_First : Natural;
            Num_Len : Natural;
         begin
            Grt.To_Strings.To_String (Num, Num_First, Ghdl_I32 (N));
            Num_Len := Num'Last - Num_First + 1;
            Name2 (Name_Len + 1 .. Name_Len + Num_Len) :=
              Num (Num_First .. Num'Last);
            Name_Len := Name_Len + Num_Len;
         end Append;

         Str : Ghdl_C_String;
      begin
         Str := To_Ghdl_C_String (Name2'Address);
         if Decl = Null_Node then
            Name_Len := 0;
            Append (Idx);
         else
            for I in 1 .. Name_Len loop
               Name2 (I) := Name_Addr (I);
            end loop;
         end if;

         if Sig_Typ.Kind = Type_Vector then
            Name2 (Name_Len + 1) := '[';
            Name_Len := Name_Len + 1;
            Append (Sig_Typ.Abound.Left);
            Name2 (Name_Len + 1) := ':';
            Name_Len := Name_Len + 1;
            Append (Sig_Typ.Abound.Right);
            Name2 (Name_Len + 1) := ']';
            Name_Len := Name_Len + 1;
         end if;

         Name2 (Name_Len + 1) := ASCII.NUL;

         Hand := fstWriterCreateVar
           (Context, Vt, Vd, unsigned (Len), Str, Alias);
      end;

      if Alias = 0 then
         --  Add the signal (or vector of signals) to the FST table.
         Info := Fst_Sig_Info'(Typ => Typ,
                               Is_Drv => Is_Drv,
                               Seen => False,
                               Alias => 0,
                               Sig => Sig,
                               W => Len,
                               Hand => Hand);
         Fst_Table.Append (Info);

         if Sub_Alias /= 0 then
            Fst_Table.Table (Fst_Table.Last).Alias :=
              Fst_Table.Table (Sub_Alias).Alias;
            Fst_Table.Table (Sub_Alias).Alias := Fst_Table.Last;
         else
            --  And set the dump index.
            for I in 1 .. Nbr_Sig loop
               Get_Signal (Info, I - 1).Dump_Table_Idx := Fst_Table.Last;
            end loop;

            Max_Width := Uns32'Max (Max_Width, Len);
         end if;
      end if;
   end Add_Signal_Scalar;

   procedure Add_Signal_Any (Sig : Memory_Ptr;
                             Typ : Type_Acc;
                             Decl_Type : Node;
                             Vd : fstVarDir;
                             Is_Drv : Boolean;
                             Decl : Node;
                             Idx : Int32)
   is
      use Simul.Vhdl_Simul;
   begin
      case Typ.Kind is
         when Type_Scalars
           | Type_Vector =>
            Add_Signal_Scalar (Sig, Typ, Decl_Type, Vd, Is_Drv, Decl, Idx);
         when Type_Record =>
            declare
               List : constant Node_Flist := Get_Elements_Declaration_List
                 (Decl_Type);
               El : Node;
            begin
               fstWriterSetScope
                 (Context, FST_ST_VHDL_RECORD, To_C_String (Decl, Idx), null);
               for I in Typ.Rec.E'Range loop
                  El := Get_Nth_Element (List, Natural (I - 1));
                  Add_Signal_Any (Sig_Index (Sig, Typ.Rec.E (I).Offs.Net_Off),
                                  Typ.Rec.E (I).Typ, Get_Type (El),
                                  Vd, Is_Drv, El, 0);
               end loop;
               fstWriterSetUpscope (Context);
            end;
         when Type_Array =>
            declare
               El : constant Type_Acc := Typ.Arr_El;
               El_Type : Node;
               Sidx : Int32;
            begin
               if Typ.Alast then
                  El_Type := Get_Element_Subtype (Decl_Type);
               else
                  El_Type := Decl_Type;
               end if;
               fstWriterSetScope
                 (Context, FST_ST_VCD_UNION, To_C_String (Decl, Idx), null);
               for I in 1 .. Typ.Abound.Len loop
                  case Typ.Abound.Dir is
                     when Dir_To =>
                        Sidx := Typ.Abound.Left + Int32 (I - 1);
                     when Dir_Downto =>
                        Sidx := Typ.Abound.Left - Int32 (I - 1);
                  end case;
                  Add_Signal_Any (Sig_Index (Sig, (I - 1) * El.W), El,
                                  El_Type, Vd, Is_Drv, Null_Node, Sidx);
               end loop;
               fstWriterSetUpscope (Context);
            end;
         when others =>
            raise Internal_Error;
      end case;
   end Add_Signal_Any;

   procedure Add_Signal (S : Signal_Entry)
   is
      Vd : fstVarDir;
      Is_Drv : Boolean;
   begin
      Is_Drv := False;
      case Get_Kind (S.Decl) is
         when Iir_Kind_Signal_Declaration =>
            Vd := FST_VD_IMPLICIT;
         when Iir_Kind_Interface_Signal_Declaration =>
            case Get_Mode (S.Decl) is
               when Iir_Unknown_Mode =>
                  raise Internal_Error;
               when Iir_Linkage_Mode =>
                  Vd := FST_VD_LINKAGE;
               when Iir_Buffer_Mode =>
                  Vd := FST_VD_BUFFER;
               when Iir_Out_Mode =>
                  Vd := FST_VD_OUTPUT;
                  Is_Drv := True;
               when Iir_Inout_Mode =>
                  Vd := FST_VD_INOUT;
               when Iir_In_Mode =>
                  Vd := FST_VD_INPUT;
            end case;
         when others =>
            --  Attributes...
            raise Internal_Error;
      end case;

      Add_Signal_Any (S.Sig, S.Typ, Get_Type (S.Decl), Vd, Is_Drv, S.Decl, 0);
   end Add_Signal;

   --  Classify an instance.
   type Hierarchy_Kind is
     (
      --  In a top-level package (or a nested package within).
      --  Outside the design hierarchy.
      Hierarchy_Package,

      --  In the design hierarchy.
      Hierarchy_Design,

      --  A block for a component.
      Hierarchy_Component
     );

   --  Return True iff INST is in the design hierarchy (and not a top-level
   --  package).
   function In_Hierarchy (Inst : Synth_Instance_Acc) return Hierarchy_Kind is
   begin
      case Get_Kind (Get_Source_Scope (Inst)) is
         when Iir_Kinds_Package_Declaration =>
            declare
               Parent : constant Synth_Instance_Acc :=
                 Get_Instance_Parent (Inst);
            begin
               if Parent = Root_Instance then
                  return Hierarchy_Package;
               else
                  return In_Hierarchy (Parent);
               end if;
            end;
         when Iir_Kind_Component_Declaration =>
            return Hierarchy_Component;
         when others =>
            return Hierarchy_Design;
      end case;
   end In_Hierarchy;

   --  Called after elaboration.
   procedure Fst_Start
   is
      Prev_Depth : Natural;
      Prev_Inst : Synth_Instance_Acc;
   begin
      --  Do nothing if there is no VCD file to generate.
      if Context = Null_fstContext then
         return;
      end if;

      Enum_Maps.Init (Enums);

      Max_Width := 0;

      --  First signals in library packages, then signals in hierarchy.
      Prev_Depth := 1;
      Prev_Inst := Root_Instance;
      for K in Hierarchy_Package .. Hierarchy_Design loop
         for I in Signals_Table.First .. Signals_Table.Last loop
            declare
               E : Signal_Entry renames Signals_Table.Table (I);
               Cur_Inst : constant Synth_Instance_Acc := E.Inst;
               Cur_Kind : Hierarchy_Kind;
            begin
               Cur_Kind := In_Hierarchy (Cur_Inst);
               if Flag_Components and Cur_Kind = Hierarchy_Component then
                  Cur_Kind := Hierarchy_Design;
               end if;

               if Cur_Kind = K then
                  --  Check type: if not supported, print a warning
                  --  and continue.
                  --  Adjust FST hierarchy compared to the previous signal
                  if Cur_Inst /= Prev_Inst then
                     Adjust_Scope (Prev_Depth, Prev_Inst, Cur_Inst);
                  end if;

                  --  Emit signal
                  Add_Signal (E);
               end if;
            end;
         end loop;
      end loop;

      for I in 2 .. Prev_Depth loop
         if not Is_Discarded_Scope (Prev_Inst) then
            fstWriterSetUpscope (Context);
         end if;
         Prev_Inst := Get_Instance_Parent (Prev_Inst);
      end loop;

      --  Allocate a string buffer large enough for any signal.
      Buffer := To_Ghdl_C_String (Grt.C.Malloc (size_t (Max_Width)));

      Enum_Maps.Free (Enums);

      Register_Cycle_Hook (Fst_Cycle'Access);
   end Fst_Start;

   --  Called before each non delta cycle.
   procedure Fst_Cycle
   is
      Last : Natural;
   begin
      --  Disp values.
      fstWriterEmitTimeChange (Context, Unsigned_64 (Current_Time));

      if Current_Time = 0 then
         --  Disp all values.
         for I in Fst_Table.First .. Fst_Table.Last loop
            Fst_Put_Var (Fst_Table.Table (I));
         end loop;
      else
         --  Disp only values changed.
         Last := 0;
         for I in Changed_Sig_Table.First .. Changed_Sig_Table.Last loop
            declare
               Sig : constant Ghdl_Signal_Ptr := Changed_Sig_Table.Table (I);
               Idx : constant Dump_Table_Index := Sig.Dump_Table_Idx;
               Info : Fst_Sig_Info renames Fst_Table.Table (Idx);
            begin
               pragma Assert (Sig.Flags.RO_Event);
               Sig.Flags.RO_Event := False;

               if not Info.Seen then
                  --  Not yet dumped.
                  Fst_Put_Var (Info);

                  --  Do not dump vectors several times.
                  if Info.Typ in Vcd_Vector_Type then
                     Info.Seen := True;
                     Last := Last + 1;
                     Changed_Sig_Table.Table (Last) := Sig;
                  end if;
               end if;
            end;
         end loop;

         --  Clear the Seen flag.
         for I in Changed_Sig_Table.First .. Last loop
            declare
               Sig : constant Ghdl_Signal_Ptr := Changed_Sig_Table.Table (I);
               Idx : constant Dump_Table_Index := Sig.Dump_Table_Idx;
               Info : Fst_Sig_Info renames Fst_Table.Table (Idx);
            begin
               Info.Seen := False;
            end;
         end loop;

         --  Flush the table.
         Changed_Sig_Table.Set_Last (0);
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
end Simul.Fst;
