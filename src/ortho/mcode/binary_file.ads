--  Binary file handling.
--  Copyright (C) 2006 Tristan Gingold
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
with System;
with Interfaces; use Interfaces;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Ortho_Ident; use Ortho_Ident;
with Tables;
with Memsegs;

package Binary_File is
   type Section_Type is limited private;
   type Section_Acc is access Section_Type;

   type Section_Flags is new Unsigned_32;
   Section_None   : constant Section_Flags;
   Section_Exec   : constant Section_Flags;
   Section_Read   : constant Section_Flags;
   Section_Write  : constant Section_Flags;
   Section_Zero   : constant Section_Flags;
   Section_Strtab : constant Section_Flags;
   Section_Debug  : constant Section_Flags;

   type Byte is new Unsigned_8;

   type Symbol is range -2 ** 31 .. 2 ** 31 - 1;
   for Symbol'Size use 32;
   Null_Symbol : constant Symbol := 0;

   type Pc_Type is mod System.Memory_Size;
   Null_Pc : constant Pc_Type := 0;
   --  Number of bytes in a word.
   Pc_Type_Sizeof : constant := Pc_Type'Size / 8;

   type Arch_Kind is
     (Arch_Unknown, Arch_X86, Arch_X86_64, Arch_Sparc, Arch_Ppc);
   Arch : Arch_Kind := Arch_Unknown;

   --  Dump assembly when generated.
   Dump_Asm : Boolean := False;

   Debug_Hex : Boolean := False;

   --  Create a section.
   procedure Create_Section (Sect : out Section_Acc;
                             Name : String; Flags : Section_Flags);
   procedure Set_Section_Info (Sect : Section_Acc;
                               Link : Section_Acc;
                               Align : Natural;
                               Esize : Natural);

   procedure Merge_Section (Dest : Section_Acc; Src : Section_Acc);

   --  Set the current section.
   procedure Set_Current_Section (Sect : Section_Acc);

   --  Create an undefined local (anonymous) symbol in the current section.
   function Create_Local_Symbol return Symbol;
   function Create_Symbol (Name : O_Ident; Code : Boolean) return Symbol;

   --  Research symbol NAME, very expansive call.
   --  Return NULL_Symbol if not found.
   function Get_Symbol (Name : String) return Symbol;

   --  Get the virtual address of a symbol.
   function Get_Symbol_Vaddr (Sym : Symbol) return Pc_Type;
   pragma Inline (Get_Symbol_Vaddr);

   --  Return True iff SYM is a code symbol.
   function Is_Symbol_Code (Sym : Symbol) return Boolean;

   --  Set the value of a symbol.
   procedure Set_Symbol_Pc (Sym : Symbol; Export : Boolean);
   function Get_Symbol_Value (Sym : Symbol) return Pc_Type;

   --  Get the current PC.
   function Get_Current_Pc return Pc_Type;
   pragma Inline (Get_Current_Pc);

   function Get_Pc (Sect : Section_Acc) return Pc_Type;
   pragma Inline (Get_Pc);

   --  Align the current section of 2 ** ALIGN.
   procedure Gen_Pow_Align (Align : Natural);

   --  Generate LENGTH times 0.
   procedure Gen_Space (Length : Integer_32);

   --  Add a reloc in the current section at the current address.
   procedure Gen_X86_Pc32 (Sym : Symbol; Off : Unsigned_32);
   procedure Gen_Sparc_Disp22 (W : Unsigned_32; Sym : Symbol);
   procedure Gen_Sparc_Disp30 (W : Unsigned_32; Sym : Symbol);
   procedure Gen_Sparc_Hi22 (W : Unsigned_32;
                             Sym : Symbol; Off : Unsigned_32);
   procedure Gen_Sparc_Lo10 (W : Unsigned_32;
                             Sym : Symbol; Off : Unsigned_32);

   --  An absolute reloc.
   procedure Gen_Abs (Sym : Symbol; Offset : Integer_32);

   --  Add a 32 bits value with a symbol relocation in the current section at
   --  the current address.
   procedure Gen_X86_32 (Sym : Symbol; Offset : Integer_32);
   procedure Gen_Sparc_32 (Sym : Symbol; Offset : Integer_32);

   --  Image based address (for Win64 unwind info)
   --  The result is a 32b offset from the image base to SYM + OFFSET
   procedure Gen_X86_Img_32 (Sym : Symbol; Offset : Unsigned_32);

   procedure Gen_Ppc_24 (V : Unsigned_32; Sym : Symbol);

   procedure Gen_Ua_Addr (Sym : Symbol; Offset : Integer_32);
   procedure Gen_Ua_32 (Sym : Symbol);

   --  Start/finish an instruction in the current section.
   procedure Start_Insn;
   procedure End_Insn;
   --  Pre allocate L bytes.
   procedure Prealloc (L : Pc_Type);

   --  Add bits in the current section.
   --  Space must be pre-allocated.
   procedure Gen_8 (B : Byte);
   procedure Gen_8 (B0, B1 : Byte);

   procedure Gen_16 (B : Unsigned_32);
   procedure Gen_32 (B : Unsigned_32);
   procedure Gen_64 (B : Unsigned_64);

   --  Add bits in the current section, but as stand-alone data.
   --  Displayed if Dump_Asm.
   procedure Gen_Data_8 (B : Unsigned_8);
   procedure Gen_Data_16 (B : Unsigned_32);
   procedure Gen_Data_32 (Sym : Symbol; Offset : Integer_32);

   --  Modify already generated code.
   procedure Patch_8 (Pc : Pc_Type; V : Unsigned_8);
   procedure Patch_16 (Pc : Pc_Type; V : Unsigned_32);
   procedure Patch_32 (Pc : Pc_Type; V : Unsigned_32);

   function To_Unsigned_32 (Off : Pc_Type) return Unsigned_32;

   --  Binary writers:

   --  Set ERROR in case of error (undefined symbol).
   --procedure Write_Memory (Error : out Boolean);

   procedure Disp_Stats;
   procedure Finish;
private
   type Byte_Array_Base is array (Pc_Type range <>) of Byte;
   subtype Byte_Array is Byte_Array_Base (Pc_Type);
   type Byte_Array_Acc is access Byte_Array;
   pragma No_Strict_Aliasing (Byte_Array_Acc);

   function To_Byte_Array_Acc is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Byte_Array_Acc);

   type String_Acc is access String;
   --type Section_Flags is new Unsigned_32;

   subtype Pc_Type8 is Pc_Type range 0 .. 255;

   --  Relocations.
   type Reloc_Kind is (Reloc_32, Reloc_Pc32,
                       Reloc_Abs, Reloc_Img_32,
                       Reloc_Ua_32, Reloc_Ua_Addr,
                       Reloc_Disp22, Reloc_Disp30,
                       Reloc_Hi22, Reloc_Lo10,
                       Reloc_Ppc_Addr24);
   type Reloc_Type;
   type Reloc_Acc is access Reloc_Type;
   type Reloc_Type is record
      Kind : Reloc_Kind;
      --  If true, the reloc was already applied.
      Done : Boolean;
      --  Negative addend (only for pcrel relocs).
      Neg_Addend : Pc_Type8;
      --  Next in simply linked list.
      --  next reloc in the section.
      Sect_Next : Reloc_Acc;
      --  next reloc for the symbol.
      Sym_Next : Reloc_Acc;
      --  Address that must be relocated.
      Addr : Pc_Type;
      --  Symbol.
      Sym : Symbol;
   end record;

   type Section_Type is record
      --  Simply linked list of sections.
      Next : Section_Acc;
      --  Flags.
      Flags : Section_Flags;
      --  Name of the section.
      Name : String_Acc;
      --  Link to another section (used by ELF).
      Link : Section_Acc;
      --  Alignment (in power of 2).
      Align : Natural;
      --  Entry size (if any).
      Esize : Natural;
      --  Offset of the next data in DATA.
      Pc : Pc_Type;
      --  Offset of the current instruction.
      Insn_Pc : Pc_Type;
      --  Data for this section.
      Data : Byte_Array_Acc;
      --  Max address for data (before extending the area).
      Data_Max : Pc_Type;
      --  Chain of relocs defined in this section.
      First_Reloc : Reloc_Acc;
      Last_Reloc : Reloc_Acc;
      --  Number of relocs in this section.
      Nbr_Relocs : Natural;
      --  Section number (set and used by binary writer).
      Number : Natural;
      --  Virtual address, if set.
      Vaddr : Pc_Type; -- SSE.Integer_Address;
      --  Offset relative to the image start
      Img_Off : Pc_Type;
      --  Memory for this segment.
      Seg : Memsegs.Memseg_Type;
   end record;

   Section_Exec   : constant Section_Flags := 2#0000_0001#;
   Section_Read   : constant Section_Flags := 2#0000_0010#;
   Section_Write  : constant Section_Flags := 2#0000_0100#;
   Section_Zero   : constant Section_Flags := 2#0000_1000#;
   Section_Strtab : constant Section_Flags := 2#0001_0000#;
   Section_Debug  : constant Section_Flags := 2#0010_0000#;
   Section_None   : constant Section_Flags := 2#0000_0000#;

   --  Scope of a symbol:
   --  SYM_PRIVATE: not visible outside of the file.
   --  SYM_UNDEF: not (yet) defined, unresolved.
   --  SYM_GLOBAL: visible to all files.
   --  SYM_LOCAL: locally generated symbol.
   type Symbol_Scope is (Sym_Undef, Sym_Global, Sym_Private, Sym_Local);
   subtype Symbol_Scope_External is Symbol_Scope range Sym_Undef .. Sym_Global;

   type Symbol_Type is record
      Section : Section_Acc;
      Value : Pc_Type;
      Scope : Symbol_Scope;
      --  True if the symbol is referenced/used.
      Used : Boolean;
      --  True if the symbol represent code (and therefore could be placed in
      --  a PLT).
      Code : Boolean;
      --  Name of the symbol.
      Name : O_Ident;
      --  List of relocation made with this symbol.
      Relocs : Reloc_Acc;
      --  Symbol number, from 0.
      Number : Natural;
   end record;

   --  Number of sections.
   Nbr_Sections : Natural := 0;
   --  Simply linked list of sections.
   Section_Chain : Section_Acc := null;
   Section_Last : Section_Acc := null;

   package Symbols is new Tables
     (Table_Component_Type => Symbol_Type,
      Table_Index_Type => Symbol,
      Table_Low_Bound => 2,
      Table_Initial => 1024);

   function Pow_Align (V : Pc_Type; Align : Natural) return Pc_Type;

   function Get_Symbol_Name (Sym : Symbol) return String;
   function Get_Symbol_Name_Length (Sym : Symbol) return Natural;

   procedure Set_Symbol_Value (Sym : Symbol; Val : Pc_Type);
   pragma Inline (Set_Symbol_Value);

   procedure Set_Scope (Sym : Symbol; Scope : Symbol_Scope);
   pragma Inline (Set_Scope);

   function Get_Scope (Sym : Symbol) return Symbol_Scope;
   pragma Inline (Get_Scope);

   function Get_Section (Sym : Symbol) return Section_Acc;
   pragma Inline (Get_Section);

   procedure Set_Section (Sym : Symbol; Sect : Section_Acc);
   pragma Inline (Set_Section);

   function Get_Name (Sym : Symbol) return O_Ident;
   pragma Inline (Get_Name);

   procedure Apply_Reloc (Sect : Section_Acc; Reloc : Reloc_Acc);
   pragma Inline (Apply_Reloc);

   procedure Set_Number (Sym : Symbol; Num : Natural);
   pragma Inline (Set_Number);

   function Get_Number (Sym : Symbol) return Natural;
   pragma Inline (Get_Number);

   function Get_Used (Sym : Symbol) return Boolean;
   pragma Inline (Get_Used);

   procedure Do_Intra_Section_Reloc (Sect : Section_Acc);

   function S_Local (Sym : Symbol) return Boolean;
   pragma Inline (S_Local);

   procedure Resize (Sect : Section_Acc; Size : Pc_Type);

   procedure Free is new Ada.Unchecked_Deallocation
     (Name => Reloc_Acc, Object => Reloc_Type);

   Write_Error : exception;
end Binary_File;
