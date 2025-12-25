--  Foreign subprogram calls.
--  Copyright (C) 2023 Tristan Gingold
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

with System; use System;
with Ada.Unchecked_Conversion;

with Hash; use Hash;
with Interning;
with Types; use Types;
with Std_Names;

with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Back_End; use Vhdl.Back_End;

with Elab.Memtype; use Elab.Memtype;
with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;
with Synth.Errors; use Synth.Errors;

with Grt.Types; use Grt.Types;
with Grt.Asserts;
with Trans_Foreign;

package body Synth.Vhdl_Foreign is
   --  Cache of node to subprogram address.
   --  Avoid multiple lookups (and decoding of FOREIGN value).
   --  TODO: maybe also cache the caller ?

   function Get_Intrinsic_Address (Decl : Iir) return Address
   is
      use Std_Names;
      Id : constant Name_Id := Get_Identifier (Decl);
   begin
      case Id is
         when Name_Get_Assert_Count =>
            return Grt.Asserts.Ghdl_Get_Assert_Count'Address;
         when Name_Clear_Assert_Count =>
            return Grt.Asserts.Ghdl_Clear_Assert_Count'Address;
         when Name_Control_Simulation
            | Name_Get_Resolution_Limit =>
            --  Handled in vhdl_eval.
            null;
         when others =>
            Error_Msg_Sem (+Decl, "unknown foreign intrinsic %i", +Decl);
      end case;
      return Null_Address;
   end Get_Intrinsic_Address;

   type Sym_Object_Type is record
      N : Node;
      Handler : Address;
   end record;

   function Sym_Build (N : Node) return Sym_Object_Type
   is
      Info : Foreign_Info_Type;
      Handler : Address;
   begin
      Info := Translate_Foreign_Id (N);

      case Info.Kind is
         when Foreign_Vhpidirect =>
            Handler := Trans_Foreign.Get_Foreign_Address (N, Info);
         when Foreign_Intrinsic =>
            Handler := Get_Intrinsic_Address (N);
         when Foreign_Unknown =>
            Handler := Null_Address;
      end case;
      return (N, Handler);
   end Sym_Build;

   function Sym_Equal (Obj : Sym_Object_Type; N : Node) return Boolean is
   begin
      return Obj.N = N;
   end Sym_Equal;

   function Sym_Hash (N : Node) return Hash_Value_Type is
   begin
      return Hash_Value_Type (N);
   end Sym_Hash;

   package Sym_Interning is new Interning
     (Key_Type => Node,
      Object_Type => Sym_Object_Type,
      Hash => Sym_Hash,
      Build => Sym_Build,
      Equal => Sym_Equal);

   --  Classify a type; this determines the profile of the function.
   type Type_Class is (Class_Void, Class_I32, Class_I8, Class_Unknown);

   type Type_Class_Array is array (Nat32 range <>) of Type_Class;

   function Classify (T : Type_Acc) return Type_Class is
   begin
      case T.Kind is
         when Type_Discrete =>
            if T.Sz = 4 then
               return Class_I32;
            elsif T.Sz = 1 then
               return Class_I8;
            end if;
         when others =>
            null;
      end case;
      return Class_Unknown;
   end Classify;

   --  Callers for each profile.
   --  This doesn't scale!

   procedure Call_Proc (Args : Valtyp_Array;
                        Res : Memory_Ptr;
                        Handler : Address)
   is
      pragma Assert (Args'Length = 0);
      pragma Assert (Res = null);
      type Proto_Acc is access procedure;
      pragma Convention (C, Proto_Acc);
      function To_Proto_Acc is new Ada.Unchecked_Conversion
        (Address, Proto_Acc);
      Proto : constant Proto_Acc := To_Proto_Acc (Handler);
   begin
      Proto.all;
   end Call_Proc;

   --  For functions that returns an int32 and no arguments.
   procedure Call_I32 (Args : Valtyp_Array;
                       Res : Memory_Ptr;
                       Handler : Address)
   is
      pragma Assert (Args'Length = 0);
      type Proto_Acc is access function return Ghdl_I32;
      pragma Convention (C, Proto_Acc);
      function To_Proto_Acc is new Ada.Unchecked_Conversion
        (Address, Proto_Acc);
      Proto : constant Proto_Acc := To_Proto_Acc (Handler);
      R : Ghdl_I32;
   begin
      R := Proto.all;
      Write_I32 (Res, R);
   end Call_I32;

   procedure Call_I32_I8 (Args : Valtyp_Array;
                          Res : Memory_Ptr;
                          Handler : Address)
   is
      pragma Assert (Args'Length = 1);
      type Proto_Acc is access function (Arg : Ghdl_E8) return Ghdl_I32;
      pragma Convention (C, Proto_Acc);
      function To_Proto_Acc is new Ada.Unchecked_Conversion
        (Address, Proto_Acc);
      Proto : constant Proto_Acc := To_Proto_Acc (Handler);
      R : Ghdl_I32;
      Arg : Ghdl_E8;
   begin
      Arg := Read_U8 (Get_Memtyp (Args (1)));
      R := Proto.all (Arg);
      Write_I32 (Res, R);
   end Call_I32_I8;

   type Call_Acc is access procedure (Args : Valtyp_Array;
                                      Res : Memory_Ptr;
                                      Handler : Address);


   --  Association between a profile and the call function.
   type Profile_Record is record
      Nargs : Nat32;
      Args : Type_Class_Array (1 .. 4);
      Res : Type_Class;
      Call : Call_Acc;
   end record;

   function Profile_Match (L, R : Profile_Record) return Boolean is
   begin
      if L.Nargs /= R.Nargs
        or else L.Res /= R.Res
      then
         return False;
      end if;
      for J in 1 .. L.Nargs loop
         if L.Args (J) /= R.Args (J) then
            return False;
         end if;
      end loop;
      return True;
   end Profile_Match;

   --  List of known/implemented profile.
   type Profile_Array is array (Natural range <>) of Profile_Record;

   Profiles : constant Profile_Array :=
     (0 => (Nargs => 0,
            Args => (others => Class_Unknown),
            Res => Class_Void,
            Call => Call_Proc'Access),
      1 => (Nargs => 0,
            Args => (others => Class_Unknown),
            Res => Class_I32,
            Call => Call_I32'Access),
      2 => (Nargs => 1,
            Args => (1 => Class_I8, others => Class_Unknown),
            Res => Class_I32,
            Call => Call_I32_I8'Access));

   function Call_Subprogram (Syn_Inst : Synth_Instance_Acc;
                             Sub_Inst : Synth_Instance_Acc;
                             Imp      : Node;
                             Loc : Node) return Valtyp
   is
      Args : Valtyp_Array (1 .. 4);
      Ret_Typ : Type_Acc;
      Inter : Node;
      Sym : Sym_Object_Type;
      Profile : Profile_Record;
      Res : Valtyp;
      Res_Mem : Memory_Ptr;
   begin
      --  Find the handle.
      Sym := Sym_Interning.Get (Imp);
      if Sym.Handler = Null_Address then
         Error_Msg_Synth (Sub_Inst, Loc, "cannot load FOREIGN %n", +Imp);
         return No_Valtyp;
      end if;

      --  Determine the profile.
      Inter := Get_Interface_Declaration_Chain (Imp);
      Profile.Nargs := 0;
      Profile.Args := (others => Class_Unknown);
      Profile.Call := null;
      while Inter /= Null_Node loop
         declare
            C : Type_Class;
            Val : Valtyp;
         begin
            Profile.Nargs := Profile.Nargs + 1;
            Val := Get_Value (Sub_Inst, Inter);
            C := Classify (Val.Typ);
            if C = Class_Unknown then
               Error_Msg_Synth
                 (Syn_Inst, Loc,
                  "unhandled type for interface %n of FOREIGN %n",
                  (+Inter, +Imp));
               return No_Valtyp;
            end if;
            Profile.Args (Profile.Nargs) := C;
            Args (Profile.Nargs) := Val;
         end;
         Inter := Get_Chain (Inter);
      end loop;

      case Iir_Kinds_Subprogram_Declaration (Get_Kind (Imp)) is
         when Iir_Kind_Function_Declaration =>
            Ret_Typ := Get_Subtype_Object (Syn_Inst, Get_Return_Type (Imp));
            Profile.Res := Classify (Ret_Typ);
            if Profile.Res = Class_Unknown then
               Error_Msg_Synth
                 (Syn_Inst, Loc,
                  "unhandled type for result of FOREIGN %n", +Imp);
               return No_Valtyp;
            end if;

         when Iir_Kind_Procedure_Declaration =>
            Ret_Typ := null;
            Profile.Res := Class_Void;
      end case;

      --  Find the profile.
      for I in Profiles'Range loop
         if Profile_Match (Profiles (I), Profile) then
            Profile.Call := Profiles (I).Call;
            exit;
         end if;
      end loop;

      if Profile.Call = null then
         Error_Msg_Synth
           (Syn_Inst, Loc, "unhandled caller for FOREIGN %n", +Imp);
         return No_Valtyp;
      end if;

      --  Allocate result.
      if Ret_Typ = null then
         Res := No_Valtyp;
         Res_Mem := null;
      else
         Res := Create_Value_Memory (Ret_Typ, Expr_Pool'Access);
         Res_Mem := Get_Memory (Res);
      end if;

      --  Call.
      Profile.Call.all (Args (1 .. Profile.Nargs), Res_Mem, Sym.Handler);

      return Res;
   end Call_Subprogram;

   procedure Initialize is
   begin
      Trans_Foreign.Init;
      Sym_Interning.Init;
   end Initialize;
end Synth.Vhdl_Foreign;
