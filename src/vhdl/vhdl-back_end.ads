--  Back-end specialization
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
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
with Vhdl.Nodes; use Vhdl.Nodes;

package Vhdl.Back_End is
   --  Back-end options.
   type Parse_Option_Acc is access function (Opt : String) return Boolean;
   Parse_Option : Parse_Option_Acc := null;

   --  Disp back-end option help.
   type Disp_Option_Acc is access procedure;
   Disp_Option : Disp_Option_Acc := null;

   --  DECL is an architecture (library unit) or a subprogram (specification)
   --  decorated with a FOREIGN attribute.  Do back-end checks.
   --  May be NULL for no additionnal checks.
   type Sem_Foreign_Acc is access procedure (Decl : Iir);
   Sem_Foreign : Sem_Foreign_Acc := null;

   --  Utils for foreign analysis.

   type Foreign_Kind_Type is (Foreign_Unknown,
                              Foreign_Vhpidirect,
                              Foreign_Intrinsic);

   type Foreign_Info_Type (Kind : Foreign_Kind_Type := Foreign_Unknown)
   is record
      case Kind is
         when Foreign_Unknown =>
            null;
         when Foreign_Vhpidirect =>
            Lib_Name : String (1 .. 32);
            Lib_Len : Natural;
            Subprg_Name : String (1 .. 64);
            Subprg_Len : Natural;
         when Foreign_Intrinsic =>
            null;
      end case;
   end record;

   Foreign_Bad : constant Foreign_Info_Type := (Kind => Foreign_Unknown);

   --  Return a foreign_info for DECL.
   --  Can generate error messages, if the attribute expression is ill-formed.
   function Translate_Foreign_Id (Decl : Iir) return Foreign_Info_Type;

   --  Wrapper for Sem_Foreign: call the hook.
   procedure Sem_Foreign_Wrapper (Decl : Iir);

   type Sem_Foreign_Hook_Type is access
     procedure (Decl : Iir; Info : Vhdl.Back_End.Foreign_Info_Type);

   --  Hook called by Sem_Foreign.
   Sem_Foreign_Hook : Sem_Foreign_Hook_Type := null;
end Vhdl.Back_End;
