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

with Types; use Types;

with Netlists; use Netlists;
with Netlists.Builders;

with Vhdl.Nodes;

with Elab.Vhdl_Objtypes; use Elab.Vhdl_Objtypes;

with Synth.Environment;
with Synth.Environment.Debug;

package Synth.Vhdl_Environment is

   type Decl_Type is record
      Obj : Vhdl.Nodes.Node;
      Typ : Type_Acc;
   end record;

   function Get_Bitwidth (Val : Memtyp) return Uns32;

   function Memtyp_To_Net (Ctxt : Builders.Context_Acc; Val : Memtyp)
                          return Net;

   function Partial_Memtyp_To_Net
     (Ctxt : Builders.Context_Acc; Val : Memtyp; Off : Uns32; Wd : Uns32)
     return Net;

   procedure Warning_No_Assignment
     (Decl : Decl_Type; First_Off : Uns32; Last_Off : Uns32);

   procedure Error_Multiple_Assignments
     (Decl : Decl_Type; First_Off : Uns32; Last_Off : Uns32);

   package Env is new Synth.Environment
     (Decl_Type => Decl_Type,
      Static_Type => Elab.Vhdl_Objtypes.Memtyp,
      Get_Width => Get_Bitwidth,
      Is_Equal => Is_Equal,
      Static_To_Net => Memtyp_To_Net,
      Partial_Static_To_Net => Partial_Memtyp_To_Net,
      Warning_No_Assignment => Warning_No_Assignment,
      Error_Multiple_Assignments => Error_Multiple_Assignments);
--      "+" => Vhdl.Nodes.Get_Location);

   package Debug is new Env.Debug;
end Synth.Vhdl_Environment;
