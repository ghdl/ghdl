--  API to build a netlist.
--  Copyright (C) 2017 Tristan Gingold
--
--  This file is part of GHDL.
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston,
--  MA 02110-1301, USA.

with Netlists.Gates; use Netlists.Gates;

package Netlists.Builders is
   type Context is private;
   type Context_Acc is access Context;

   --  Create a builder for Design.  Must be called once.
   function Build_Builders (Design : Module) return Context_Acc;

   --  Set the parent for the new instances.
   procedure Set_Parent (Ctxt : Context_Acc; Parent : Module);

   function Build_Dyadic (Ctxt : Context_Acc;
                          Id : Dyadic_Module_Id;
                          L, R : Net) return Net;

   function Build_Monadic (Ctxt : Context_Acc;
                           Id : Monadic_Module_Id;
                           Op : Net) return Net;

   function Build_Compare (Ctxt : Context_Acc;
                           Id : Compare_Module_Id;
                           L, R : Net) return Net;

   function Build_Const_UB32 (Ctxt : Context_Acc;
                              Val : Uns32;
                              W : Width) return Net;
   function Build_Const_UL32 (Ctxt : Context_Acc;
                              Val : Uns32;
                              Xz : Uns32;
                              W : Width) return Net;

   function Build_Edge (Ctxt : Context_Acc;
                        Is_Pos : Boolean;
                        Src : Net) return Net;

   function Build_Mux2 (Ctxt : Context_Acc;
                        Sel : Net;
                        I0, I1 : Net) return Net;
   function Build_Mux4 (Ctxt : Context_Acc;
                        Sel : Net;
                        I0, I1, I2, I3 : Net) return Net;

   function Build_Concat2 (Ctxt : Context_Acc; I0, I1 : Net) return Net;
   function Build_Concat3 (Ctxt : Context_Acc; I0, I1, I2 : Net) return Net;
   function Build_Concat4 (Ctxt : Context_Acc; I0, I1, I2, I3 : Net)
                          return Net;

   function Build_Trunc
     (Ctxt : Context_Acc; Id : Module_Id; I : Net; W : Width) return Net;
   function Build_Extend
     (Ctxt : Context_Acc; Id : Module_Id; I : Net; W : Width) return Net;

   function Build_Slice
     (Ctxt : Context_Acc; I : Net; Off, W : Width) return Net;
   function Build_Extract_Bit
     (Ctxt : Context_Acc; I : Net; Off : Width) return Net;

   function Build_Output (Ctxt : Context_Acc; W : Width) return Net;
   function Build_Signal (Ctxt : Context_Acc; Name : Sname; W : Width)
                         return Net;
   function Build_Isignal (Ctxt : Context_Acc; Name : Sname; Init : Net)
                          return Net;

   --  A simple flip-flop.
   function Build_Dff (Ctxt : Context_Acc;
                       Clk : Net;
                       D : Net) return Net;
   --  A flip-flop with an initial value (only for fpga)
   function Build_Idff (Ctxt : Context_Acc;
                        Clk : Net;
                        D : Net;
                        Init : Net) return Net;
private
   type Module_Arr is array (Module_Id range <>) of Module;

   type Context is record
      Design : Module;
      Parent : Module;
      Num : Uns32;
      M_Dyadic : Module_Arr (Dyadic_Module_Id);
      M_Monadic : Module_Arr (Monadic_Module_Id);
      M_Compare : Module_Arr (Compare_Module_Id);
      M_Concat : Module_Arr (Concat_Module_Id);
      M_Const_UB32 : Module;
      M_Const_UL32 : Module;
      M_Posedge : Module;
      M_Negedge : Module;
      M_Mux2 : Module;
      M_Mux4 : Module;
      M_Output : Module;
      M_Signal : Module;
      M_Isignal : Module;
      M_Dff : Module;
      M_Idff : Module;
      M_Truncate : Module_Arr (Truncate_Module_Id);
      M_Extend : Module_Arr (Extend_Module_Id);
      M_Extract : Module;
   end record;
end Netlists.Builders;
