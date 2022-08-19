--  API to build a netlist.
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

with Ada.Unchecked_Deallocation;
with Netlists.Gates; use Netlists.Gates;

package Netlists.Builders is
   type Context is private;
   type Context_Acc is access Context;

   No_Context : constant Context_Acc := null;

   type Uns32_Arr is array (Natural range <>) of Uns32;
   type Uns32_Arr_Acc is access Uns32_Arr;
   procedure Unchecked_Deallocate is new Ada.Unchecked_Deallocation
     (Uns32_Arr, Uns32_Arr_Acc);

   function New_Internal_Name (Ctxt : Context_Acc; Prefix : Sname := No_Sname)
                              return Sname;

   --  Create a builder for Design.  Must be called once.
   function Build_Builders (Design : Module) return Context_Acc;

   function Get_Design (Ctxt : Context_Acc) return Module;

   --  Set the parent for the new instances.
   procedure Set_Parent (Ctxt : Context_Acc; Parent : Module);

   function Build_Dyadic (Ctxt : Context_Acc;
                          Id : Dyadic_Module_Id;
                          L, R : Net) return Net;

   function Build_Shift_Rotate (Ctxt : Context_Acc;
                                Id : Shift_Rotate_Module_Id;
                                L, R : Net) return Net;

   function Build_Monadic (Ctxt : Context_Acc;
                           Id : Monadic_Module_Id;
                           Op : Net) return Net;

   function Build_Compare (Ctxt : Context_Acc;
                           Id : Compare_Module_Id;
                           L, R : Net) return Net;

   function Build_Reduce (Ctxt : Context_Acc;
                          Id : Reduce_Module_Id;
                          Op : Net) return Net;

   function Build_Const_X (Ctxt : Context_Acc;
                           W : Width) return Net;
   function Build_Const_Z (Ctxt : Context_Acc;
                           W : Width) return Net;

   function Build_Const_UB32 (Ctxt : Context_Acc;
                              Val : Uns32;
                              W : Width) return Net;
   function Build_Const_UL32 (Ctxt : Context_Acc;
                              Val : Uns32;
                              Xz : Uns32;
                              W : Width) return Net;

   function Build_Const_SB32 (Ctxt : Context_Acc;
                              Val : Int32;
                              W : Width) return Net;

   --  Large constants.
   --  Bit means only 0 or 1.
   --  Log means 0/1/Z/X.  Parameters 2N are aval, 2N+1 are bval.
   function Build_Const_Bit (Ctxt : Context_Acc;
                             W : Width) return Instance;
   function Build_Const_Log (Ctxt : Context_Acc;
                             W : Width) return Instance;

   function Build_Posedge (Ctxt : Context_Acc; Src : Net) return Net;
   function Build_Negedge (Ctxt : Context_Acc; Src : Net) return Net;

   function Build_Mux2 (Ctxt : Context_Acc;
                        Sel : Net;
                        I0, I1 : Net) return Net;
   function Build_Mux4 (Ctxt : Context_Acc;
                        Sel : Net;
                        I0, I1, I2, I3 : Net) return Net;

   --  The width of SEL gives the number of inputs (+ 2).
   --  The width of DEF (default value) gives the width of the output.
   function Build_Pmux (Ctxt : Context_Acc;
                        Sel : Net;
                        Def : Net) return Net;

   --  Build: I0 & I1 [ & I2 [ & I3 ]]
   function Build_Concat2 (Ctxt : Context_Acc; I0, I1 : Net) return Net;
   function Build_Concat3 (Ctxt : Context_Acc; I0, I1, I2 : Net) return Net;
   function Build_Concat4 (Ctxt : Context_Acc; I0, I1, I2, I3 : Net)
                          return Net;
   --  NBR_INPUTS is the number of inputs ports, W is the width of the
   --  output.
   function Build_Concatn (Ctxt : Context_Acc; W : Width; Nbr_Inputs : Uns32)
                          return Net;

   function Build_Trunc
     (Ctxt : Context_Acc; Id : Module_Id; I : Net; W : Width) return Net;
   function Build_Extend
     (Ctxt : Context_Acc; Id : Module_Id; I : Net; W : Width) return Net;

   --  Note: OFF is the offset, 0 is LSB.

   function Build_Extract
     (Ctxt : Context_Acc; I : Net; Off, W : Width) return Net;

   function Build_Extract_Bit
     (Ctxt : Context_Acc; I : Net; Off : Width) return Net;
   function Build_Dyn_Extract
     (Ctxt : Context_Acc; Mem : Net; Idx : Net; Off : Uns32; W : Width)
     return Net;

   function Build_Dyn_Insert
     (Ctxt : Context_Acc; Mem : Net; V : Net; Idx : Net; Off : Uns32)
     return Net;
   function Build_Dyn_Insert_En
     (Ctxt : Context_Acc; Mem : Net; V : Net; Idx : Net; En : Net; Off : Uns32)
     return Net;

   function Build_Memidx
     (Ctxt : Context_Acc;
      I : Net; Step : Uns32; Max : Uns32; W : Width) return Net;
   function Build_Addidx (Ctxt : Context_Acc; L, R : Net) return Net;

   function Build_Memory
     (Ctxt : Context_Acc; Name : Sname; W : Width) return Instance;
   function Build_Memory_Init
     (Ctxt : Context_Acc; Name : Sname; W : Width; Init : Net) return Instance;

   function Build_Mem_Rd
     (Ctxt : Context_Acc; Pport : Net; Addr : Net; Data_W : Width)
     return Instance;
   function Build_Mem_Rd_Sync (Ctxt : Context_Acc;
                               Pport : Net;
                               Addr : Net;
                               Clk : Net;
                               En : Net;
                               Data_W : Width) return Instance;
   function Build_Mem_Wr_Sync (Ctxt : Context_Acc;
                               Pport : Net;
                               Addr : Net;
                               Clk : Net;
                               En : Net;
                               Data : Net) return Instance;

   function Build_Mem_Multiport (Ctxt : Context_Acc; I0, I1 : Net) return Net;

   function Build_Output (Ctxt : Context_Acc; W : Width) return Net;
   function Build_Ioutput (Ctxt : Context_Acc; Init : Net) return Net;
   function Build_Inout (Ctxt : Context_Acc; W : Width) return Instance;
   function Build_Iinout (Ctxt : Context_Acc; W : Width) return Instance;
   function Build_Signal (Ctxt : Context_Acc; Name : Sname; W : Width)
                         return Net;
   function Build_Isignal (Ctxt : Context_Acc; Name : Sname; Init : Net)
                          return Net;
   function Build_Port (Ctxt : Context_Acc; N : Net) return Net;
   function Build_Enable (Ctxt : Context_Acc) return Net;

   function Build_Assert (Ctxt : Context_Acc; Name : Sname; Cond : Net)
                         return Instance;
   function Build_Assume (Ctxt : Context_Acc; Name : Sname; Cond : Net)
                         return Instance;
   function Build_Cover (Ctxt : Context_Acc; Name : Sname; Cond : Net)
                        return Instance;
   function Build_Assert_Cover (Ctxt : Context_Acc; Name : Sname; Cond : Net)
                         return Instance;

   function Build_Formal_Input
     (Ctxt : Context_Acc; Id : Formal_Module_Id; W : Width) return Net;

   --  A simple flip-flop.
   function Build_Dff (Ctxt : Context_Acc;
                       Clk : Net;
                       D : Net) return Net;
   --  A flip-flop with an initial value (only for fpga)
   --  The width is derived from INIT and D can be No_Net.
   function Build_Idff (Ctxt : Context_Acc;
                        Clk : Net;
                        D : Net;
                        Init : Net) return Net;
   function Build_Adff (Ctxt : Context_Acc;
                        Clk : Net;
                        D : Net; Rst : Net; Rst_Val : Net) return Net;
   function Build_Iadff (Ctxt : Context_Acc;
                         Clk : Net;
                         D : Net; Rst : Net; Rst_Val : Net;
                                             Init : Net) return Net;

   function Build_Mdff (Ctxt : Context_Acc;
                        Clk : Net;
                        D : Net;
                        Els : Net) return Net;
   function Build_Midff (Ctxt : Context_Acc;
                         Clk : Net;
                         D : Net;
                         Els : Net;
                         Init : Net) return Net;

   function Build_Dlatch (Ctxt : Context_Acc; En : Net; D : Net) return Net;

   function Build_Tri (Ctxt : Context_Acc; En : Net; D : Net) return Net;

   function Build_Resolver (Ctxt : Context_Acc; L, R : Net) return Net;

   function Build_Nop (Ctxt : Context_Acc; I : Net) return Net;
private
   type Module_Arr is array (Module_Id range <>) of Module;

   type Context is record
      Design : Module;
      Parent : Module;
      Num : Uns32;
      M_Dyadic : Module_Arr (Dyadic_Module_Id);
      M_Shift_Rotate : Module_Arr (Shift_Rotate_Module_Id);
      M_Monadic : Module_Arr (Monadic_Module_Id);
      M_Compare : Module_Arr (Compare_Module_Id);
      M_Concat : Module_Arr (Concat_Module_Id);
      M_Concatn : Module;
      M_Const_UB32 : Module;
      M_Const_SB32 : Module;
      M_Const_UL32 : Module;
      M_Const_X : Module;
      M_Const_Z : Module;
      M_Const_Bit : Module;
      M_Const_Log : Module;
      M_Posedge : Module;
      M_Negedge : Module;
      M_Mux2 : Module;
      M_Mux4 : Module;
      M_Pmux : Module;
      M_Nop : Module;
      M_Output : Module;
      M_Ioutput : Module;
      M_Signal : Module;
      M_Isignal : Module;
      M_Port : Module;
      M_Inout : Module;
      M_Iinout : Module;
      M_Enable : Module;
      M_Dff : Module;
      M_Idff : Module;
      M_Adff : Module;
      M_Iadff : Module;
      M_Mdff : Module;
      M_Midff : Module;
      M_Dlatch : Module;
      M_Tri : Module;
      M_Resolver : Module;
      M_Truncate : Module_Arr (Truncate_Module_Id);
      M_Extend : Module_Arr (Extend_Module_Id);
      M_Reduce : Module_Arr (Reduce_Module_Id);
      M_Extract : Module;
      M_Dyn_Extract : Module;
      M_Dyn_Insert : Module;
      M_Dyn_Insert_En : Module;
      M_Memidx : Module;
      M_Addidx : Module;
      M_Memory : Module;
      M_Memory_Init : Module;
      M_Mem_Rd : Module;
      M_Mem_Rd_Sync : Module;
      M_Mem_Wr_Sync : Module;
      M_Mem_Multiport : Module;
      M_Assert : Module;
      M_Assume : Module;
      M_Cover : Module;
      M_Assert_Cover : Module;
      M_Formal_Input : Module_Arr (Formal_Module_Id);
   end record;
end Netlists.Builders;
