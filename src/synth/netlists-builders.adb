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

with Types_Utils; use Types_Utils;
with Name_Table; use Name_Table;
with Std_Names; use Std_Names;

package body Netlists.Builders is
   function Create_Port (Id : String; Dir : Port_Kind; W : Width := 0)
                        return Port_Desc is
   begin
      return (Name => New_Sname_Artificial (Get_Identifier (Id), No_Sname),
              Dir => Dir,
              W => W);
   end Create_Port;

   function Create_Input (Id : String; W : Width := 0) return Port_Desc is
   begin
      return Create_Port (Id, Port_In, W);
   end Create_Input;

   function Create_Output (Id : String; W : Width := 0) return Port_Desc is
   begin
      return Create_Port (Id, Port_Out, W);
   end Create_Output;

   procedure Create_Dyadic_Module (Design : Module;
                                   Res : out Module;
                                   Name : Name_Id;
                                   Id : Module_Id)
   is
      Inputs : Port_Desc_Array (0 .. 1);
      Outputs : Port_Desc_Array (0 .. 0);
   begin
      Res := New_User_Module (Design, New_Sname_Artificial (Name, No_Sname),
                              Id, 2, 1, 0);
      Inputs := (0 => Create_Input ("a"),
                 1 => Create_Input ("b"));
      Outputs := (0 => Create_Output ("o"));
      Set_Ports_Desc (Res, Inputs, Outputs);
   end Create_Dyadic_Module;

   procedure Create_Monadic_Module (Design : Module;
                                    Res : out Module;
                                    Name : Name_Id;
                                    Id : Module_Id)
   is
      Inputs : Port_Desc_Array (0 .. 0);
      Outputs : Port_Desc_Array (0 .. 0);
   begin
      Res := New_User_Module (Design, New_Sname_Artificial (Name, No_Sname),
                              Id, 1, 1, 0);
      Inputs := (0 => Create_Input ("i"));
      Outputs := (0 => Create_Output ("o"));
      Set_Ports_Desc (Res, Inputs, Outputs);
   end Create_Monadic_Module;

   procedure Create_Compare_Module (Design : Module;
                                    Res : out Module;
                                    Name : Name_Id;
                                    Id : Module_Id)
   is
      Inputs : Port_Desc_Array (0 .. 1);
      Outputs : Port_Desc_Array (0 .. 0);
   begin
      Res := New_User_Module (Design, New_Sname_Artificial (Name, No_Sname),
                              Id, 2, 1, 0);
      Inputs := (0 => Create_Input ("a"),
                 1 => Create_Input ("b"));
      Outputs := (0 => Create_Output ("o", 1));
      Set_Ports_Desc (Res, Inputs, Outputs);
   end Create_Compare_Module;

   procedure Create_Concat_Modules (Ctxt : Context_Acc)
   is
      Inputs : Port_Desc_Array (0 .. 3);
      Outputs : Port_Desc_Array (0 .. 0);
      Res : Module;
   begin
      Inputs := (0 => Create_Input ("i1"),
                 1 => Create_Input ("i2"),
                 2 => Create_Input ("i3"),
                 3 => Create_Input ("i4"));
      Outputs := (0 => Create_Output ("o"));

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("concat2"), No_Sname),
         Id_Concat2, 2, 1, 0);
      Ctxt.M_Concat (Id_Concat2) := Res;
      Set_Ports_Desc (Res, Inputs (0 .. 1), Outputs);

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("concat3"), No_Sname),
         Id_Concat3, 3, 1, 0);
      Ctxt.M_Concat (Id_Concat3) := Res;
      Set_Ports_Desc (Res, Inputs (0 .. 2), Outputs);

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("concat4"), No_Sname),
         Id_Concat4, 4, 1, 0);
      Ctxt.M_Concat (Id_Concat4) := Res;
      Set_Ports_Desc (Res, Inputs (0 .. 3), Outputs);

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("concatn"), No_Sname),
         Id_Concatn, 0, 1, 1);
      Ctxt.M_Concatn := Res;
      Set_Ports_Desc (Res, Inputs (1 .. 0), Outputs);
      Set_Params_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("n"), No_Sname),
                     Typ => Param_Uns32)));
   end Create_Concat_Modules;

   procedure Create_Const_Modules (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Res : Module;
   begin
      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("const_UB32"), No_Sname),
         Id_Const_UB32, 0, 1, 1);
      Ctxt.M_Const_UB32 := Res;
      Outputs := (0 => Create_Output ("o"));
      Set_Ports_Desc (Res, Port_Desc_Array'(1 .. 0 => <>), Outputs);
      Set_Params_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("val"), No_Sname),
                     Typ => Param_Uns32)));

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("const_SB32"), No_Sname),
         Id_Const_SB32, 0, 1, 1);
      Ctxt.M_Const_SB32 := Res;
      Outputs := (0 => Create_Output ("o"));
      Set_Ports_Desc (Res, Port_Desc_Array'(1 .. 0 => <>), Outputs);
      Set_Params_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("val"), No_Sname),
                     Typ => Param_Uns32)));

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("const_UL32"), No_Sname),
         Id_Const_UL32, 0, 1, 2);
      Ctxt.M_Const_UL32 := Res;
      Set_Ports_Desc (Res, Port_Desc_Array'(1 .. 0 => <>), Outputs);
      Set_Params_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("val"), No_Sname),
                     Typ => Param_Uns32),
               1 => (New_Sname_Artificial (Get_Identifier ("xz"), No_Sname),
                     Typ => Param_Uns32)));

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("const_Z"), No_Sname),
         Id_Const_Z, 0, 1, 0);
      Ctxt.M_Const_Z := Res;
      Outputs := (0 => Create_Output ("o"));
      Set_Ports_Desc (Res, Port_Desc_Array'(1 .. 0 => <>), Outputs);

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("const_X"), No_Sname),
         Id_Const_X, 0, 1, 0);
      Ctxt.M_Const_X := Res;
      Outputs := (0 => Create_Output ("o"));
      Set_Ports_Desc (Res, Port_Desc_Array'(1 .. 0 => <>), Outputs);

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("const_bit"), No_Sname),
         Id_Const_Bit, 0, 1, 0);
      Ctxt.M_Const_Bit := Res;
      Outputs := (0 => Create_Output ("o"));
      Set_Ports_Desc (Res, Port_Desc_Array'(1 .. 0 => <>), Outputs);

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("const_log"), No_Sname),
         Id_Const_Log, 0, 1, 0);
      Ctxt.M_Const_Log := Res;
      Outputs := (0 => Create_Output ("o"));
      Set_Ports_Desc (Res, Port_Desc_Array'(1 .. 0 => <>), Outputs);
   end Create_Const_Modules;

   procedure Create_Extract_Module (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs : Port_Desc_Array (0 .. 0);
      Res : Module;
   begin
      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("extract"), No_Sname),
         Id_Extract, 1, 1, 1);
      Ctxt.M_Extract := Res;
      Outputs := (0 => Create_Output ("o"));
      Inputs := (0 => Create_Input ("i"));
      Set_Ports_Desc (Res, Inputs, Outputs);
      Set_Params_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("offset"),
                                           No_Sname),
                     Typ => Param_Uns32)));
   end Create_Extract_Module;

   procedure Create_Dyn_Extract_Module (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs : Port_Desc_Array (0 .. 1);
      Res : Module;
   begin
      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("dyn_extract"), No_Sname),
         Id_Dyn_Extract, 2, 1, 1);
      Ctxt.M_Dyn_Extract := Res;
      Outputs := (0 => Create_Output ("o"));
      Inputs := (0 => Create_Input ("v"),
                 1 => Create_Input ("i"));
      Set_Ports_Desc (Res, Inputs, Outputs);
      Set_Params_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("offset"),
                                           No_Sname),
                     Typ => Param_Uns32)));
   end Create_Dyn_Extract_Module;

   procedure Create_Dyn_Insert_Modules (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs : Port_Desc_Array (0 .. 3);
      Res : Module;
   begin
      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("dyn_insert"), No_Sname),
         Id_Dyn_Insert, 3, 1, 1);
      Ctxt.M_Dyn_Insert := Res;
      Outputs := (0 => Create_Output ("o"));
      Inputs := (0 => Create_Input ("v"),
                 1 => Create_Input ("d"),
                 2 => Create_Input ("i"),
                 3 => Create_Input ("en"));
      Set_Ports_Desc (Res, Inputs (0 .. 2), Outputs);
      Set_Params_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("offset"),
                                           No_Sname),
                     Typ => Param_Uns32)));

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("dyn_insert_en"), No_Sname),
         Id_Dyn_Insert_En, 4, 1, 1);
      Ctxt.M_Dyn_Insert_En := Res;
      Set_Ports_Desc (Res, Inputs (0 .. 3), Outputs);
      Set_Params_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("offset"),
                                           No_Sname),
                     Typ => Param_Uns32)));
   end Create_Dyn_Insert_Modules;

   procedure Create_Memidx_Module (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs : Port_Desc_Array (0 .. 1);
      Res : Module;
   begin
      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("memidx"), No_Sname),
         Id_Memidx, 1, 1, 2);
      Ctxt.M_Memidx := Res;
      Outputs := (0 => Create_Output ("o"));
      Inputs (0) := Create_Input ("i");
      Set_Ports_Desc (Res, Inputs (0 .. 0), Outputs);
      Set_Params_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("step"), No_Sname),
                     Typ => Param_Uns32),
               1 => (New_Sname_Artificial (Get_Identifier ("max"), No_Sname),
                     Typ => Param_Uns32)));
   end Create_Memidx_Module;

   procedure Create_Addidx_Module (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs : Port_Desc_Array (0 .. 1);
      Res : Module;
   begin
      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("addidx"), No_Sname),
         Id_Addidx, 2, 1, 0);
      Ctxt.M_Addidx := Res;
      Outputs := (0 => Create_Output ("o"));
      Inputs := (0 => Create_Input ("i0"),
                 1 => Create_Input ("i1"));
      Set_Ports_Desc (Res, Inputs, Outputs);
   end Create_Addidx_Module;

   procedure Create_Memory_Modules (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 1);
      Inputs : Port_Desc_Array (0 .. 4);
      Res : Module;
   begin
      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("memory"), No_Sname),
         Id_Memory, 1, 1, 0);
      Ctxt.M_Memory := Res;
      Outputs (0 .. 0) := (0 => Create_Output ("oport"));
      Inputs (0 .. 0) := (0 => Create_Input ("iport"));
      Set_Ports_Desc (Res, Inputs (0 .. 0), Outputs (0 .. 0));

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("memory_init"), No_Sname),
         Id_Memory_Init, 2, 1, 0);
      Ctxt.M_Memory_Init := Res;
      Outputs (0 .. 0) := (0 => Create_Output ("oport"));
      Inputs (0 .. 1) := (0 => Create_Input ("iport"),
                          1 => Create_Input ("init"));
      Set_Ports_Desc (Res, Inputs (0 .. 1), Outputs (0 .. 0));

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("mem_rd"), No_Sname),
         Id_Mem_Rd, 2, 2, 0);
      Ctxt.M_Mem_Rd := Res;
      Inputs (0 .. 1) := (0 => Create_Input ("iport"),
                          1 => Create_Input ("addr"));
      Outputs (0 .. 1) := (0 => Create_Output ("oport"),
                           1 => Create_Output ("data"));
      Set_Ports_Desc (Res, Inputs (0 .. 1), Outputs (0 .. 1));

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("mem_rd_sync"), No_Sname),
         Id_Mem_Rd_Sync, 4, 2, 0);
      Ctxt.M_Mem_Rd_Sync := Res;
      Inputs (0 .. 3) := (0 => Create_Input ("iport"),
                          1 => Create_Input ("addr"),
                          2 => Create_Input ("clk"),
                          3 => Create_Input ("en"));
      Outputs (0 .. 1) := (0 => Create_Output ("oport"),
                           1 => Create_Output ("data"));
      Set_Ports_Desc (Res, Inputs (0 .. 3), Outputs (0 .. 1));

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("mem_wr_sync"), No_Sname),
         Id_Mem_Wr_Sync, 5, 1, 0);
      Ctxt.M_Mem_Wr_Sync := Res;
      Inputs := (0 => Create_Input ("iport"),
                 1 => Create_Input ("addr"),
                 2 => Create_Input ("clk"),
                 3 => Create_Input ("en"),
                 4 => Create_Input ("data"));
      Outputs (0 .. 0) := (0 => Create_Output ("oport"));
      Set_Ports_Desc (Res, Inputs (0 .. 4), Outputs (0 .. 0));

      Res := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("mem_multiport"), No_Sname),
         Id_Mem_Multiport, 2, 1, 0);
      Ctxt.M_Mem_Multiport := Res;
      Inputs (0 .. 1) := (0 => Create_Input ("i0"),
                          1 => Create_Input ("i1"));
      Set_Ports_Desc (Res, Inputs (0 .. 1), Outputs (0 .. 0));
   end Create_Memory_Modules;

   procedure Create_Edge_Module (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs : Port_Desc_Array (0 .. 0);
   begin
      Ctxt.M_Posedge := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name_Posedge, No_Sname),
         Id_Posedge, 1, 1, 0);
      Inputs := (0 => Create_Input ("i", 1));
      Outputs := (0 => Create_Output ("o", 1));
      Set_Ports_Desc (Ctxt.M_Posedge, Inputs, Outputs);

      Ctxt.M_Negedge := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name_Negedge, No_Sname),
         Id_Negedge, 1, 1, 0);
      Set_Ports_Desc (Ctxt.M_Negedge, Inputs, Outputs);
   end Create_Edge_Module;

   procedure Create_Mux_Modules (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs : Port_Desc_Array (0 .. 4);
   begin
      Inputs := (0 => Create_Input ("s", 1),
                 1 => Create_Input ("i0"),
                 2 => Create_Input ("i1"),
                 3 => Create_Input ("i2"),
                 4 => Create_Input ("i3"));
      Outputs := (0 => Create_Output ("o"));

      Ctxt.M_Mux2 := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("mux2"), No_Sname),
         Id_Mux2, 3, 1, 0);
      Set_Ports_Desc (Ctxt.M_Mux2, Inputs (0 .. 2), Outputs);

      Inputs (0).W := 2;
      Ctxt.M_Mux4 := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("mux4"), No_Sname),
         Id_Mux4, 5, 1, 0);
      Set_Ports_Desc (Ctxt.M_Mux4, Inputs (0 .. 4), Outputs);

      Inputs (0).W := 0;
      Inputs (1) := Create_Input ("def");
      Ctxt.M_Pmux := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("pmux"), No_Sname),
         Id_Pmux, 2, 1, 1);
      Set_Ports_Desc (Ctxt.M_Pmux, Inputs (0 .. 1), Outputs);
      Set_Params_Desc
        (Ctxt.M_Pmux,
         (0 => (New_Sname_Artificial (Get_Identifier ("n"), No_Sname),
                Typ => Param_Uns32)));
   end Create_Mux_Modules;

   procedure Create_Objects_Module (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs2 : Port_Desc_Array (0 .. 1);
      Outputs2 : Port_Desc_Array (0 .. 1);
   begin
      Inputs2 := (0 => Create_Input ("i"),
                  1 => Create_Input ("init"));
      Outputs := (0 => Create_Output ("o"));

      Ctxt.M_Output := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name_Output, No_Sname),
         Id_Output, 1, 1, 0);
      Set_Ports_Desc (Ctxt.M_Output, Inputs2 (0 .. 0), Outputs);

      Ctxt.M_Ioutput := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("ioutput"), No_Sname),
         Id_Ioutput, 2, 1, 0);
      Set_Ports_Desc (Ctxt.M_Ioutput, Inputs2, Outputs);

      Ctxt.M_Signal := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name_Signal, No_Sname),
         Id_Signal, 1, 1, 0);
      Set_Ports_Desc (Ctxt.M_Signal, Inputs2 (0 .. 0), Outputs);

      Ctxt.M_Isignal := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("isignal"), No_Sname),
         Id_Isignal, 2, 1, 0);
      Set_Ports_Desc (Ctxt.M_Isignal, Inputs2, Outputs);

      Ctxt.M_Port := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name_Port, No_Sname),
         Id_Port, 1, 1, 0);
      Set_Ports_Desc (Ctxt.M_Port, Inputs2 (0 .. 0), Outputs);

      Ctxt.M_Nop := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("nop"), No_Sname),
         Id_Nop, 1, 1, 0);
      Set_Ports_Desc (Ctxt.M_Nop, Inputs2 (0 .. 0), Outputs);

      Ctxt.M_Enable := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("enable"), No_Sname),
         Id_Enable, 1, 1, 0);
      Set_Ports_Desc (Ctxt.M_Enable, Inputs2 (0 .. 0), Outputs);

      Ctxt.M_Inout := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name_Inout, No_Sname),
         Id_Inout, 1, 2, 0);
      Outputs2 := (0 => Outputs (0),
                   1 => Create_Output ("oport"));
      Set_Ports_Desc (Ctxt.M_Inout, Inputs2 (0 .. 0), Outputs2);

      Ctxt.M_Iinout := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier("iinout"), No_Sname),
         Id_Iinout, 2, 2, 0);
      Set_Ports_Desc (Ctxt.M_Iinout, Inputs2 (0 .. 1), Outputs2);
   end Create_Objects_Module;

   procedure Create_Dff_Modules (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
   begin
      Ctxt.M_Dff := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("dff"), No_Sname),
         Id_Dff, 2, 1, 0);
      Outputs := (0 => Create_Output ("q"));
      Set_Ports_Desc (Ctxt.M_Dff, (0 => Create_Input ("clk", 1),
                                  1 => Create_Input ("d")),
                     Outputs);

      Ctxt.M_Idff := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("idff"), No_Sname),
         Id_Idff, 3, 1, 0);
      Set_Ports_Desc (Ctxt.M_Idff, (0 => Create_Input ("clk", 1),
                                   1 => Create_Input ("d"),
                                   2 => Create_Input ("init")),
                     Outputs);


      Ctxt.M_Adff := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("adff"), No_Sname),
         Id_Adff, 4, 1, 0);
      Outputs := (0 => Create_Output ("q"));
      Set_Ports_Desc (Ctxt.M_Adff, (0 => Create_Input ("clk", 1),
                                   1 => Create_Input ("d"),
                                   2 => Create_Input ("rst", 1),
                                   3 => Create_Input ("rst_val")),
                     Outputs);

      Ctxt.M_Iadff := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("iadff"), No_Sname),
         Id_Iadff, 5, 1, 0);
      Outputs := (0 => Create_Output ("q"));
      Set_Ports_Desc (Ctxt.M_Iadff, (0 => Create_Input ("clk", 1),
                                     1 => Create_Input ("d"),
                                     2 => Create_Input ("rst"),
                                     3 => Create_Input ("rst_val"),
                                     4 => Create_Input ("init")),
                      Outputs);

      Ctxt.M_Mdff := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("mdff"), No_Sname),
         Id_Mdff, 3, 1, 0);
      Set_Ports_Desc (Ctxt.M_Mdff, (0 => Create_Input ("clk", 1),
                                    1 => Create_Input ("d"),
                                    2 => Create_Input ("els")),
                      Outputs);

      Ctxt.M_Midff := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("midff"), No_Sname),
         Id_Midff, 4, 1, 0);
      Set_Ports_Desc (Ctxt.M_Midff, (0 => Create_Input ("clk", 1),
                                     1 => Create_Input ("d"),
                                     2 => Create_Input ("els"),
                                     3 => Create_Input ("init")),
                      Outputs);
   end Create_Dff_Modules;

   procedure Create_Latch_Modules (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
   begin
      Ctxt.M_Dlatch := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("dlatch"), No_Sname),
         Id_Dlatch, 2, 1, 0);
      Outputs := (0 => Create_Output ("q"));
      Set_Ports_Desc (Ctxt.M_Dlatch, (0 => Create_Input ("d"),
                                      1 => Create_Input ("en", 1)),
                      Outputs);
   end Create_Latch_Modules;

   procedure Create_Assert_Assume_Cover (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (1 .. 0);
   begin
      Ctxt.M_Assert := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name_Assert, No_Sname), Id_Assert,
         1, 0, 0);
      Set_Ports_Desc (Ctxt.M_Assert, (0 => Create_Input ("cond", 1)),
                     Outputs);

      Ctxt.M_Assume := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name_Assume, No_Sname), Id_Assume,
         1, 0, 0);
      Set_Ports_Desc (Ctxt.M_Assume, (0 => Create_Input ("cond", 1)),
                     Outputs);

      Ctxt.M_Cover := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name_Cover, No_Sname), Id_Cover,
         1, 0, 0);
      Set_Ports_Desc (Ctxt.M_Cover, (0 => Create_Input ("cond", 1)),
                     Outputs);

      Ctxt.M_Assert_Cover := New_User_Module
        (Ctxt.Design,
         New_Sname_Artificial (Get_Identifier ("assert_cover"), No_Sname),
         Id_Assert_Cover, 1, 0, 0);
      Set_Ports_Desc (Ctxt.M_Assert_Cover, (0 => Create_Input ("cond", 1)),
                     Outputs);
   end Create_Assert_Assume_Cover;

   procedure Create_Formal_Input
     (Ctxt : Context_Acc; Id : Formal_Module_Id; Name : Name_Id)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Res : Module;
   begin
      Res := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name, No_Sname), Id, 0, 1, 0);
      Ctxt.M_Formal_Input (Id) := Res;
      Outputs := (0 => Create_Output ("o"));
      Set_Ports_Desc (Res, Port_Desc_Array'(1 .. 0 => <>), Outputs);
   end Create_Formal_Input;

   procedure Create_Tri_Module (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs : Port_Desc_Array (0 .. 1);
      Res : Module;
   begin
      Res := New_User_Module (Ctxt.Design,
                              New_Sname_Artificial (Name_Tri, No_Sname),
                              Id_Tri, 2, 1, 0);
      Ctxt.M_Tri := Res;
      Outputs := (0 => Create_Output ("o"));
      Inputs := (0 => Create_Input ("en"),
                 1 => Create_Input ("i"));
      Set_Ports_Desc (Res, Inputs, Outputs);
   end Create_Tri_Module;

   function Build_Builders (Design : Module) return Context_Acc
   is
      Res : Context_Acc;
   begin
      Res := new Context'(Design => Design,
                          Parent => No_Module,
                          Num => 0,
                          M_Dyadic => (others => No_Module),
                          M_Shift_Rotate => (others => No_Module),
                          M_Monadic => (others => No_Module),
                          M_Compare => (others => No_Module),
                          M_Concat => (others => No_Module),
                          M_Truncate | M_Extend => (others => No_Module),
                          M_Reduce => (others => No_Module),
                          M_Formal_Input => (others => No_Module),
                          others => No_Module);

      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_And), Name_And, Id_And);
      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Or), Name_Or, Id_Or);
      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Xor), Name_Xor, Id_Xor);

      Create_Dyadic_Module
        (Design, Res.M_Dyadic (Id_Nand), Name_Nand, Id_Nand);
      Create_Dyadic_Module
        (Design, Res.M_Dyadic (Id_Nor),  Name_Nor,  Id_Nor);
      Create_Dyadic_Module
        (Design, Res.M_Dyadic (Id_Xnor), Name_Xnor, Id_Xnor);

      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Add),
                            Get_Identifier ("add"), Id_Add);
      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Sub),
                            Get_Identifier ("sub"), Id_Sub);

      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Umin),
                            Get_Identifier ("umin"), Id_Umin);
      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Smin),
                            Get_Identifier ("smin"), Id_Smin);
      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Umax),
                            Get_Identifier ("umax"), Id_Umax);
      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Smax),
                            Get_Identifier ("smax"), Id_Smax);

      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Umul),
                            Get_Identifier ("umul"), Id_Umul);
      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Smul),
                            Get_Identifier ("smul"), Id_Smul);
      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Udiv),
                            Get_Identifier ("udiv"), Id_Udiv);
      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Sdiv),
                            Get_Identifier ("sdiv"), Id_Sdiv);

      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Umod),
                            Get_Identifier ("umod"), Id_Umod);
      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Smod),
                            Get_Identifier ("smod"), Id_Smod);

      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Srem),
                            Get_Identifier ("srem"), Id_Srem);

      Create_Dyadic_Module (Design, Res.M_Shift_Rotate (Id_Lsl),
                            Get_Identifier ("lsl"), Id_Lsl);
      Create_Dyadic_Module (Design, Res.M_Shift_Rotate (Id_Lsr),
                            Get_Identifier ("lsr"), Id_Lsr);
      Create_Dyadic_Module (Design, Res.M_Shift_Rotate (Id_Asr),
                            Get_Identifier ("asr"), Id_Asr);
      Create_Dyadic_Module (Design, Res.M_Shift_Rotate (Id_Rol),
                            Name_Rol, Id_Rol);
      Create_Dyadic_Module (Design, Res.M_Shift_Rotate (Id_Ror),
                            Name_Ror, Id_Ror);

      Create_Monadic_Module (Design, Res.M_Monadic (Id_Not), Name_Not, Id_Not);
      Create_Monadic_Module (Design, Res.M_Monadic (Id_Neg),
                             Get_Identifier ("neg"), Id_Neg);
      Create_Monadic_Module (Design, Res.M_Monadic (Id_Abs), Name_Abs, Id_Abs);

      Create_Compare_Module (Design, Res.M_Compare (Id_Eq),
                             Get_Identifier ("eq"), Id_Eq);
      Create_Compare_Module (Design, Res.M_Compare (Id_Ne),
                             Get_Identifier ("ne"), Id_Ne);

      Create_Compare_Module (Design, Res.M_Compare (Id_Ult),
                             Get_Identifier ("ult"), Id_Ult);
      Create_Compare_Module (Design, Res.M_Compare (Id_Ule),
                             Get_Identifier ("ule"), Id_Ule);

      Create_Compare_Module (Design, Res.M_Compare (Id_Ugt),
                             Get_Identifier ("ugt"), Id_Ugt);
      Create_Compare_Module (Design, Res.M_Compare (Id_Uge),
                             Get_Identifier ("uge"), Id_Uge);

      Create_Compare_Module (Design, Res.M_Compare (Id_Slt),
                             Get_Identifier ("slt"), Id_Slt);
      Create_Compare_Module (Design, Res.M_Compare (Id_Sle),
                             Get_Identifier ("sle"), Id_Sle);

      Create_Compare_Module (Design, Res.M_Compare (Id_Sgt),
                             Get_Identifier ("sgt"), Id_Sgt);
      Create_Compare_Module (Design, Res.M_Compare (Id_Sge),
                             Get_Identifier ("sge"), Id_Sge);

      Create_Concat_Modules (Res);
      Create_Const_Modules (Res);

      Create_Extract_Module (Res);
      Create_Dyn_Extract_Module (Res);
      Create_Dyn_Insert_Modules (Res);
      Create_Memidx_Module (Res);
      Create_Addidx_Module (Res);

      Create_Memory_Modules (Res);

      Create_Monadic_Module (Design, Res.M_Truncate (Id_Utrunc),
                             Get_Identifier ("utrunc"), Id_Utrunc);
      Create_Monadic_Module (Design, Res.M_Truncate (Id_Strunc),
                             Get_Identifier ("strunc"), Id_Strunc);

      Create_Monadic_Module (Design, Res.M_Extend (Id_Uextend),
                             Get_Identifier ("uextend"), Id_Uextend);
      Create_Monadic_Module (Design, Res.M_Extend (Id_Sextend),
                             Get_Identifier ("sextend"), Id_Sextend);

      Create_Monadic_Module (Design, Res.M_Reduce (Id_Red_Or),
                             Get_Identifier ("red_or"), Id_Red_Or);
      Create_Monadic_Module (Design, Res.M_Reduce (Id_Red_And),
                             Get_Identifier ("red_and"), Id_Red_And);
      Create_Monadic_Module (Design, Res.M_Reduce (Id_Red_Xor),
                             Get_Identifier ("red_xor"), Id_Red_Xor);

      Create_Edge_Module (Res);

      Create_Mux_Modules (Res);
      Create_Objects_Module (Res);
      Create_Dff_Modules (Res);
      Create_Latch_Modules (Res);

      Create_Assert_Assume_Cover (Res);

      Create_Formal_Input (Res, Id_Allconst, Name_Allconst);
      Create_Formal_Input (Res, Id_Anyconst, Name_Anyconst);
      Create_Formal_Input (Res, Id_Allseq, Name_Allseq);
      Create_Formal_Input (Res, Id_Anyseq, Name_Anyseq);

      Create_Tri_Module (Res);
      Create_Dyadic_Module (Design, Res.M_Resolver,
                            Get_Identifier ("resolver"), Id_Resolver);

      return Res;
   end Build_Builders;

   procedure Set_Parent (Ctxt : Context_Acc; Parent : Module) is
   begin
      Ctxt.Parent := Parent;
   end Set_Parent;

   function Get_Design (Ctxt : Context_Acc) return Module is
   begin
      return Ctxt.Design;
   end Get_Design;

   function New_Internal_Name (Ctxt : Context_Acc; Prefix : Sname := No_Sname)
                              return Sname
   is
      Name : Sname;
   begin
      Name := New_Sname_Version (Ctxt.Num, Prefix);
      Ctxt.Num := Ctxt.Num + 1;
      return Name;
   end New_Internal_Name;

   function New_Internal_Instance (Ctxt : Context_Acc; M : Module)
                                  return Instance
   is
      pragma Assert (M /= No_Module);
   begin
      return New_Instance (Ctxt.Parent, M, New_Internal_Name (Ctxt));
   end New_Internal_Instance;

   function Build_Dyadic (Ctxt : Context_Acc;
                          Id : Dyadic_Module_Id;
                          L, R : Net) return Net
   is
      Wd : constant Width := Get_Width (L);
      pragma Assert (Get_Width (R) = Wd);
      pragma Assert (Ctxt.M_Dyadic (Id) /= No_Module);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Dyadic (Id));
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), L);
      Connect (Get_Input (Inst, 1), R);
      return O;
   end Build_Dyadic;

   function Build_Shift_Rotate (Ctxt : Context_Acc;
                                Id : Shift_Rotate_Module_Id;
                                L, R : Net) return Net
   is
      Wd : constant Width := Get_Width (L);
      pragma Assert (Wd /= No_Width);
      pragma Assert (Get_Width (R) /= No_Width);
      pragma Assert (Ctxt.M_Shift_Rotate (Id) /= No_Module);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Shift_Rotate (Id));
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), L);
      Connect (Get_Input (Inst, 1), R);
      return O;
   end Build_Shift_Rotate;

   function Build_Monadic (Ctxt : Context_Acc;
                           Id : Monadic_Module_Id;
                           Op : Net) return Net
   is
      pragma Assert (Ctxt.M_Monadic (Id) /= No_Module);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Monadic (Id));
      O := Get_Output (Inst, 0);
      Set_Width (O, Get_Width (Op));
      Connect (Get_Input (Inst, 0), Op);
      return O;
   end Build_Monadic;

   function Build_Reduce (Ctxt : Context_Acc;
                          Id : Reduce_Module_Id;
                          Op : Net) return Net
   is
      pragma Assert (Ctxt.M_Reduce (Id) /= No_Module);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Reduce (Id));
      O := Get_Output (Inst, 0);
      Set_Width (O, 1);
      Connect (Get_Input (Inst, 0), Op);
      return O;
   end Build_Reduce;

   function Build_Compare (Ctxt : Context_Acc;
                           Id : Compare_Module_Id;
                           L, R : Net) return Net
   is
      Wd : constant Width := Get_Width (L);
      pragma Assert (Wd /= No_Width);
      pragma Assert (Get_Width (R) = Wd);
      pragma Assert (Ctxt.M_Compare (Id) /= No_Module);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Compare (Id));
      O := Get_Output (Inst, 0);
      Connect (Get_Input (Inst, 0), L);
      Connect (Get_Input (Inst, 1), R);
      return O;
   end Build_Compare;

   function Build_Const_X (Ctxt : Context_Acc; W : Width) return Net
   is
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Const_X);
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      return O;
   end Build_Const_X;

   function Build_Const_Z (Ctxt : Context_Acc; W : Width) return Net
   is
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Const_Z);
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      return O;
   end Build_Const_Z;

   function Build_Const_UB32 (Ctxt : Context_Acc;
                              Val : Uns32;
                              W : Width) return Net
   is
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Const_UB32);
      O := Get_Output (Inst, 0);
      Set_Param_Uns32 (Inst, 0, Val);
      Set_Width (O, W);
      return O;
   end Build_Const_UB32;

   function Build_Const_SB32 (Ctxt : Context_Acc;
                              Val : Int32;
                              W : Width) return Net
   is
      pragma Assert (W > 0);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Const_SB32);
      O := Get_Output (Inst, 0);
      Set_Param_Uns32 (Inst, 0, To_Uns32 (Val));
      Set_Width (O, W);
      return O;
   end Build_Const_SB32;

   function Build_Const_UL32 (Ctxt : Context_Acc;
                              Val : Uns32;
                              Xz : Uns32;
                              W : Width) return Net
   is
      pragma Assert (W > 0 and W <= 32);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Const_UL32);
      O := Get_Output (Inst, 0);
      Set_Param_Uns32 (Inst, 0, Val);
      Set_Param_Uns32 (Inst, 1, Xz);
      Set_Width (O, W);
      return O;
   end Build_Const_UL32;

   function Build_Const_Bit (Ctxt : Context_Acc; W : Width)
                            return Instance
   is
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Var_Instance (Ctxt.Parent, Ctxt.M_Const_Bit,
                                New_Internal_Name (Ctxt),
                                0, 1, Param_Idx ((W + 31) / 32));
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      return Inst;
   end Build_Const_Bit;

   function Build_Const_Log (Ctxt : Context_Acc; W : Width)
                            return Instance
   is
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Var_Instance (Ctxt.Parent, Ctxt.M_Const_Log,
                                New_Internal_Name (Ctxt),
                                0, 1, 2 * Param_Idx ((W + 31) / 32));
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      return Inst;
   end Build_Const_Log;

   function Build_Edge (Ctxt : Context_Acc; M : Module; Src : Net) return Net
   is
      pragma Assert (Get_Width (Src) = 1);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, M);
      O := Get_Output (Inst, 0);
      pragma Assert (Get_Width (O) = 1);
      Connect (Get_Input (Inst, 0), Src);
      return O;
   end Build_Edge;

   pragma Inline (Build_Edge);

   function Build_Posedge (Ctxt : Context_Acc; Src : Net) return Net is
   begin
      return Build_Edge (Ctxt, Ctxt.M_Posedge, Src);
   end Build_Posedge;

   function Build_Negedge (Ctxt : Context_Acc; Src : Net) return Net is
   begin
      return Build_Edge (Ctxt, Ctxt.M_Negedge, Src);
   end Build_Negedge;

   function Build_Mux2 (Ctxt : Context_Acc;
                        Sel : Net;
                        I0, I1 : Net) return Net
   is
      Wd : constant Width := Get_Width (I1);
      pragma Assert (I0 = No_Net or else Get_Width (I0) = Wd);
      pragma Assert (Get_Width (Sel) = 1);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Mux2);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), Sel);
      if I0 /= No_Net then
         Connect (Get_Input (Inst, 1), I0);
      end if;
      Connect (Get_Input (Inst, 2), I1);
      return O;
   end Build_Mux2;

   function Build_Mux4 (Ctxt : Context_Acc;
                        Sel : Net;
                        I0, I1, I2, I3 : Net) return Net
   is
      Wd : constant Width := Get_Width (I0);
      pragma Assert (Get_Width (I1) = Wd);
      pragma Assert (Get_Width (I2) = Wd);
      pragma Assert (Get_Width (I3) = Wd);
      pragma Assert (Get_Width (Sel) = 2);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Mux4);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), Sel);
      Connect (Get_Input (Inst, 1), I0);
      Connect (Get_Input (Inst, 2), I1);
      Connect (Get_Input (Inst, 3), I2);
      Connect (Get_Input (Inst, 4), I3);
      return O;
   end Build_Mux4;

   function Build_Pmux (Ctxt : Context_Acc; Sel : Net; Def : Net) return Net
   is
      Sel_W : constant Width := Get_Width (Sel);
      Def_W : constant Width := Get_Width (Def);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Var_Instance (Ctxt.Parent, Ctxt.M_Pmux,
                                New_Internal_Name (Ctxt),
                                2 + Port_Nbr (Sel_W), 1, 1);
      Set_Param_Uns32 (Inst, 0, 2 + Sel_W);
      O := Get_Output (Inst, 0);
      Set_Width (O, Def_W);
      Connect (Get_Input (Inst, 0), Sel);
      Connect (Get_Input (Inst, 1), Def);
      return O;
   end Build_Pmux;

   function Build_Concat2 (Ctxt : Context_Acc; I0, I1 : Net) return Net
   is
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Concat (Id_Concat2));
      O := Get_Output (Inst, 0);
      Set_Width (O, Get_Width (I0) + Get_Width (I1));
      Connect (Get_Input (Inst, 0), I0);
      Connect (Get_Input (Inst, 1), I1);
      return O;
   end Build_Concat2;

   function Build_Concat3 (Ctxt : Context_Acc; I0, I1, I2 : Net) return Net
   is
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Concat (Id_Concat3));
      O := Get_Output (Inst, 0);
      Set_Width (O, Get_Width (I0) + Get_Width (I1) + Get_Width (I2));
      Connect (Get_Input (Inst, 0), I0);
      Connect (Get_Input (Inst, 1), I1);
      Connect (Get_Input (Inst, 2), I2);
      return O;
   end Build_Concat3;

   function Build_Concat4 (Ctxt : Context_Acc; I0, I1, I2, I3 : Net)
                          return Net
   is
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Concat (Id_Concat4));
      O := Get_Output (Inst, 0);
      Set_Width (O, Get_Width (I0) + Get_Width (I1)
                   + Get_Width (I2) + Get_Width (I3));
      Connect (Get_Input (Inst, 0), I0);
      Connect (Get_Input (Inst, 1), I1);
      Connect (Get_Input (Inst, 2), I2);
      Connect (Get_Input (Inst, 3), I3);
      return O;
   end Build_Concat4;

   function Build_Concatn (Ctxt : Context_Acc; W : Width; Nbr_Inputs : Uns32)
                          return Net
   is
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Var_Instance (Ctxt.Parent, Ctxt.M_Concatn,
                                New_Internal_Name (Ctxt),
                                Port_Nbr (Nbr_Inputs), 1, 1);
      Set_Param_Uns32 (Inst, 0, Nbr_Inputs);
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      return O;
   end Build_Concatn;

   function Build_Trunc
     (Ctxt : Context_Acc; Id : Module_Id; I : Net; W : Width) return Net
   is
      pragma Assert (Get_Width (I) > W);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Truncate (Id));
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      Connect (Get_Input (Inst, 0), I);
      return O;
   end Build_Trunc;

   function Build_Extend
     (Ctxt : Context_Acc; Id : Module_Id; I : Net; W : Width) return Net
   is
      pragma Assert (Get_Width (I) < W);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Extend (Id));
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      Connect (Get_Input (Inst, 0), I);
      return O;
   end Build_Extend;

   function Build_Dyn_Insert
     (Ctxt : Context_Acc; Mem : Net; V : Net; Idx : Net; Off : Uns32)
     return Net
   is
      Wd : constant Width := Get_Width (Mem);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Dyn_Insert);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), Mem);
      if V /= No_Net then
         Connect (Get_Input (Inst, 1), V);
      end if;
      Connect (Get_Input (Inst, 2), Idx);
      Set_Param_Uns32 (Inst, 0, Off);
      return O;
   end Build_Dyn_Insert;

   function Build_Dyn_Insert_En
     (Ctxt : Context_Acc; Mem : Net; V : Net; Idx : Net; En : Net; Off : Uns32)
     return Net
   is
      Wd : constant Width := Get_Width (Mem);
      pragma Assert (Wd /= No_Width);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Dyn_Insert_En);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), Mem);
      if V /= No_Net then
         Connect (Get_Input (Inst, 1), V);
      end if;
      Connect (Get_Input (Inst, 2), Idx);
      Connect (Get_Input (Inst, 3), En);
      Set_Param_Uns32 (Inst, 0, Off);
      return O;
   end Build_Dyn_Insert_En;

   function Build_Memidx
     (Ctxt : Context_Acc;
      I : Net; Step : Uns32; Max : Uns32; W : Width) return Net
   is
      --  Note: step could be 0.
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Memidx);
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      Connect (Get_Input (Inst, 0), I);
      Set_Param_Uns32 (Inst, 0, Step);
      Set_Param_Uns32 (Inst, 1, Max);
      return O;
   end Build_Memidx;

   function Build_Addidx (Ctxt : Context_Acc; L, R : Net) return Net
   is
      Wl : constant Width := Get_Width (L);
      Wr : constant Width := Get_Width (R);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Addidx);
      O := Get_Output (Inst, 0);
      Set_Width (O, Width'Max (Wl, Wr));
      Connect (Get_Input (Inst, 0), L);
      Connect (Get_Input (Inst, 1), R);
      return O;
   end Build_Addidx;

   function Build_Memory
     (Ctxt : Context_Acc; Name : Sname; W : Width) return Instance
   is
      pragma Assert (W > 0);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Instance (Ctxt.Parent, Ctxt.M_Memory, Name);
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      return Inst;
   end Build_Memory;

   function Build_Memory_Init
     (Ctxt : Context_Acc; Name : Sname; W : Width; Init : Net) return Instance
   is
      pragma Assert (W > 0);
      pragma Assert (Get_Width (Init) = W);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Instance (Ctxt.Parent, Ctxt.M_Memory_Init, Name);
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      Connect (Get_Input (Inst, 1), Init);
      return Inst;
   end Build_Memory_Init;

   function Build_Mem_Rd
     (Ctxt : Context_Acc; Pport : Net; Addr : Net; Data_W : Width)
     return Instance
   is
      Mem_W : constant Width := Get_Width (Pport);
      pragma Assert (Mem_W > 0);
      Addr_W : constant Width := Get_Width (Addr);
      pragma Assert (Addr_W > 0);
      pragma Assert (Data_W * (2**Natural(Addr_W)) >= Mem_W);
      Inst : Instance;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Mem_Rd);
      Set_Width (Get_Output (Inst, 0), Mem_W);
      Set_Width (Get_Output (Inst, 1), Data_W);
      Connect (Get_Input (Inst, 0), Pport);
      Connect (Get_Input (Inst, 1), Addr);
      return Inst;
   end Build_Mem_Rd;

   function Build_Mem_Rd_Sync (Ctxt : Context_Acc;
                               Pport : Net;
                               Addr : Net;
                               Clk : Net;
                               En : Net;
                               Data_W : Width)
                              return Instance
   is
      Mem_W : constant Width := Get_Width (Pport);
      pragma Assert (Mem_W > 0);
      Addr_W : constant Width := Get_Width (Addr);
      pragma Assert (Addr_W > 0);
      pragma Assert (Data_W * (2**Natural(Addr_W)) >= Mem_W);
      pragma Assert (Get_Width (Clk) = 1);
      pragma Assert (Get_Width (En) = 1);
      Inst : Instance;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Mem_Rd_Sync);
      Set_Width (Get_Output (Inst, 0), Mem_W);
      Set_Width (Get_Output (Inst, 1), Data_W);
      Connect (Get_Input (Inst, 0), Pport);
      Connect (Get_Input (Inst, 1), Addr);
      Connect (Get_Input (Inst, 2), Clk);
      Connect (Get_Input (Inst, 3), En);
      return Inst;
   end Build_Mem_Rd_Sync;

   function Build_Mem_Wr_Sync (Ctxt : Context_Acc;
                               Pport : Net;
                               Addr : Net;
                               Clk : Net;
                               En : Net;
                               Data : Net) return Instance
   is
      Mem_W : constant Width := Get_Width (Pport);
      pragma Assert (Mem_W > 0);
      Addr_W : constant Width := Get_Width (Addr);
      pragma Assert (Addr_W > 0);
      Data_W : constant Width := Get_Width (Data);
      pragma Assert (Data_W * (2**Natural(Addr_W)) >= Mem_W);
      --  Assertions currently disabled as netlists-memories create
      --  mem_wr_sync without clk/en (and connect them later).
      --  pragma Assert (Get_Width (Clk) = 1);
      --  pragma Assert (Get_Width (En) = 1);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Mem_Wr_Sync);
      O := Get_Output (Inst, 0);
      Set_Width (O, Mem_W);
      Connect (Get_Input (Inst, 0), Pport);
      Connect (Get_Input (Inst, 1), Addr);
      if Clk /= No_Net then
         Connect (Get_Input (Inst, 2), Clk);
      end if;
      if En /= No_Net then
         Connect (Get_Input (Inst, 3), En);
      end if;
      Connect (Get_Input (Inst, 4), Data);
      return Inst;
   end Build_Mem_Wr_Sync;

   function Build_Mem_Multiport (Ctxt : Context_Acc; I0, I1 : Net) return Net
   is
      W : constant Width := Get_Width (I0);
      pragma Assert (Get_Width (I1) = W);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Mem_Multiport);
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      Connect (Get_Input (Inst, 0), I0);
      Connect (Get_Input (Inst, 1), I1);
      return O;
   end Build_Mem_Multiport;

   function Build_Object (Ctxt : Context_Acc; M : Module; W : Width) return Net
   is
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, M);
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      return O;
   end Build_Object;

   function Build_Output (Ctxt : Context_Acc; W : Width) return Net is
   begin
      return Build_Object (Ctxt, Ctxt.M_Output, W);
   end Build_Output;

   function Build_Inout_Object (Ctxt : Context_Acc; M : Module; W : Width)
                                return Instance
   is
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, M);
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      O := Get_Output (Inst, 1);
      Set_Width (O, W);
      return Inst;
   end Build_Inout_Object;

   function Build_Inout (Ctxt : Context_Acc; W : Width) return Instance is
   begin
      return Build_Inout_Object (Ctxt, Ctxt.M_Inout, W);
   end Build_Inout;

   function Build_Iinout (Ctxt : Context_Acc; W : Width) return Instance is
   begin
      return Build_Inout_Object (Ctxt, Ctxt.M_Iinout, W);
   end Build_Iinout;

   function Build_Ioutput (Ctxt : Context_Acc; Init : Net) return Net
   is
      Wd : constant Width := Get_Width (Init);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Ioutput);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 1), Init);
      return O;
   end Build_Ioutput;

   function Build_Signal (Ctxt : Context_Acc; Name : Sname; W : Width)
                         return Net
   is
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Instance (Ctxt.Parent, Ctxt.M_Signal, Name);
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      return O;
   end Build_Signal;

   function Build_Isignal (Ctxt : Context_Acc; Name : Sname; Init : Net)
                          return Net
   is
      Wd : constant Width := Get_Width (Init);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Instance (Ctxt.Parent, Ctxt.M_Isignal, Name);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 1), Init);
      return O;
   end Build_Isignal;

   function Build_Port (Ctxt : Context_Acc; N : Net) return Net
   is
      Wd : constant Width := Get_Width (N);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Port);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), N);
      return O;
   end Build_Port;

   function Build_Nop (Ctxt : Context_Acc; I : Net) return Net
   is
      Wd : constant Width := Get_Width (I);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Nop);
      Connect (Get_Input (Inst, 0), I);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      return O;
   end Build_Nop;

   function Build_Enable (Ctxt : Context_Acc) return Net
   is
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Enable);
      O := Get_Output (Inst, 0);
      Set_Width (O, 1);
      return O;
   end Build_Enable;

   function Build_Dff (Ctxt : Context_Acc;
                       Clk : Net;
                       D : Net) return Net
   is
      Wd : constant Width := Get_Width (D);
      pragma Assert (Get_Width (Clk) = 1);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Dff);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), Clk);
      Connect (Get_Input (Inst, 1), D);
      return O;
   end Build_Dff;

   function Build_Idff (Ctxt : Context_Acc;
                        Clk : Net;
                        D : Net;
                        Init : Net) return Net
   is
      Wd : constant Width := Get_Width (Init);
      pragma Assert (D = No_Net or else Get_Width (D) = Wd);
      pragma Assert (Get_Width (Clk) = 1);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Idff);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), Clk);
      if D /= No_Net then
         Connect (Get_Input (Inst, 1), D);
      end if;
      Connect (Get_Input (Inst, 2), Init);
      return O;
   end Build_Idff;

   function Build_Adff (Ctxt : Context_Acc;
                        Clk : Net;
                        D : Net;
                        Rst : Net; Rst_Val : Net) return Net
   is
      Wd : constant Width := Get_Width (D);
      pragma Assert (Get_Width (Clk) = 1);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Adff);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), Clk);
      Connect (Get_Input (Inst, 1), D);
      Connect (Get_Input (Inst, 2), Rst);
      Connect (Get_Input (Inst, 3), Rst_Val);
      return O;
   end Build_Adff;

   function Build_Iadff (Ctxt : Context_Acc;
                         Clk : Net;
                         D : Net;
                         Rst : Net; Rst_Val : Net; Init : Net) return Net
   is
      Wd : constant Width := Get_Width (Init);
      pragma Assert (Get_Width (Clk) = 1);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Iadff);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), Clk);
      if D /= No_Net then
         Connect (Get_Input (Inst, 1), D);
      end if;
      Connect (Get_Input (Inst, 2), Rst);
      Connect (Get_Input (Inst, 3), Rst_Val);
      Connect (Get_Input (Inst, 4), Init);
      return O;
   end Build_Iadff;

   function Build_Mdff (Ctxt : Context_Acc;
                        Clk : Net;
                        D : Net;
                        Els : Net) return Net
   is
      Wd : constant Width := Get_Width (D);
      pragma Assert (Get_Width (Clk) = 1);
      pragma Assert (Get_Width (Els) = Wd);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Mdff);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), Clk);
      Connect (Get_Input (Inst, 1), D);
      Connect (Get_Input (Inst, 2), Els);
      return O;
   end Build_Mdff;

   function Build_Midff (Ctxt : Context_Acc;
                         Clk : Net;
                         D : Net;
                         Els : Net;
                         Init : Net) return Net
   is
      Wd : constant Width := Get_Width (D);
      pragma Assert (Get_Width (Clk) = 1);
      pragma Assert (Get_Width (Els) = Wd);
      pragma Assert (Get_Width (Init) = Wd);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Midff);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), Clk);
      Connect (Get_Input (Inst, 1), D);
      Connect (Get_Input (Inst, 2), Els);
      Connect (Get_Input (Inst, 3), Init);
      return O;
   end Build_Midff;

   function Build_Dlatch (Ctxt : Context_Acc; En : Net; D : Net) return Net
   is
      Wd : constant Width := Get_Width (D);
      pragma Assert (Get_Width (En) = 1);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Dlatch);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), D);
      Connect (Get_Input (Inst, 1), En);
      return O;
   end Build_Dlatch;

   function Build_Tri (Ctxt : Context_Acc; En : Net; D : Net) return Net
   is
      Wd : constant Width := Get_Width (D);
      pragma Assert (Get_Width (En) = 1);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Tri);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), En);
      Connect (Get_Input (Inst, 1), D);
      return O;
   end Build_Tri;

   --  Reuse Build_Dyadic ?
   function Build_Resolver (Ctxt : Context_Acc; L, R : Net) return Net
   is
      Wd : constant Width := Get_Width (L);
      pragma Assert (Get_Width (R) = Wd);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Resolver);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), L);
      Connect (Get_Input (Inst, 1), R);
      return O;
   end Build_Resolver;

   function Build_Extract
     (Ctxt : Context_Acc; I : Net; Off, W : Width) return Net
   is
      Wd : constant Width := Get_Width (I);
      pragma Assert (W + Off <= Wd);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Extract);
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      Connect (Get_Input (Inst, 0), I);
      Set_Param_Uns32 (Inst, 0, Off);
      return O;
   end Build_Extract;

   function Build_Dyn_Extract
     (Ctxt : Context_Acc; Mem : Net; Idx : Net; Off : Uns32; W : Width)
     return Net
   is
      Wd : constant Width := Get_Width (Mem);
      pragma Assert (Wd /= No_Width);
      pragma Assert (W > 0);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Dyn_Extract);
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      Connect (Get_Input (Inst, 0), Mem);
      Connect (Get_Input (Inst, 1), Idx);
      Set_Param_Uns32 (Inst, 0, Off);
      return O;
   end Build_Dyn_Extract;

   function Build_Extract_Bit
     (Ctxt : Context_Acc; I : Net; Off : Width) return Net is
   begin
      return Build_Extract (Ctxt, I, Off, 1);
   end Build_Extract_Bit;

   function Name_Or_Internal (Name : Sname; Ctxt : Context_Acc) return Sname is
   begin
      if Name = No_Sname then
         return New_Internal_Name (Ctxt);
      else
         return Name;
      end if;
   end Name_Or_Internal;

   function Build_Formal
     (Ctxt : Context_Acc; Gate : Module; Name : Sname; Cond : Net)
     return Instance
   is
      Inst : Instance;
   begin
      Inst := New_Instance (Ctxt.Parent, Gate,
                            Name_Or_Internal (Name, Ctxt));
      Connect (Get_Input (Inst, 0), Cond);
      return Inst;
   end Build_Formal;

   function Build_Assert (Ctxt : Context_Acc; Name : Sname; Cond : Net)
                         return Instance is
   begin
      return Build_Formal (Ctxt, Ctxt.M_Assert, Name, Cond);
   end Build_Assert;

   function Build_Assume (Ctxt : Context_Acc; Name : Sname; Cond : Net)
                         return Instance is
   begin
      return Build_Formal (Ctxt, Ctxt.M_Assume, Name, Cond);
   end Build_Assume;

   function Build_Cover (Ctxt : Context_Acc; Name : Sname; Cond : Net)
                         return Instance is
   begin
      return Build_Formal (Ctxt, Ctxt.M_Cover, Name, Cond);
   end Build_Cover;

   function Build_Assert_Cover (Ctxt : Context_Acc; Name : Sname; Cond : Net)
                         return Instance is
   begin
      return Build_Formal (Ctxt, Ctxt.M_Assert_Cover, Name, Cond);
   end Build_Assert_Cover;

   function Build_Formal_Input
     (Ctxt : Context_Acc; Id : Formal_Module_Id; W : Width) return Net
   is
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Instance (Ctxt.Parent, Ctxt.M_Formal_Input (Id),
                            New_Internal_Name (Ctxt));
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      return O;
   end Build_Formal_Input;
end Netlists.Builders;
