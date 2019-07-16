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

with Types_Utils; use Types_Utils;
with Name_Table; use Name_Table;
with Std_Names; use Std_Names;

package body Netlists.Builders is
   function Create_Input (Id : String; W : Width := 0) return Port_Desc is
   begin
      return (Name => New_Sname_Artificial (Get_Identifier (Id)),
              W => W,
              Dir => Port_In,
              Left | Right => 0);
   end Create_Input;

   function Create_Output (Id : String; W : Width := 0) return Port_Desc is
   begin
      return (Name => New_Sname_Artificial (Get_Identifier (Id)),
              W => W,
              Dir => Port_Out,
              Left | Right => 0);
   end Create_Output;

   procedure Create_Dyadic_Module (Design : Module;
                                   Res : out Module;
                                   Name : Name_Id;
                                   Id : Module_Id)
   is
      Inputs : Port_Desc_Array (0 .. 1);
      Outputs : Port_Desc_Array (0 .. 0);
   begin
      Res := New_User_Module (Design, New_Sname_Artificial (Name),
                              Id, 2, 1, 0);
      Inputs := (0 => Create_Input ("a"),
                 1 => Create_Input ("b"));
      Outputs := (0 => Create_Output ("o"));
      Set_Port_Desc (Res, Inputs, Outputs);
   end Create_Dyadic_Module;

   procedure Create_Monadic_Module (Design : Module;
                                    Res : out Module;
                                    Name : Name_Id;
                                    Id : Module_Id)
   is
      Inputs : Port_Desc_Array (0 .. 0);
      Outputs : Port_Desc_Array (0 .. 0);
   begin
      Res := New_User_Module (Design, New_Sname_Artificial (Name),
                              Id, 1, 1, 0);
      Inputs := (0 => Create_Input ("i"));
      Outputs := (0 => Create_Output ("o"));
      Set_Port_Desc (Res, Inputs, Outputs);
   end Create_Monadic_Module;

   procedure Create_Compare_Module (Design : Module;
                                    Res : out Module;
                                    Name : Name_Id;
                                    Id : Module_Id)
   is
      Inputs : Port_Desc_Array (0 .. 1);
      Outputs : Port_Desc_Array (0 .. 0);
   begin
      Res := New_User_Module (Design, New_Sname_Artificial (Name),
                              Id, 2, 1, 0);
      Inputs := (0 => Create_Input ("a"),
                 1 => Create_Input ("b"));
      Outputs := (0 => Create_Output ("o", 1));
      Set_Port_Desc (Res, Inputs, Outputs);
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
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("concat2")),
         Id_Concat2, 2, 1, 0);
      Ctxt.M_Concat (Id_Concat2) := Res;
      Set_Port_Desc (Res, Inputs (0 .. 1), Outputs);

      Res := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("concat3")),
         Id_Concat3, 3, 1, 0);
      Ctxt.M_Concat (Id_Concat3) := Res;
      Set_Port_Desc (Res, Inputs (0 .. 2), Outputs);

      Res := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("concat4")),
         Id_Concat4, 4, 1, 0);
      Ctxt.M_Concat (Id_Concat4) := Res;
      Set_Port_Desc (Res, Inputs (0 .. 3), Outputs);
   end Create_Concat_Modules;

   procedure Create_Const_Modules (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Res : Module;
   begin
      Res := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("const_UB32")),
         Id_Const_UB32, 0, 1, 1);
      Ctxt.M_Const_UB32 := Res;
      Outputs := (0 => Create_Output ("o"));
      Set_Port_Desc (Res, Port_Desc_Array'(1 .. 0 => <>), Outputs);
      Set_Param_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("val")),
                     Typ => Param_Uns32)));

      Res := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("const_UL32")),
         Id_Const_UL32, 0, 1, 2);
      Ctxt.M_Const_UL32 := Res;
      Set_Port_Desc (Res, Port_Desc_Array'(1 .. 0 => <>), Outputs);
      Set_Param_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("val")),
                     Typ => Param_Uns32),
               1 => (New_Sname_Artificial (Get_Identifier ("xz")),
                     Typ => Param_Uns32)));
   end Create_Const_Modules;

   procedure Create_Extract_Module (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs : Port_Desc_Array (0 .. 0);
      Res : Module;
   begin
      Res := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("extract")),
         Id_Extract, 1, 1, 1);
      Ctxt.M_Extract := Res;
      Outputs := (0 => Create_Output ("o"));
      Inputs := (0 => Create_Input ("i"));
      Set_Port_Desc (Res, Inputs, Outputs);
      Set_Param_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("offset")),
                     Typ => Param_Uns32)));
   end Create_Extract_Module;

   procedure Create_Dyn_Extract_Module (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs : Port_Desc_Array (0 .. 1);
      Res : Module;
   begin
      Res := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("dyn_extract")),
         Id_Extract, 2, 1, 2);
      Ctxt.M_Dyn_Extract := Res;
      Outputs := (0 => Create_Output ("o"));
      Inputs := (0 => Create_Input ("i"),
                 1 => Create_Input ("v"));
      Set_Port_Desc (Res, Inputs, Outputs);
      Set_Param_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("step")),
                     Typ => Param_Uns32),
               1 => (New_Sname_Artificial (Get_Identifier ("offset")),
                     Typ => Param_Uns32)));
   end Create_Dyn_Extract_Module;

   procedure Create_Insert_Module (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs : Port_Desc_Array (0 .. 1);
      Res : Module;
   begin
      Res := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("insert")),
         Id_Insert, 2, 1, 1);
      Ctxt.M_Insert := Res;
      Outputs := (0 => Create_Output ("o"));
      Inputs := (0 => Create_Input ("i"),
                 1 => Create_Input ("v"));
      Set_Port_Desc (Res, Inputs, Outputs);
      Set_Param_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("index")),
                     Typ => Param_Uns32)));
   end Create_Insert_Module;

   procedure Create_Dyn_Insert_Module (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs : Port_Desc_Array (0 .. 2);
      Res : Module;
   begin
      Res := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("dyn_insert")),
         Id_Dyn_Insert, 3, 1, 2);
      Ctxt.M_Dyn_Insert := Res;
      Outputs := (0 => Create_Output ("o"));
      Inputs := (0 => Create_Input ("i"),
                 1 => Create_Input ("v"),
                 2 => Create_Input ("p"));
      Set_Port_Desc (Res, Inputs, Outputs);
      Set_Param_Desc
        (Res, (0 => (New_Sname_Artificial (Get_Identifier ("step")),
                     Typ => Param_Uns32),
               1 => (New_Sname_Artificial (Get_Identifier ("offset")),
                     Typ => Param_Uns32)));
   end Create_Dyn_Insert_Module;

   procedure Create_Edge_Module (Ctxt : Context_Acc;
                                 Res : out Module;
                                 Name : Name_Id)

   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs : Port_Desc_Array (0 .. 0);
   begin
      Res := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name), Id_Edge, 1, 1, 0);
      Inputs := (0 => Create_Input ("i", 1));
      Outputs := (0 => Create_Output ("o", 1));
      Set_Port_Desc (Res, Inputs, Outputs);
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
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("mux2")),
         Id_Mux2, 3, 1, 0);
      Set_Port_Desc (Ctxt.M_Mux2, Inputs (0 .. 2), Outputs);

      Ctxt.M_Mux4 := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("mux4")),
         Id_Mux4, 5, 1, 0);
      Set_Port_Desc (Ctxt.M_Mux4, Inputs (0 .. 4), Outputs);
   end Create_Mux_Modules;

   procedure Create_Objects_Module (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
      Inputs : Port_Desc_Array (0 .. 0);
      Inputs2 : Port_Desc_Array (0 .. 1);
   begin
      Inputs := (0 => Create_Input ("i"));
      Outputs := (0 => Create_Output ("o"));

      Ctxt.M_Output := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name_Output),
         Id_Output, 1, 1, 0);
      Set_Port_Desc (Ctxt.M_Output, Inputs, Outputs);

      Ctxt.M_Signal := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name_Signal),
         Id_Signal, 1, 1, 0);
      Set_Port_Desc (Ctxt.M_Signal, Inputs, Outputs);

      Inputs2 := (0 => Create_Input ("i"),
                  1 => Create_Input ("init"));
      Ctxt.M_Isignal := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("isignal")),
         Id_Isignal, 2, 1, 0);
      Set_Port_Desc (Ctxt.M_Isignal, Inputs2, Outputs);

      Ctxt.M_Port := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name_Port),
         Id_Port, 1, 1, 0);
      Set_Port_Desc (Ctxt.M_Port, Inputs, Outputs);
   end Create_Objects_Module;

   procedure Create_Dff_Modules (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (0 .. 0);
   begin
      Ctxt.M_Dff := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("dff")),
         Id_Dff, 2, 1, 0);
      Outputs := (0 => Create_Output ("q"));
      Set_Port_Desc (Ctxt.M_Dff, (0 => Create_Input ("clk", 1),
                                  1 => Create_Input ("d")),
                     Outputs);

      Ctxt.M_Idff := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("idff")),
         Id_Idff, 3, 1, 0);
      Set_Port_Desc (Ctxt.M_Idff, (0 => Create_Input ("clk", 1),
                                   1 => Create_Input ("d"),
                                   2 => Create_Input ("init")),
                     Outputs);


      Ctxt.M_Adff := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("adff")),
         Id_Adff, 4, 1, 0);
      Outputs := (0 => Create_Output ("q"));
      Set_Port_Desc (Ctxt.M_Adff, (0 => Create_Input ("clk", 1),
                                   1 => Create_Input ("d"),
                                   2 => Create_Input ("rst", 1),
                                   3 => Create_Input ("rst_val")),
                     Outputs);

      Ctxt.M_Iadff := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Get_Identifier ("iadff")),
         Id_Iadff, 5, 1, 0);
      Outputs := (0 => Create_Output ("q"));
      Set_Port_Desc (Ctxt.M_Iadff, (0 => Create_Input ("clk", 1),
                                    1 => Create_Input ("d"),
                                    2 => Create_Input ("rst"),
                                    3 => Create_Input ("rst_val"),
                                    4 => Create_Input ("init")),
                     Outputs);
   end Create_Dff_Modules;

   procedure Create_Assert_Assume (Ctxt : Context_Acc)
   is
      Outputs : Port_Desc_Array (1 .. 0);
   begin
      Ctxt.M_Assert := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name_Assert), Id_Assert,
         1, 0, 0);
      Set_Port_Desc (Ctxt.M_Assert, (0 => Create_Input ("cond", 1)),
                     Outputs);

      Ctxt.M_Assume := New_User_Module
        (Ctxt.Design, New_Sname_Artificial (Name_Assume), Id_Assume,
         1, 0, 0);
      Set_Port_Desc (Ctxt.M_Assume, (0 => Create_Input ("cond", 1)),
                     Outputs);
   end Create_Assert_Assume;

   function Build_Builders (Design : Module) return Context_Acc
   is
      Res : Context_Acc;
   begin
      Res := new Context'(Design => Design,
                          Parent => No_Module,
                          Num => 0,
                          M_Dyadic => (others => No_Module),
                          M_Monadic => (others => No_Module),
                          M_Compare => (others => No_Module),
                          M_Concat => (others => No_Module),
                          M_Truncate | M_Extend => (others => No_Module),
                          M_Reduce => (others => No_Module),
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
      Create_Dyadic_Module (Design, Res.M_Dyadic (Id_Mul),
                            Get_Identifier ("mul"), Id_Mul);

      Create_Monadic_Module (Design, Res.M_Monadic (Id_Not), Name_Not, Id_Not);
      Create_Monadic_Module (Design, Res.M_Monadic (Id_Neg),
                             Get_Identifier ("neg"), Id_Neg);

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

      Create_Concat_Modules (Res);
      Create_Const_Modules (Res);

      Create_Extract_Module (Res);
      Create_Dyn_Extract_Module (Res);
      Create_Insert_Module (Res);
      Create_Dyn_Insert_Module (Res);

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

      Create_Edge_Module (Res, Res.M_Edge, Name_Edge);

      Create_Mux_Modules (Res);
      Create_Objects_Module (Res);
      Create_Dff_Modules (Res);

      Create_Assert_Assume (Res);

      return Res;
   end Build_Builders;

   procedure Set_Parent (Ctxt : Context_Acc; Parent : Module) is
   begin
      Ctxt.Parent := Parent;
   end Set_Parent;

   function New_Internal_Name (Ctxt : Context_Acc) return Sname
   is
      Name : Sname;
   begin
      Name := New_Sname_Version (Get_Name (Ctxt.Parent), Ctxt.Num);
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
      pragma Assert (Wd /= No_Width);
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

   function Build_Const_UB32 (Ctxt : Context_Acc;
                              Val : Uns32;
                              W : Width) return Net
   is
      pragma Assert (W <= 32);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Const_UB32);
      O := Get_Output (Inst, 0);
      Set_Param_Uns32 (Inst, 0, Val);
      Set_Width (O, W);
      return O;
   end Build_Const_UB32;

   function Build_Const_UL32 (Ctxt : Context_Acc;
                              Val : Uns32;
                              Xz : Uns32;
                              W : Width) return Net
   is
      pragma Assert (W <= 32);
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

   function Build_Edge (Ctxt : Context_Acc; Src : Net) return Net
   is
      pragma Assert (Get_Width (Src) = 1);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Edge);
      O := Get_Output (Inst, 0);
      pragma Assert (Get_Width (O) = 1);
      Connect (Get_Input (Inst, 0), Src);
      return O;
   end Build_Edge;

   function Build_Mux2 (Ctxt : Context_Acc;
                        Sel : Net;
                        I0, I1 : Net) return Net
   is
      Wd : constant Width := Get_Width (I0);
      pragma Assert (Wd /= No_Width);
      pragma Assert (Get_Width (I1) = Wd);
      pragma Assert (Get_Width (Sel) = 1);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Mux2);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), Sel);
      Connect (Get_Input (Inst, 1), I0);
      Connect (Get_Input (Inst, 2), I1);
      return O;
   end Build_Mux2;

   function Build_Mux4 (Ctxt : Context_Acc;
                        Sel : Net;
                        I0, I1, I2, I3 : Net) return Net
   is
      Wd : constant Width := Get_Width (I0);
      pragma Assert (Wd /= No_Width);
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

   function Build_Insert
     (Ctxt : Context_Acc; I : Net; V : Net; Off : Width) return Net
   is
      Wd : constant Width := Get_Width (I);
      pragma Assert (Off < Wd);
      pragma Assert (Get_Width (V) + Off <= Wd);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Insert);
      Connect (Get_Input (Inst, 0), I);
      Connect (Get_Input (Inst, 1), V);
      Set_Param_Uns32 (Inst, 0, Off);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      return O;
   end Build_Insert;

   function Build_Dyn_Insert
     (Ctxt : Context_Acc;
      I : Net; V : Net; P : Net; Step : Uns32; Off : Int32)
     return Net
   is
      Wd : constant Width := Get_Width (I);
      pragma Assert (Wd /= No_Width);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Dyn_Insert);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), I);
      Connect (Get_Input (Inst, 1), V);
      Connect (Get_Input (Inst, 2), P);
      Set_Param_Uns32 (Inst, 0, Step);
      Set_Param_Uns32 (Inst, 1, To_Uns32 (Off));
      return O;
   end Build_Dyn_Insert;

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
      pragma Assert (Wd /= No_Width);
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
      pragma Assert (Wd /= No_Width);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Port);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), N);
      return O;
   end Build_Port;

   function Build_Dff (Ctxt : Context_Acc;
                       Clk : Net;
                       D : Net) return Net
   is
      Wd : constant Width := Get_Width (D);
      pragma Assert (Wd /= No_Width);
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
      pragma Assert (Wd /= No_Width);
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
      pragma Assert (Wd /= No_Width);
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
      Wd : constant Width := Get_Width (D);
      pragma Assert (Wd /= No_Width);
      pragma Assert (Get_Width (Clk) = 1);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Iadff);
      O := Get_Output (Inst, 0);
      Set_Width (O, Wd);
      Connect (Get_Input (Inst, 0), Clk);
      Connect (Get_Input (Inst, 1), D);
      Connect (Get_Input (Inst, 2), Rst);
      Connect (Get_Input (Inst, 3), Rst_Val);
      Connect (Get_Input (Inst, 4), Init);
      return O;
   end Build_Iadff;

   function Build_Extract
     (Ctxt : Context_Acc; I : Net; Off, W : Width) return Net
   is
      Wd : constant Width := Get_Width (I);
      pragma Assert (Wd /= No_Width);
      pragma Assert (W > 0);
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
     (Ctxt : Context_Acc;
      I : Net; P : Net; Step : Uns32; Off : Int32; W : Width) return Net
   is
      Wd : constant Width := Get_Width (I);
      pragma Assert (Wd /= No_Width);
      pragma Assert (W > 0);
      Inst : Instance;
      O : Net;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Dyn_Extract);
      O := Get_Output (Inst, 0);
      Set_Width (O, W);
      Connect (Get_Input (Inst, 0), I);
      Connect (Get_Input (Inst, 1), P);
      Set_Param_Uns32 (Inst, 0, Step);
      Set_Param_Uns32 (Inst, 1, To_Uns32 (Off));
      return O;
   end Build_Dyn_Extract;

   function Build_Extract_Bit
     (Ctxt : Context_Acc; I : Net; Off : Width) return Net is
   begin
      return Build_Extract (Ctxt, I, Off, 1);
   end Build_Extract_Bit;

   procedure Build_Assert (Ctxt : Context_Acc; Cond : Net)
   is
      Inst : Instance;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Assert);
      Connect (Get_Input (Inst, 0), Cond);
   end Build_Assert;

   procedure Build_Assume (Ctxt : Context_Acc; Cond : Net)
   is
      Inst : Instance;
   begin
      Inst := New_Internal_Instance (Ctxt, Ctxt.M_Assume);
      Connect (Get_Input (Inst, 0), Cond);
   end Build_Assume;

end Netlists.Builders;
