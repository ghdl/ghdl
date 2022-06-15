--  Netlist.
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

with Std_Names;
with Name_Table;
with Tables;
with Simple_IO;
with Dyn_Maps;

with Netlists.Utils; use Netlists.Utils;
with Netlists.Gates;

package body Netlists is

   --  Names

   package Snames_Table is new Tables
     (Table_Component_Type => Sname_Record,
      Table_Index_Type => Sname,
      Table_Low_Bound => 0,
      Table_Initial => 1024);

   function New_Sname_User (Id : Name_Id; Prefix : Sname) return Sname is
   begin
      Snames_Table.Append ((Kind => Sname_User,
                            Prefix => Prefix,
                            Suffix => Uns32 (Id)));
      return Snames_Table.Last;
   end New_Sname_User;

   function New_Sname_Artificial (Id : Name_Id; Prefix : Sname) return Sname is
   begin
      Snames_Table.Append ((Kind => Sname_Artificial,
                            Prefix => Prefix,
                            Suffix => Uns32 (Id)));
      return Snames_Table.Last;
   end New_Sname_Artificial;

   function New_Sname_Version (Ver : Uns32; Prefix : Sname) return Sname is
   begin
      Snames_Table.Append ((Kind => Sname_Version,
                            Prefix => Prefix,
                            Suffix => Ver));
      return Snames_Table.Last;
   end New_Sname_Version;

   function Is_Valid (Name : Sname) return Boolean is
   begin
      return Name > No_Sname and Name <= Snames_Table.Last;
   end Is_Valid;

   function Get_Sname_Kind (Name : Sname) return Sname_Kind is
   begin
      pragma Assert (Is_Valid (Name));
      return Snames_Table.Table (Name).Kind;
   end Get_Sname_Kind;

   function Get_Sname_Prefix (Name : Sname) return Sname is
   begin
      pragma Assert (Is_Valid (Name));
      return Snames_Table.Table (Name).Prefix;
   end Get_Sname_Prefix;

   procedure Set_Sname_Prefix (Name : Sname; Prefix : Sname) is
   begin
      pragma Assert (Is_Valid (Name));
      Snames_Table.Table (Name).Prefix := Prefix;
   end Set_Sname_Prefix;

   function Get_Sname_Suffix (Name : Sname) return Name_Id
   is
      subtype Snames_Suffix is Sname_Kind range Sname_User .. Sname_Artificial;
   begin
      pragma Assert (Is_Valid (Name));
      pragma Assert (Get_Sname_Kind (Name) in Snames_Suffix);
      return Name_Id (Snames_Table.Table (Name).Suffix);
   end Get_Sname_Suffix;

   function Get_Sname_Version (Name : Sname) return Uns32 is
   begin
      pragma Assert (Is_Valid (Name));
      pragma Assert (Get_Sname_Kind (Name) = Sname_Version);
      return Snames_Table.Table (Name).Suffix;
   end Get_Sname_Version;


   --  Modules

   package Modules_Table is new Tables
     (Table_Component_Type => Module_Record,
      Table_Index_Type => Module,
      Table_Low_Bound => No_Module,
      Table_Initial => 1024);

   package Port_Desc_Table is new Tables
     (Table_Component_Type => Port_Desc,
      Table_Index_Type => Port_Desc_Idx,
      Table_Low_Bound => No_Port_Desc_Idx,
      Table_Initial => 1024);

   function New_Design (Name : Sname) return Module
   is
      Res : Module;
      Self : Instance;
   begin
      Modules_Table.Append ((Parent => No_Module,
                             Name => Name,
                             Id => Id_Design,
                             First_Port_Desc => No_Port_Desc_Idx,
                             Nbr_Inputs => 0,
                             Nbr_Outputs => 0,
                             First_Param_Desc => No_Param_Desc_Idx,
                             Nbr_Params => 0,
                             First_Sub_Module => No_Module,
                             Last_Sub_Module => No_Module,
                             Next_Sub_Module => No_Module,
                             First_Instance => No_Instance,
                             Last_Instance => No_Instance));
      Res := Modules_Table.Last;
      Self := Create_Self_Instance (Res);
      pragma Unreferenced (Self);

      return Res;
   end New_Design;

   function Is_Valid (M : Module) return Boolean is
   begin
      return M > No_Module and then M <= Modules_Table.Last;
   end Is_Valid;

   function New_User_Module (Parent : Module;
                             Name : Sname;
                             Id : Module_Id;
                             Nbr_Inputs : Port_Nbr;
                             Nbr_Outputs : Port_Nbr;
                             Nbr_Params : Param_Nbr := 0)
                            return Module
   is
      pragma Assert (Is_Valid (Parent));
      Ports_Desc : Port_Desc_Idx;
      Res : Module;
   begin
      Ports_Desc := Port_Desc_Table.Last + 1;
      for I in 1 .. Nbr_Inputs + Nbr_Outputs loop
         Port_Desc_Table.Append
           ((Name => No_Sname, Dir => Port_In, W => 0));
      end loop;

      Modules_Table.Append
        ((Parent => Parent,
          Name => Name,
          Id => Id,
          First_Port_Desc => Ports_Desc,
          Nbr_Inputs => Nbr_Inputs,
          Nbr_Outputs => Nbr_Outputs,
          First_Param_Desc => No_Param_Desc_Idx,
          Nbr_Params => Nbr_Params,
          First_Sub_Module => No_Module,
          Last_Sub_Module => No_Module,
          Next_Sub_Module => No_Module,
          First_Instance => No_Instance,
          Last_Instance => No_Instance));
      Res := Modules_Table.Last;

      --  Append
      declare
         Parent_Rec : Module_Record renames Modules_Table.Table (Parent);
      begin
         if Parent_Rec.First_Sub_Module = No_Module then
            Parent_Rec.First_Sub_Module := Res;
         else
            Modules_Table.Table (Parent_Rec.Last_Sub_Module).Next_Sub_Module
              := Res;
         end if;
         Parent_Rec.Last_Sub_Module := Res;
      end;

      return Res;
   end New_User_Module;

   function Get_Module_Name (M : Module) return Sname is
   begin
      pragma Assert (Is_Valid (M));
      return Modules_Table.Table (M).Name;
   end Get_Module_Name;

   function Get_Id (M : Module) return Module_Id is
   begin
      pragma Assert (Is_Valid (M));
      return Modules_Table.Table (M).Id;
   end Get_Id;

   function Get_Nbr_Inputs (M : Module) return Port_Nbr is
   begin
      pragma Assert (Is_Valid (M));
      return Modules_Table.Table (M).Nbr_Inputs;
   end Get_Nbr_Inputs;

   function Get_Nbr_Outputs (M : Module) return Port_Nbr is
   begin
      pragma Assert (Is_Valid (M));
      return Modules_Table.Table (M).Nbr_Outputs;
   end Get_Nbr_Outputs;

   function Get_Nbr_Params (M : Module) return Param_Nbr is
   begin
      pragma Assert (Is_Valid (M));
      return Modules_Table.Table (M).Nbr_Params;
   end Get_Nbr_Params;

   function Get_First_Port_Desc (M : Module) return Port_Desc_Idx is
   begin
      pragma Assert (Is_Valid (M));
      return Modules_Table.Table (M).First_Port_Desc;
   end Get_First_Port_Desc;

   function Get_Input_First_Desc (M : Module) return Port_Desc_Idx
   is
      pragma Assert (Is_Valid (M));
   begin
      return Modules_Table.Table (M).First_Port_Desc;
   end Get_Input_First_Desc;

   function Get_Output_First_Desc (M : Module) return Port_Desc_Idx
   is
      pragma Assert (Is_Valid (M));
   begin
      return Modules_Table.Table (M).First_Port_Desc
        + Port_Desc_Idx (Modules_Table.Table (M).Nbr_Inputs);
   end Get_Output_First_Desc;

   function Get_Self_Instance (M : Module) return Instance is
   begin
      pragma Assert (Is_Valid (M));
      return Modules_Table.Table (M).First_Instance;
   end Get_Self_Instance;

   function Get_First_Instance (M : Module) return Instance
   is
      First : constant Instance := Get_Self_Instance (M);
   begin
      if First = No_Instance then
         --  Empty module.
         return No_Instance;
      else
         return Get_Next_Instance (First);
      end if;
   end Get_First_Instance;

   function Get_First_Sub_Module (M : Module) return Module is
   begin
      pragma Assert (Is_Valid (M));
      return Modules_Table.Table (M).First_Sub_Module;
   end Get_First_Sub_Module;

   function Get_Next_Sub_Module (M : Module) return Module is
   begin
      pragma Assert (Is_Valid (M));
      return Modules_Table.Table (M).Next_Sub_Module;
   end Get_Next_Sub_Module;

   --  Instances

   --  List of free instances, linked by Next_Instance.
   Free_Instances : Instance := No_Instance;

   package Instances_Table is new Tables
     (Table_Component_Type => Instance_Record,
      Table_Index_Type => Instance,
      Table_Low_Bound => No_Instance,
      Table_Initial => 1024);

   --  List of free nets.
   --  As most of gates have only one output, just keep a single list.
   Free_Nets : Net := No_Net;

   package Nets_Table is new Tables
     (Table_Component_Type => Net_Record,
      Table_Index_Type => Net,
      Table_Low_Bound => No_Net,
      Table_Initial => 1024);

   --  List of free consecutive inputs.
   Free_Inputs : array (Port_Idx range 1 .. 4) of Input :=
     (others => No_Input);

   package Inputs_Table is new Tables
     (Table_Component_Type => Input_Record,
      Table_Index_Type => Input,
      Table_Low_Bound => No_Input,
      Table_Initial => 1024);

   Free_Params : array (Param_Idx range 1 .. 32) of Param_Idx :=
     (others => No_Param_Idx);
   Free_Params2 : Param_Idx := No_Param_Idx;

   package Params_Table is new Tables
     (Table_Component_Type => Uns32,
      Table_Index_Type => Param_Idx,
      Table_Low_Bound => No_Param_Idx,
      Table_Initial => 256);

   --  Hash INST (simply return its index).
   function Hash (Inst : Instance) return Hash_Value_Type is
   begin
      return Hash_Value_Type (Inst);
   end Hash;

   procedure Extract_All_Instances (M : Module; First_Instance : out Instance)
   is
      pragma Assert (Is_Valid (M));
      M_Ent : Module_Record renames Modules_Table.Table (M);
   begin
      First_Instance := M_Ent.First_Instance;

      --  Clear the instance list.
      M_Ent.First_Instance := No_Instance;
      M_Ent.Last_Instance := No_Instance;
   end Extract_All_Instances;

   procedure Append_Instance (M : Module; Inst : Instance)
   is
      M_Ent : Module_Record renames Modules_Table.Table (M);
   begin
      if M_Ent.First_Instance = No_Instance then
         M_Ent.First_Instance := Inst;
      else
         Instances_Table.Table (M_Ent.Last_Instance).Next_Instance := Inst;
      end if;
      Instances_Table.Table (Inst).Prev_Instance := M_Ent.Last_Instance;
      Instances_Table.Table (Inst).Next_Instance := No_Instance;
      M_Ent.Last_Instance := Inst;
   end Append_Instance;

   procedure Extract_Instance (Inst : Instance)
   is
      pragma Assert (Is_Valid (Inst));
      Inst_Ent : Instance_Record renames Instances_Table.Table (Inst);
      M : constant Module := Inst_Ent.Parent;
      M_Ent : Module_Record renames Modules_Table.Table (M);
   begin
      if Inst_Ent.Prev_Instance /= No_Instance then
         Set_Next_Instance (Inst_Ent.Prev_Instance, Inst_Ent.Next_Instance);
      else
         pragma Assert (M_Ent.First_Instance = Inst);
         M_Ent.First_Instance := Inst_Ent.Next_Instance;
      end if;

      if Inst_Ent.Next_Instance /= No_Instance then
         Set_Prev_Instance (Inst_Ent.Next_Instance, Inst_Ent.Prev_Instance);
      else
         pragma Assert (M_Ent.Last_Instance = Inst);
         M_Ent.Last_Instance := Inst_Ent.Prev_Instance;
      end if;

      Inst_Ent.Prev_Instance := No_Instance;
      Inst_Ent.Next_Instance := No_Instance;
   end Extract_Instance;

   function Check_Connected (Inst : Instance) return Boolean
   is
      Nbr_Outputs : constant Port_Idx := Get_Nbr_Outputs (Inst);
      Nbr_Inputs : constant Port_Idx := Get_Nbr_Inputs (Inst);
   begin
      --  Check that all outputs are unused.
      if Nbr_Outputs > 0 then
         for K in 0 .. Nbr_Outputs - 1 loop
            if Is_Connected (Get_Output (Inst, K)) then
               return True;
            end if;
         end loop;
      end if;

      --  First disconnect inputs.
      if Nbr_Inputs > 0 then
         for K in 0 .. Nbr_Inputs - 1 loop
            if Get_Driver (Get_Input (Inst, K)) /= No_Net then
               return True;
            end if;
         end loop;
      end if;

      return False;
   end Check_Connected;

   procedure Remove_Instance (Inst : Instance) is
   begin
      pragma Assert (not Check_Connected (Inst));
      Extract_Instance (Inst);
      Free_Instance (Inst);
   end Remove_Instance;

   function New_Instance_Internal (Parent : Module;
                                   M : Module;
                                   Name : Sname;
                                   Nbr_Inputs : Port_Nbr;
                                   Nbr_Outputs : Port_Nbr;
                                   Nbr_Params : Param_Nbr)
                                  return Instance
   is
      pragma Assert (Is_Valid (Parent));
      pragma Assert (Is_Valid (M));
      Res : Instance;
      Inputs : Input;
      Outputs : Net;
      Params : Param_Idx;
   begin
      if Free_Instances = No_Instance then
         Instances_Table.Increment_Last;
         Res := Instances_Table.Last;
      else
         Res := Free_Instances;
         Free_Instances := Instances_Table.Table (Res).Next_Instance;
      end if;

      if Nbr_Inputs > 0 then
         if Nbr_Inputs <= Free_Inputs'Last then
            if Free_Inputs (Nbr_Inputs) /= No_Input then
               --  Get a free input from the free list.
               Inputs := Free_Inputs (Nbr_Inputs);
               Free_Inputs (Nbr_Inputs) :=
                 Inputs_Table.Table (Inputs).Next_Sink;
            elsif Nbr_Inputs = 1 and then Free_Inputs (2) /= No_Input
            then
               --  Ok, common case: need just one input; get it from the list
               --  of free 2-inputs.
               pragma Assert (Free_Inputs (1) = No_Input);
               Inputs := Free_Inputs (2);
               Free_Inputs (2) := Inputs_Table.Table (Inputs).Next_Sink;
               Free_Inputs (1) := Inputs + 1;
               Inputs_Table.Table (Inputs + 1).Next_Sink := 0;
            else
               Inputs := Inputs_Table.Allocate (Natural (Nbr_Inputs));
            end if;
         else
            Inputs := Inputs_Table.Allocate (Natural (Nbr_Inputs));
         end if;
      else
         Inputs := No_Input;
      end if;

      if Nbr_Outputs > 0 then
         if Nbr_Outputs = 1 and then Free_Nets /= No_Net then
            Outputs := Free_Nets;
            Free_Nets := Net (Nets_Table.Table (Outputs).First_Sink);
         else
            Outputs := Nets_Table.Allocate (Natural (Nbr_Outputs));
         end if;
      else
         Outputs := No_Net;
      end if;

      if Nbr_Params > 0 then
         if Nbr_Params <= Free_Params'Last
           and then Free_Params (Nbr_Params) /= No_Param_Idx
         then
            Params := Free_Params (Nbr_Params);
            Free_Params (Nbr_Params) :=
              Param_Idx (Params_Table.Table (Params));
         else
            Params := Params_Table.Allocate (Natural (Nbr_Params));
         end if;
      else
         Params := No_Param_Idx;
      end if;

      Instances_Table.Table (Res) := ((Parent => Parent,
                                       Has_Attr => False,
                                       Flag4 => False,
                                       Next_Instance => No_Instance,
                                       Prev_Instance => No_Instance,
                                       Klass => M,
                                       Flag_Mark => False,
                                       Flag5 | Flag6 => False,
                                       Flag2 => False,
                                       Name => Name,
                                       First_Param => Params,
                                       First_Input => Inputs,
                                       First_Output => Outputs));

      --  Setup inputs.
      if Nbr_Inputs > 0 then
         for I in 0 .. Nbr_Inputs - 1 loop
            Inputs_Table.Table (Inputs + Input (I)) :=
              (Parent => Res,
               Driver => No_Net,
               Next_Sink => No_Input);
         end loop;
      end if;

      --  Setup nets.
      if Nbr_Outputs > 0 then
         for I in 0 .. Nbr_Outputs - 1 loop
            Nets_Table.Table (Outputs + Net (I)) := (Parent => Res,
                                                     First_Sink => No_Input,
                                                     W => 0);
         end loop;
      end if;

      --  Init params (to 0).
      if Nbr_Params > 0 then
         for I in 0 .. Nbr_Params - 1 loop
            Params_Table.Table (Params + I) := 0;
         end loop;
      end if;

      return Res;
   end New_Instance_Internal;

   procedure Set_Outputs_Width_From_Desc (Inst : Instance;
                                          Nbr_Outputs : Port_Nbr;
                                          Outputs_Desc : Port_Desc_Idx) is
   begin
      if Nbr_Outputs > 0 then
         for I in 0 .. Nbr_Outputs - 1 loop
            Set_Width
              (Get_Output (Inst, I),
               Get_Port_Desc (Outputs_Desc + Port_Desc_Idx (I)).W);
         end loop;
      end if;
   end Set_Outputs_Width_From_Desc;

   function New_Instance (Parent : Module; M : Module; Name : Sname)
                         return Instance
   is
      Nbr_Inputs : constant Port_Nbr := Get_Nbr_Inputs (M);
      Nbr_Outputs : constant Port_Nbr := Get_Nbr_Outputs (M);
      Nbr_Params : constant Param_Nbr := Get_Nbr_Params (M);
      Res : Instance;
   begin
      Res := New_Instance_Internal
        (Parent, M, Name, Nbr_Inputs, Nbr_Outputs, Nbr_Params);
      Set_Outputs_Width_From_Desc
        (Res, Nbr_Outputs, Get_Output_First_Desc (M));

      --  Link instance
      Append_Instance (Parent, Res);

      return Res;
   end New_Instance;

   function New_Var_Instance (Parent : Module;
                              M : Module;
                              Name : Sname;
                              Nbr_Inputs : Port_Nbr;
                              Nbr_Outputs : Port_Nbr;
                              Nbr_Params : Param_Nbr)
                             return Instance
   is
      Res : Instance;
   begin
      Res := New_Instance_Internal
        (Parent, M, Name, Nbr_Inputs, Nbr_Outputs, Nbr_Params);

      --  Link instance
      Append_Instance (Parent, Res);

      return Res;
   end New_Var_Instance;

   function Create_Self_Instance (M : Module) return Instance
   is
      --  Can be done only once.
      pragma Assert (Get_Self_Instance (M) = No_Instance);
      Nbr_Inputs : constant Port_Nbr := Get_Nbr_Inputs (M);
      Nbr_Outputs : constant Port_Nbr := Get_Nbr_Outputs (M);
      Res : Instance;
   begin
      --  Swap inputs and outputs; no parameters.
      Res := New_Instance_Internal
        (M, M, Get_Module_Name (M), Nbr_Outputs, Nbr_Inputs, 0);
      Set_Outputs_Width_From_Desc
        (Res, Nbr_Inputs, Get_Input_First_Desc (M));

      Append_Instance (M, Res);

      return Res;
   end Create_Self_Instance;

   function Is_Valid (I : Instance) return Boolean is
   begin
      return I > No_Instance and then I <= Instances_Table.Last;
   end Is_Valid;

   function Is_Self_Instance (I : Instance) return Boolean is
      Irec : Instance_Record renames Instances_Table.Table (I);
   begin
      return Irec.Parent = Irec.Klass;
   end Is_Self_Instance;

   type Module_Counter_Type is array (Module range 1 .. 128) of Natural;
   Free_Instances_Counter :  Module_Counter_Type :=
     (others => 0);

   procedure Free_Input (First : Input; Nbr : Port_Nbr)
   is
      pragma Assert (Nbr in Free_Inputs'Range);
   begin
      Inputs_Table.Table (First).Next_Sink := Free_Inputs (Nbr);
      Free_Inputs (Nbr) := First;
   end Free_Input;

   procedure Free_Instance (Inst : Instance)
   is
      pragma Assert (Is_Valid (Inst));
      Inst_Rec : Instance_Record renames Instances_Table.Table (Inst);
      Nbr_Outputs : Port_Nbr;
      Nbr_Inputs  : Port_Nbr;
      Nbr_Params : Param_Idx;
   begin
      pragma Assert (not Check_Connected (Inst));

      --  Instance must not be linked anymore.
      pragma Assert (Inst_Rec.Prev_Instance = No_Instance);
      pragma Assert (Inst_Rec.Next_Instance = No_Instance);

      if Inst_Rec.Klass <= Free_Instances_Counter'Last then
         Free_Instances_Counter (Inst_Rec.Klass) :=
           Free_Instances_Counter (Inst_Rec.Klass) + 1;
      end if;

      Nbr_Outputs := Get_Nbr_Outputs (Inst);
      for I in 1 .. Nbr_Outputs loop
         declare
            N : constant Net := Get_Output (Inst, I - 1);
         begin
            Nets_Table.Table (N).Parent := No_Instance;
         end;
      end loop;

      if Nbr_Outputs /= 0 then
         --  Put all nets, one by one, on the list of free nets.
         for I in 0 .. Net (Nbr_Outputs - 1) loop
            Nets_Table.Table (Inst_Rec.First_Output + I).First_Sink :=
              Input (Inst_Rec.First_Output + I + 1);
         end loop;
         Nets_Table.Table (Inst_Rec.First_Output
                           + Net (Nbr_Outputs - 1)).First_Sink :=
           Input (Free_Nets);
         Free_Nets := Inst_Rec.First_Output;
      end if;

      Nbr_Inputs := Get_Nbr_Inputs (Inst);
      for I in 1 .. Nbr_Inputs loop
         declare
            Inp : constant Input := Get_Input (Inst, I - 1);
         begin
            Inputs_Table.Table (Inp).Parent := No_Instance;
         end;
      end loop;

      if Nbr_Inputs /= 0 then
         if Nbr_Inputs <= Free_Inputs'Last then
            Free_Input (Inst_Rec.First_Input, Nbr_Inputs);
         else
            declare
               Num : Port_Nbr;
               First : Input;
            begin
               --  Free per pairs.
               Num := Nbr_Inputs;
               First := Inst_Rec.First_Input;
               while Num >= 2 loop
                  Free_Input (First, 2);
                  First := First + 2;
                  Num := Num - 2;
               end loop;
               --  Free the last one.
               if Num = 1 then
                  Free_Input (First, 1);
               end if;
            end;
         end if;
      end if;

      Nbr_Params := Get_Nbr_Params (Inst);
      if Nbr_Params /= 0 then
         if Nbr_Params <= Free_Params'Last then
            Params_Table.Table (Inst_Rec.First_Param) :=
              Uns32 (Free_Params (Nbr_Params));
            Free_Params (Nbr_Params) := Inst_Rec.First_Param;
         else
            Params_Table.Table (Inst_Rec.First_Param) :=
              Uns32 (Free_Params2);
            Params_Table.Table (Inst_Rec.First_Param + 1) :=
              Uns32 (Nbr_Params);
            Free_Params2 := Inst_Rec.First_Param;
         end if;
      end if;

      Inst_Rec.Klass := Free_Module;

      Inst_Rec.Next_Instance := Free_Instances;
      Free_Instances := Inst;
   end Free_Instance;

   function Get_Module (Inst : Instance) return Module is
   begin
      pragma Assert (Is_Valid (Inst));
      return Instances_Table.Table (Inst).Klass;
   end Get_Module;

   function Get_Instance_Name (Inst : Instance) return Sname is
   begin
      pragma Assert (Is_Valid (Inst));
      return Instances_Table.Table (Inst).Name;
   end Get_Instance_Name;

   procedure Set_Instance_Name (Inst : Instance; Name : Sname) is
   begin
      pragma Assert (Is_Valid (Inst));
      Instances_Table.Table (Inst).Name := Name;
   end Set_Instance_Name;

   function Get_Instance_Parent (Inst : Instance) return Module is
   begin
      pragma Assert (Is_Valid (Inst));
      return Instances_Table.Table (Inst).Parent;
   end Get_Instance_Parent;

   function Get_Next_Instance (Inst : Instance) return Instance is
   begin
      pragma Assert (Is_Valid (Inst));
      return Instances_Table.Table (Inst).Next_Instance;
   end Get_Next_Instance;

   procedure Set_Next_Instance (Inst : Instance; Next : Instance) is
   begin
      pragma Assert (Is_Valid (Inst));
      Instances_Table.Table (Inst).Next_Instance := Next;
   end Set_Next_Instance;

   procedure Set_Prev_Instance (Inst : Instance; Prev : Instance) is
   begin
      pragma Assert (Is_Valid (Inst));
      Instances_Table.Table (Inst).Prev_Instance := Prev;
   end Set_Prev_Instance;

   function Get_First_Output (Inst : Instance) return Net is
   begin
      pragma Assert (Is_Valid (Inst));
      return Instances_Table.Table (Inst).First_Output;
   end Get_First_Output;

   function Get_Output (Inst : Instance; Idx : Port_Idx) return Net is
   begin
      pragma Assert (Is_Valid (Inst));
      pragma Assert (Idx < Get_Nbr_Outputs (Inst));
      return Instances_Table.Table (Inst).First_Output + Net (Idx);
   end Get_Output;

   function Get_Input (Inst : Instance; Idx : Port_Idx) return Input is
   begin
      pragma Assert (Is_Valid (Inst));
      pragma Assert (Idx < Get_Nbr_Inputs (Inst));
      return Instances_Table.Table (Inst).First_Input + Input (Idx);
   end Get_Input;

   --  Nets

   function Is_Valid (N : Net) return Boolean is
   begin
      return N > No_Net and then N <= Nets_Table.Last;
   end Is_Valid;

   function Get_Net_Parent (O : Net) return Instance is
   begin
      pragma Assert (Is_Valid (O));
      return Nets_Table.Table (O).Parent;
   end Get_Net_Parent;

   function Get_Port_Idx (O : Net) return Port_Idx
   is
      pragma Assert (Is_Valid (O));
      Parent : constant Instance := Get_Net_Parent (O);
   begin
      return Port_Idx (O - Instances_Table.Table (Parent).First_Output);
   end Get_Port_Idx;

   function Get_First_Sink (O : Net) return Input is
   begin
      pragma Assert (Is_Valid (O));
      return Nets_Table.Table (O).First_Sink;
   end Get_First_Sink;

   function Get_Width (N : Net) return Width
   is
      pragma Assert (Is_Valid (N));
   begin
      return Nets_Table.Table (N).W;
   end Get_Width;

   procedure Set_Width (N : Net; W : Width)
   is
      pragma Assert (Is_Valid (N));
   begin
      if Nets_Table.Table (N).W /= No_Width then
         raise Internal_Error;
      end if;
      Nets_Table.Table (N).W := W;
   end Set_Width;


   --  Inputs

   function Is_Valid (N : Input) return Boolean is
   begin
      return N > No_Input and then N <= Inputs_Table.Last;
   end Is_Valid;

   function Get_Input_Parent (I : Input) return Instance is
   begin
      pragma Assert (Is_Valid (I));
      return Inputs_Table.Table (I).Parent;
   end Get_Input_Parent;

   function Get_Port_Idx (I : Input) return Port_Idx
   is
      pragma Assert (Is_Valid (I));
      Parent : constant Instance := Get_Input_Parent (I);
   begin
      return Port_Idx (I - Instances_Table.Table (Parent).First_Input);
   end Get_Port_Idx;

   function Get_Driver (I : Input) return Net is
   begin
      pragma Assert (Is_Valid (I));
      return Inputs_Table.Table (I).Driver;
   end Get_Driver;

   function Get_Next_Sink (I : Input) return Input is
   begin
      pragma Assert (Is_Valid (I));
      return Inputs_Table.Table (I).Next_Sink;
   end Get_Next_Sink;


   --  Port_Desc

   function Get_Port_Desc (Idx : Port_Desc_Idx) return Port_Desc is
   begin
      return Port_Desc_Table.Table (Idx);
   end Get_Port_Desc;

   procedure Set_Port_Desc (Idx : Port_Desc_Idx; Desc : Port_Desc) is
   begin
      Port_Desc_Table.Table (Idx) := Desc;
   end Set_Port_Desc;

   function Get_Input_Desc (M : Module; I : Port_Idx) return Port_Desc
   is
      F : constant Port_Desc_Idx := Get_Input_First_Desc (M);
      pragma Assert (I < Get_Nbr_Inputs (M));
   begin
      return Get_Port_Desc (F + Port_Desc_Idx (I));
   end Get_Input_Desc;

   function Get_Output_Desc (M : Module; O : Port_Idx) return Port_Desc
   is
      F : constant Port_Desc_Idx := Get_Output_First_Desc (M);
      pragma Assert (O < Get_Nbr_Outputs (M));
   begin
      return Get_Port_Desc (F + Port_Desc_Idx (O));
   end Get_Output_Desc;

   procedure Set_Input_Desc (M : Module; I : Port_Idx; Desc : Port_Desc)
   is
      F : constant Port_Desc_Idx := Get_Input_First_Desc (M);
      pragma Assert (I < Get_Nbr_Inputs (M));
      Idx : constant Port_Desc_Idx := F + Port_Desc_Idx (I);
   begin
      Set_Port_Desc (Idx, Desc);
   end Set_Input_Desc;

   procedure Set_Output_Desc (M : Module; O : Port_Idx; Desc : Port_Desc)
   is
      F : constant Port_Desc_Idx := Get_Output_First_Desc (M);
      pragma Assert (O < Get_Nbr_Outputs (M));
      Idx : constant Port_Desc_Idx := F + Port_Desc_Idx (O);
   begin
      Set_Port_Desc (Idx, Desc);
   end Set_Output_Desc;

   procedure Set_Ports_Desc (M : Module;
                             Input_Descs : Port_Desc_Array;
                             Output_Descs : Port_Desc_Array)
   is
      pragma Assert (Is_Valid (M));
      pragma Assert (Input_Descs'Length = Get_Nbr_Inputs (M));
      pragma Assert (Output_Descs'Length = Get_Nbr_Outputs (M));
   begin
      for I in Input_Descs'Range loop
         Set_Input_Desc (M, I - Input_Descs'First, Input_Descs (I));
      end loop;

      for O in Output_Descs'Range loop
         Set_Output_Desc (M, O - Output_Descs'First, Output_Descs (O));
      end loop;
   end Set_Ports_Desc;

   --  Param_Desc

   package Param_Desc_Table is new Tables
     (Table_Component_Type => Param_Desc,
      Table_Index_Type => Param_Desc_Idx,
      Table_Low_Bound => No_Param_Desc_Idx,
      Table_Initial => 256);

   procedure Set_Params_Desc (M : Module;
                              Params : Param_Desc_Array)
   is
      pragma Assert (Is_Valid (M));
      pragma Assert (Params'Length = Get_Nbr_Params (M));
   begin
      pragma Assert
        (Modules_Table.Table (M).First_Param_Desc = No_Param_Desc_Idx);

      Modules_Table.Table (M).First_Param_Desc := Param_Desc_Table.Last + 1;

      for P of Params loop
         Param_Desc_Table.Append (P);
      end loop;
   end Set_Params_Desc;

   function Get_Param_Desc (M : Module; Param : Param_Idx) return Param_Desc
   is
      use Netlists.Gates;
      pragma Assert (Is_Valid (M));
   begin
      case Get_Id (M) is
         when Id_Const_Bit
           | Id_Const_Log =>
            return (No_Sname, Param_Uns32);
         when others =>
            pragma Assert (Param < Get_Nbr_Params (M));
            return Param_Desc_Table.Table
              (Modules_Table.Table (M).First_Param_Desc
                 + Param_Desc_Idx (Param));
      end case;
   end Get_Param_Desc;

   function Get_Mark_Flag (Inst : Instance) return Boolean
   is
      pragma Assert (Is_Valid (Inst));
   begin
      return Instances_Table.Table (Inst).Flag_Mark;
   end Get_Mark_Flag;

   procedure Set_Mark_Flag (Inst : Instance; Flag : Boolean)
   is
      pragma Assert (Is_Valid (Inst));
   begin
      Instances_Table.Table (Inst).Flag_Mark := Flag;
   end Set_Mark_Flag;

   function Get_Param_Idx (Inst : Instance; Param : Param_Idx) return Param_Idx
   is
      pragma Assert (Is_Valid (Inst));
      pragma Assert (Param < Get_Nbr_Params (Inst));
   begin
      return Instances_Table.Table (Inst).First_Param + Param;
   end Get_Param_Idx;

   function Get_Param_Uns32 (Inst : Instance; Param : Param_Idx) return Uns32
   is
      pragma Assert (Is_Valid (Inst));
      M : constant Module := Get_Module (Inst);
      pragma Assert (Param < Get_Nbr_Params (Inst));
      pragma Assert (Get_Param_Desc (M, Param).Typ = Param_Uns32);
   begin
      return Params_Table.Table (Get_Param_Idx (Inst, Param));
   end Get_Param_Uns32;

   procedure Set_Param_Uns32 (Inst : Instance; Param : Param_Idx; Val : Uns32)
   is
      pragma Assert (Is_Valid (Inst));
      M : constant Module := Get_Module (Inst);
      pragma Assert (Param < Get_Nbr_Params (Inst));
      pragma Assert (Get_Param_Desc (M, Param).Typ = Param_Uns32);
   begin
      Params_Table.Table (Get_Param_Idx (Inst, Param)) := Val;
   end Set_Param_Uns32;

   function Get_Param_Pval (Inst : Instance; Param : Param_Idx) return Pval
   is
      M : constant Module := Get_Module (Inst);
      pragma Assert (Param < Get_Nbr_Params (Inst));
      pragma Assert (Get_Param_Desc (M, Param).Typ in Param_Types_Pval);
   begin
      return Pval (Params_Table.Table (Get_Param_Idx (Inst, Param)));
   end Get_Param_Pval;

   procedure Set_Param_Pval (Inst : Instance; Param : Param_Idx; Val : Pval)
   is
      M : constant Module := Get_Module (Inst);
      pragma Assert (Param < Get_Nbr_Params (Inst));
      pragma Assert (Get_Param_Desc (M, Param).Typ in Param_Types_Pval);
   begin
      Params_Table.Table (Get_Param_Idx (Inst, Param)) := Uns32 (Val);
   end Set_Param_Pval;

   procedure Connect (I : Input; O : Net)
   is
      pragma Assert (Is_Valid (I));
      pragma Assert (Is_Valid (O));
      --  Check Width compatibility
      --  pragma assert (get_width (i) = get_width (o));
      pragma Assert (Get_Driver (I) = No_Net);
      I_Ent : Input_Record renames Inputs_Table.Table (I);
      O_Ent : Net_Record renames Nets_Table.Table (O);
   begin
      I_Ent.Driver := O;
      I_Ent.Next_Sink := O_Ent.First_Sink;
      O_Ent.First_Sink := I;
   end Connect;

   procedure Disconnect (I : Input)
   is
      pragma Assert (Is_Valid (I));
      Drv : constant Net := Get_Driver (I);
      pragma Assert (Drv /= No_Net);
      Next_Sink : constant Input := Get_Next_Sink (I);
      I_Ent : Input_Record renames Inputs_Table.Table (I);
      D_Ent : Net_Record renames Nets_Table.Table (Drv);
      S, N_S : Input;
   begin
      I_Ent.Next_Sink := No_Input;
      I_Ent.Driver := No_Net;

      if D_Ent.First_Sink = I then
         --  Was the first sink.
         D_Ent.First_Sink := Next_Sink;
      else
         --  Walk
         S := D_Ent.First_Sink;
         loop
            pragma Assert (Is_Valid (S));
            N_S := Get_Next_Sink (S);
            if N_S = I then
               Inputs_Table.Table (S).Next_Sink := Next_Sink;
               exit;
            else
               S := N_S;
            end if;
         end loop;
      end if;
   end Disconnect;

   procedure Redirect_Inputs (Old : Net; N : Net)
   is
      First_I, I : Input;
      Last_I : Input;
   begin
      First_I := Get_First_Sink (Old);
      if First_I = No_Input then
         --  Nothing to do if no input.
         return;
      end if;

      I := First_I;
      Last_I := No_Input;
      while I /= No_Input loop
         declare
            I_Rec : Input_Record renames Inputs_Table.Table (I);
         begin
            pragma Assert (I_Rec.Driver = Old);
            I_Rec.Driver := N;

            Last_I := I;

            I := I_Rec.Next_Sink;
         end;
      end loop;
      Inputs_Table.Table (Last_I).Next_Sink := Get_First_Sink (N);
      Nets_Table.Table (N).First_Sink := First_I;

      --  Also disconnect OLD
      Nets_Table.Table (Old).First_Sink := No_Input;
   end Redirect_Inputs;

   type Pval_Record is record
      Len : Uns32;
      Va_Idx : Uns32;
      Zx_Idx : Uns32;
   end record;

   package Pval_Table is new Tables
     (Table_Component_Type => Pval_Record,
      Table_Index_Type => Pval,
      Table_Low_Bound => 0,
      Table_Initial => 32);

   package Pval_Word_Table is new Tables
     (Table_Component_Type => Uns32,
      Table_Index_Type => Uns32,
      Table_Low_Bound => 0,
      Table_Initial => 32);

   function Create_Pval4 (Len : Uns32) return Pval
   is
      Nwords : constant Uns32 := (Len + 31) / 32;
      Idx : constant Uns32 := Pval_Word_Table.Last + 1;
      Res : Uns32;
   begin
      Pval_Table.Append ((Len => Len,
                          Va_Idx => Idx,
                          Zx_Idx => Idx + Nwords));
      Res := Pval_Word_Table.Allocate (Natural (2 * Nwords));
      pragma Assert (Res = Idx);
      return Pval_Table.Last;
   end Create_Pval4;

   function Create_Pval2 (Len : Uns32) return Pval
   is
      Nwords : constant Uns32 := (Len + 31) / 32;
      Idx : constant Uns32 := Pval_Word_Table.Last + 1;
      Res : Uns32;
   begin
      Pval_Table.Append ((Len => Len,
                          Va_Idx => Idx,
                          Zx_Idx => 0));
      Res := Pval_Word_Table.Allocate (Natural (Nwords));
      pragma Assert (Res = Idx);
      return Pval_Table.Last;
   end Create_Pval2;

   function Get_Pval_Length (P : Pval) return Uns32
   is
      pragma Assert (P <= Pval_Table.Last);
   begin
      return Pval_Table.Table (P).Len;
   end Get_Pval_Length;

   function Read_Pval (P : Pval; Off : Uns32) return Logic_32
   is
      pragma Assert (P <= Pval_Table.Last);
      Pval_Rec : Pval_Record renames Pval_Table.Table (P);
      pragma Assert (Pval_Rec.Len > 0);
      pragma Assert (Off <= (Pval_Rec.Len - 1) / 32);
      Res : Logic_32;
   begin
      Res.Val := Pval_Word_Table.Table (Pval_Rec.Va_Idx + Off);
      if Pval_Rec.Zx_Idx = 0 then
         Res.Zx := 0;
      else
         Res.Zx := Pval_Word_Table.Table (Pval_Rec.Zx_Idx + Off);
      end if;
      return Res;
   end Read_Pval;

   procedure Write_Pval (P : Pval; Off : Uns32; Val : Logic_32)
   is
      pragma Assert (P <= Pval_Table.Last);
      Pval_Rec : Pval_Record renames Pval_Table.Table (P);
      pragma Assert (Pval_Rec.Len > 0);
      pragma Assert (Off <= (Pval_Rec.Len - 1) / 32);
   begin
      Pval_Word_Table.Table (Pval_Rec.Va_Idx + Off) := Val.Val;
      if Pval_Rec.Zx_Idx = 0 then
         pragma Assert (Val.Zx = 0);
         null;
      else
         Pval_Word_Table.Table (Pval_Rec.Zx_Idx + Off) := Val.Zx;
      end if;
   end Write_Pval;

   --  Attributes

   package Attributes_Table is new Tables
     (Table_Component_Type => Attribute_Record,
      Table_Index_Type     => Attribute,
      Table_Low_Bound      => 0,
      Table_Initial        => 64);

   function Instance_Attribute_Hash (Params : Instance)
                                    return Hash_Value_Type is
   begin
      return Hash_Value_Type (Params);
   end Instance_Attribute_Hash;

   function Instance_Attribute_Build (Params : Instance) return Instance is
   begin
      return Params;
   end Instance_Attribute_Build;

   function Instance_Attribute_Build_Value (Obj : Instance) return Attribute
   is
      pragma Unreferenced (Obj);
   begin
      return No_Attribute;
   end Instance_Attribute_Build_Value;

   package Instances_Attribute_Maps is new Dyn_Maps
     (Params_Type => Instance,
      Object_Type => Instance,
      Value_Type => Attribute,
      Hash => Instance_Attribute_Hash,
      Build => Instance_Attribute_Build,
      Build_Value => Instance_Attribute_Build_Value,
      Equal => "=");

   Instances_Attribute_Map : Instances_Attribute_Maps.Instance;

   procedure Set_Instance_Attribute
     (Inst : Instance; Id : Name_Id; Ptype : Param_Type; Pv : Pval)
   is
      pragma Assert (Is_Valid (Inst));
      Attr       : Attribute;
      Idx        : Instances_Attribute_Maps.Index_Type;
      Prev       : Attribute;
   begin
      --  There is now at least one attribute for INST.
      Instances_Table.Table (Inst).Has_Attr := True;

      --  Get (or create and get) an entry for INST.  If created, it will be
      --  No_Attribute (returned by attribute_build_value).
      Instances_Attribute_Maps.Get_Index (Instances_Attribute_Map, Inst, Idx);

      Prev := Instances_Attribute_Maps.Get_Value
        (Instances_Attribute_Map, Idx);
      Attributes_Table.Append ((Name => Id,
                                Typ => Ptype,
                                Val => Pv,
                                Chain => Prev));
      Attr := Attributes_Table.Last;

      Instances_Attribute_Maps.Set_Value (Instances_Attribute_Map, Idx, Attr);
   end Set_Instance_Attribute;

   function Has_Instance_Attribute (Inst : Instance) return Boolean is
   begin
      return Instances_Table.Table (Inst).Has_Attr;
   end Has_Instance_Attribute;

   function Get_Instance_First_Attribute (Inst : Instance) return Attribute
   is
      pragma Assert (Is_Valid (Inst));
   begin
      if not Has_Instance_Attribute (Inst) then
         return No_Attribute;
      end if;
      declare
         Idx   : Instances_Attribute_Maps.Index_Type;
         Res   : Attribute;
      begin
         Instances_Attribute_Maps.Get_Index
           (Instances_Attribute_Map, Inst, Idx);
         Res := Instances_Attribute_Maps.Get_Value
           (Instances_Attribute_Map, Idx);
         return Res;
      end;
   end Get_Instance_First_Attribute;

   function Is_Valid (Attr : Attribute) return Boolean is
   begin
      return Attr > No_Attribute and then Attr <= Attributes_Table.Last;
   end Is_Valid;

   function Get_Attribute_Name (Attr : Attribute) return Name_Id
   is
      pragma Assert (Is_Valid (Attr));
   begin
      return Attributes_Table.Table (Attr).Name;
   end Get_Attribute_Name;

   function Get_Attribute_Type (Attr : Attribute) return Param_Type
   is
      pragma Assert (Is_Valid (Attr));
   begin
      return Attributes_Table.Table (Attr).Typ;
   end Get_Attribute_Type;

   function Get_Attribute_Pval (Attr : Attribute) return Pval
   is
      pragma Assert (Is_Valid (Attr));
   begin
      return Attributes_Table.Table (Attr).Val;
   end Get_Attribute_Pval;

   function Get_Attribute_Next (Attr : Attribute) return Attribute
   is
      pragma Assert (Is_Valid (Attr));
   begin
      return Attributes_Table.Table (Attr).Chain;
   end Get_Attribute_Next;

   function Port_Attribute_Hash (Params : Port_Desc_Idx)
                                return Hash_Value_Type is
   begin
      return Hash_Value_Type (Params);
   end Port_Attribute_Hash;

   function Port_Attribute_Build (Params : Port_Desc_Idx)
                                 return Port_Desc_Idx is
   begin
      return Params;
   end Port_Attribute_Build;

   function Port_Attribute_Build_Value (Obj : Port_Desc_Idx) return Attribute
   is
      pragma Unreferenced (Obj);
   begin
      return No_Attribute;
   end Port_Attribute_Build_Value;

   package Ports_Attribute_Maps is new Dyn_Maps
     (Params_Type => Port_Desc_Idx,
      Object_Type => Port_Desc_Idx,
      Value_Type => Attribute,
      Hash => Port_Attribute_Hash,
      Build => Port_Attribute_Build,
      Build_Value => Port_Attribute_Build_Value,
      Equal => "=");

   Ports_Attribute_Map : Ports_Attribute_Maps.Instance;

   procedure Set_Port_Attribute
     (Port : Port_Desc_Idx; Id : Name_Id; Ptype : Param_Type; Pv : Pval)
   is
      Attr       : Attribute;
      Idx        : Ports_Attribute_Maps.Index_Type;
      Prev       : Attribute;
   begin
      Ports_Attribute_Maps.Get_Index (Ports_Attribute_Map, Port, Idx);

      Prev := Ports_Attribute_Maps.Get_Value (Ports_Attribute_Map, Idx);
      Attributes_Table.Append ((Name => Id,
                                Typ => Ptype,
                                Val => Pv,
                                Chain => Prev));
      Attr := Attributes_Table.Last;

      Ports_Attribute_Maps.Set_Value (Ports_Attribute_Map, Idx, Attr);
   end Set_Port_Attribute;

   procedure Set_Input_Port_Attribute (M : Module;
                                       Port : Port_Idx;
                                       Id : Name_Id;
                                       Ptype : Param_Type;
                                       Pv : Pval)
   is
      Idx  : constant Port_Desc_Idx :=
        Get_Input_First_Desc (M) + Port_Desc_Idx (Port);
   begin
      Set_Port_Attribute (Idx, Id, Ptype, Pv);
   end Set_Input_Port_Attribute;

   procedure Set_Output_Port_Attribute (M : Module;
                                        Port : Port_Idx;
                                        Id : Name_Id;
                                        Ptype : Param_Type;
                                        Pv : Pval)
   is
      Idx  : constant Port_Desc_Idx :=
        Get_Output_First_Desc (M) + Port_Desc_Idx (Port);
   begin
      Set_Port_Attribute (Idx, Id, Ptype, Pv);
   end Set_Output_Port_Attribute;

   function Get_Port_First_Attribute (Port : Port_Desc_Idx) return Attribute
   is
      Idx        : Ports_Attribute_Maps.Index_Type;
      Res        : Attribute;
   begin
      Ports_Attribute_Maps.Get_Index (Ports_Attribute_Map, Port, Idx);
      Res := Ports_Attribute_Maps.Get_Value (Ports_Attribute_Map, Idx);
      return Res;
   end Get_Port_First_Attribute;

   function Get_Input_Port_First_Attribute (M : Module; Port : Port_Idx)
                                           return Attribute
   is
      Idx  : constant Port_Desc_Idx :=
        Get_Input_First_Desc (M) + Port_Desc_Idx (Port);
   begin
      return Get_Port_First_Attribute (Idx);
   end Get_Input_Port_First_Attribute;

   function Get_Output_Port_First_Attribute (M : Module; Port : Port_Idx)
                                            return Attribute
   is
      Idx  : constant Port_Desc_Idx :=
        Get_Output_First_Desc (M) + Port_Desc_Idx (Port);
   begin
      return Get_Port_First_Attribute (Idx);
   end Get_Output_Port_First_Attribute;

   --  Statistics

   function Count_Free_Inputs (Head : Input) return Natural
   is
      Unused : Natural;
      Inp : Input;
   begin
      Unused := 0;
      Inp := Head;
      while Inp /= No_Input loop
         Unused := Unused + 1;
         Inp := Inputs_Table.Table (Inp).Next_Sink;
      end loop;
      return Unused;
   end Count_Free_Inputs;

   procedure Disp_Stats
   is
      use Simple_IO;
      Unused : Natural;
      Nbr_Modules : Module_Counter_Type := (others => 0);
   begin
      Put_Line_Err ("Statistics for netlists:");
      Put_Line_Err
        (" snames:    " & Sname'Image (Snames_Table.Last));
      Put_Line_Err
        (" modules:   " & Module'Image (Modules_Table.Last));

      Put_Err
        (" instances: " & Instance'Image (Instances_Table.Last));
      Unused := 0;
      for I in No_Instance + 1 .. Instances_Table.Last loop
         if Get_Module (I) = Free_Module then
            Unused := Unused + 1;
         end if;
      end loop;
      Put_Line_Err
        (" (free:" & Natural'Image (Unused) & ')');

      Put_Err
        (" nets:      " & Net'Image (Nets_Table.Last));
      Unused := 0;
      for I in No_Net + 1 .. Nets_Table.Last loop
         if Get_Net_Parent (I) = No_Instance then
            Unused := Unused + 1;
         end if;
      end loop;
      Put_Line_Err
        (" (free:" & Natural'Image (Unused) & ')');

      Put_Err
        (" inputs:    " & Input'Image (Inputs_Table.Last));
      Unused := 0;
      for I in No_Input + 1 .. Inputs_Table.Last loop
         if Get_Input_Parent (I) = No_Instance then
            Unused := Unused + 1;
         end if;
      end loop;
      Put_Line_Err
        (" (free:" & Natural'Image (Unused) & ')');
      for I in Free_Inputs'Range loop
         Unused := Count_Free_Inputs (Free_Inputs (I));
         if Unused /= 0 then
            Put_Line_Err ("  free" & Port_Nbr'Image (I) & " inputs:"
                          & Natural'Image (Unused)
                          & " *" & Port_Nbr'Image (I)
                          & " =" & Natural'Image (Unused * Natural (I)));
         end if;
      end loop;

      Put_Line_Err
        (" params:    " & Param_Idx'Image (Params_Table.Last));

      for I in No_Instance + 1 .. Instances_Table.Last loop
         declare
            M : constant Module := Get_Module (I);
         begin
            if M <= Nbr_Modules'Last then
               Nbr_Modules (M) := Nbr_Modules (M) + 1;
            end if;
         end;
      end loop;

      for J in 1 .. 2 loop
         case J is
            when 1 =>
               Put_Line_Err (" Number of instances (per module):");
            when 2 =>
               Put_Line_Err (" Number of freed instances (per module):");
         end case;
         for I in Module_Counter_Type'Range loop
            case J is
               when 1 =>
                  Unused := Nbr_Modules (I);
               when 2 =>
                  Unused := Free_Instances_Counter (I);
            end case;

            if Unused /= 0 then
               declare
                  Name : constant Sname := Get_Module_Name (I);
               begin
                  case Get_Sname_Kind (Name) is
                     when Sname_User
                        | Sname_Artificial =>
                        Put_Err
                          ("  " & Name_Table.Image (Get_Sname_Suffix (Name)));
                     when others =>
                        Put_Err
                          ("  module " & Module_Id'Image (Get_Id (I)));
                  end case;
                  Put_Line_Err (":" & Natural'Image (Unused));
               end;
            end if;
         end loop;
      end loop;
   end Disp_Stats;

begin
   --  Initialize snames_table: create the first entry for No_Sname.
   Snames_Table.Append ((Kind => Sname_Artificial,
                         Prefix => No_Sname,
                         Suffix => 0));
   pragma Assert (Snames_Table.Last = No_Sname);

   Modules_Table.Append ((Parent => No_Module,
                          Name => No_Sname,
                          Id => Id_None,
                          First_Port_Desc => No_Port_Desc_Idx,
                          Nbr_Inputs => 0,
                          Nbr_Outputs => 0,
                          First_Param_Desc => No_Param_Desc_Idx,
                          Nbr_Params => 0,
                          First_Sub_Module => No_Module,
                          Last_Sub_Module => No_Module,
                          Next_Sub_Module => No_Module,
                          First_Instance => No_Instance,
                          Last_Instance => No_Instance));
   pragma Assert (Modules_Table.Last = No_Module);

   Modules_Table.Append ((Parent => No_Module,
                          Name => New_Sname_Artificial (Std_Names.Name_None,
                                                        No_Sname),
                          Id => Id_Free,
                          First_Port_Desc => No_Port_Desc_Idx,
                          Nbr_Inputs => 0,
                          Nbr_Outputs => 0,
                          First_Param_Desc => No_Param_Desc_Idx,
                          Nbr_Params => 0,
                          First_Sub_Module => No_Module,
                          Last_Sub_Module => No_Module,
                          Next_Sub_Module => No_Module,
                          First_Instance => No_Instance,
                          Last_Instance => No_Instance));
   pragma Assert (Modules_Table.Last = Free_Module);

   Instances_Table.Append ((Parent => No_Module,
                            Has_Attr => False,
                            Flag4 => False,
                            Next_Instance => No_Instance,
                            Prev_Instance => No_Instance,
                            Klass => No_Module,
                            Flag5 | Flag6 => False,
                            Flag_Mark => False,
                            Flag2 => False,
                            Name => No_Sname,
                            First_Param => No_Param_Idx,
                            First_Input => No_Input,
                            First_Output => No_Net));
   pragma Assert (Instances_Table.Last = No_Instance);

   Nets_Table.Append ((Parent => No_Instance,
                       First_Sink => No_Input,
                       W => 0));
   pragma Assert (Nets_Table.Last = No_Net);

   Inputs_Table.Append ((Parent => No_Instance,
                         Driver => No_Net,
                         Next_Sink => No_Input));
   pragma Assert (Inputs_Table.Last = No_Input);

   Port_Desc_Table.Append ((Name => No_Sname,
                            Dir => Port_In,
                            W => 0));
   pragma Assert (Port_Desc_Table.Last = No_Port_Desc_Idx);

   Param_Desc_Table.Append ((Name => No_Sname,
                             Typ => Param_Uns32));
   pragma Assert (Param_Desc_Table.Last = No_Param_Desc_Idx);

   Params_Table.Append (0);
   pragma Assert (Params_Table.Last = No_Param_Idx);

   Pval_Table.Append ((Len => 0,
                       Va_Idx => 0,
                       Zx_Idx => 0));
   pragma Assert (Pval_Table.Last = No_Pval);

   Pval_Word_Table.Append (0);

   Attributes_Table.Append ((Name => No_Name_Id,
                             Typ => Param_Invalid,
                             Val => No_Pval,
                             Chain => No_Attribute));
   pragma Assert (Attributes_Table.Last = No_Attribute);

   Ports_Attribute_Maps.Init (Ports_Attribute_Map);

   Instances_Attribute_Maps.Init (Instances_Attribute_Map);
end Netlists;
