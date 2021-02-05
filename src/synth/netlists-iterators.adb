--  Iterators for elements of a netlist.
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

with Netlists.Utils; use Netlists.Utils;

package body Netlists.Iterators is
   function Sub_Modules (M : Module) return Modules_Iterator
   is
      pragma Assert (Is_Valid (M));
   begin
      return Modules_Iterator'(M => M);
   end Sub_Modules;

   function Modules_First (It : Modules_Iterator) return Modules_Cursor is
   begin
      return Modules_Cursor'(M => Get_First_Sub_Module (It.M));
   end Modules_First;

   function Modules_Next (It : Modules_Iterator; Cur : Modules_Cursor)
                         return Modules_Cursor
   is
      pragma Unreferenced (It);
   begin
      return Modules_Cursor'(M => Get_Next_Sub_Module (Cur.M));
   end Modules_Next;

   function Modules_Has_Element (It : Modules_Iterator; Cur : Modules_Cursor)
                                return Boolean
   is
      pragma Unreferenced (It);
   begin
      return Cur.M /= No_Module;
   end Modules_Has_Element;

   function Modules_Element (It : Modules_Iterator; Cur : Modules_Cursor)
                            return Module
   is
      pragma Unreferenced (It);
   begin
      return Cur.M;
   end Modules_Element;

   function Ports_Desc_First (It : Ports_Desc_Iterator)
                             return Ports_Desc_Cursor is
   begin
      return Ports_Desc_Cursor'
        (Idx => Get_First_Port_Desc (It.M),
         Num => Get_Nbr_Inputs (It.M) + Get_Nbr_Outputs (It.M));
   end Ports_Desc_First;

   function Ports_Desc_Next (It : Ports_Desc_Iterator; Cur : Ports_Desc_Cursor)
                            return Ports_Desc_Cursor
   is
      pragma Unreferenced (It);
   begin
      return Ports_Desc_Cursor'(Idx => Cur.Idx + 1,
                                Num => Cur.Num - 1);
   end Ports_Desc_Next;

   function Ports_Desc_Has_Element
     (It : Ports_Desc_Iterator; Cur : Ports_Desc_Cursor) return Boolean
   is
      pragma Unreferenced (It);
   begin
      return Cur.Num > 0;
   end Ports_Desc_Has_Element;

   function Ports_Desc_Element
     (It : Ports_Desc_Iterator; Cur : Ports_Desc_Cursor) return Port_Desc
   is
      pragma Unreferenced (It);
   begin
      return Get_Port_Desc (Cur.Idx);
   end Ports_Desc_Element;

   function Ports_Desc (M : Module) return Ports_Desc_Iterator
   is
      pragma Assert (Is_Valid (M));
   begin
      return Ports_Desc_Iterator'(M => M);
   end Ports_Desc;

   function Params_Desc_First (It : Params_Desc_Iterator)
                              return Params_Desc_Cursor is
   begin
      return Params_Desc_Cursor'
        (Idx => 0,
         Num => Get_Nbr_Params (It.M));
   end Params_Desc_First;

   function Params_Desc_Next
     (It : Params_Desc_Iterator; Cur : Params_Desc_Cursor)
     return Params_Desc_Cursor
   is
      pragma Unreferenced (It);
   begin
      return Params_Desc_Cursor'(Idx => Cur.Idx + 1,
                                 Num => Cur.Num - 1);
   end Params_Desc_Next;

   function Params_Desc_Has_Element
     (It : Params_Desc_Iterator; Cur : Params_Desc_Cursor) return Boolean
   is
      pragma Unreferenced (It);
   begin
      return Cur.Num > 0;
   end Params_Desc_Has_Element;

   function Params_Desc_Element
     (It : Params_Desc_Iterator; Cur : Params_Desc_Cursor) return Param_Desc is
   begin
      return Get_Param_Desc (It.M, Cur.Idx);
   end Params_Desc_Element;

   function Params_Desc (M : Module) return Params_Desc_Iterator is
   begin
      return Params_Desc_Iterator'(M => M);
   end Params_Desc;

   function Instances_First (It : Instances_Iterator)
                            return Instances_Cursor is
   begin
      return Instances_Cursor'(Inst => Get_First_Instance (It.M));
   end Instances_First;

   function Instances_Next (It : Instances_Iterator; Cur : Instances_Cursor)
                           return Instances_Cursor
   is
      pragma Unreferenced (It);
   begin
      return Instances_Cursor'(Inst => Get_Next_Instance (Cur.Inst));
   end Instances_Next;

   function Instances_Has_Element
     (It : Instances_Iterator; Cur : Instances_Cursor) return Boolean
   is
      pragma Unreferenced (It);
   begin
      return Cur.Inst /= No_Instance;
   end Instances_Has_Element;

   function Instances_Element
     (It : Instances_Iterator; Cur : Instances_Cursor) return Instance
   is
      pragma Unreferenced (It);
   begin
      return Cur.Inst;
   end Instances_Element;

   function Instances (M : Module) return Instances_Iterator is
   begin
      return Instances_Iterator'(M => M);
   end Instances;

   function Inputs_First (It : Inputs_Iterator) return Inputs_Cursor is
   begin
      return Inputs_Cursor'(Idx => 0,
                            Nbr => Get_Nbr_Inputs (It.Inst));
   end Inputs_First;

   function Inputs_Next (It : Inputs_Iterator; Cur : Inputs_Cursor)
                        return Inputs_Cursor
   is
      pragma Unreferenced (It);
   begin
      return Inputs_Cursor'(Idx => Cur.Idx + 1,
                            Nbr => Cur.Nbr);
   end Inputs_Next;

   function Inputs_Has_Element (It : Inputs_Iterator; Cur : Inputs_Cursor)
                               return Boolean
   is
      pragma Unreferenced (It);
   begin
      return Cur.Idx < Cur.Nbr;
   end Inputs_Has_Element;

   function Inputs_Element (It : Inputs_Iterator; Cur : Inputs_Cursor)
                           return Input is
   begin
      return Get_Input (It.Inst, Cur.Idx);
   end Inputs_Element;

   function Inputs (Inst : Instance) return Inputs_Iterator is
   begin
      return Inputs_Iterator'(Inst => Inst);
   end Inputs;

   function Outputs_First (It : Outputs_Iterator) return Outputs_Cursor is
   begin
      return Outputs_Cursor'(Idx => 0,
                             Nbr => Get_Nbr_Outputs (It.Inst));
   end Outputs_First;

   function Outputs_Next (It : Outputs_Iterator; Cur : Outputs_Cursor)
                        return Outputs_Cursor
   is
      pragma Unreferenced (It);
   begin
      return Outputs_Cursor'(Idx => Cur.Idx + 1,
                             Nbr => Cur.Nbr);
   end Outputs_Next;

   function Outputs_Has_Element (It : Outputs_Iterator; Cur : Outputs_Cursor)
                               return Boolean
   is
      pragma Unreferenced (It);
   begin
      return Cur.Idx < Cur.Nbr;
   end Outputs_Has_Element;

   function Outputs_Element (It : Outputs_Iterator; Cur : Outputs_Cursor)
                            return Net is
   begin
      return Get_Output (It.Inst, Cur.Idx);
   end Outputs_Element;

   function Outputs (Inst : Instance) return Outputs_Iterator is
   begin
      return Outputs_Iterator'(Inst => Inst);
   end Outputs;

   function Params_First (It : Params_Iterator) return Params_Cursor is
   begin
      return Params_Cursor'(Idx => 0,
                            Nbr => Get_Nbr_Params (It.Inst));
   end Params_First;

   function Params_Next (It : Params_Iterator; Cur : Params_Cursor)
                        return Params_Cursor
   is
      pragma Unreferenced (It);
   begin
      return Params_Cursor'(Idx => Cur.Idx + 1,
                            Nbr => Cur.Nbr - 1);
   end Params_Next;

   function Params_Has_Element (It : Params_Iterator; Cur : Params_Cursor)
                               return Boolean
   is
      pragma Unreferenced (It);
   begin
      return Cur.Nbr > 0;
   end Params_Has_Element;

   function Params (Inst : Instance) return Params_Iterator is
   begin
      return Params_Iterator'(Inst => Inst);
   end Params;

   function Get_Param_Idx (Cur : Params_Cursor) return Param_Idx is
   begin
      return Cur.Idx;
   end Get_Param_Idx;

   function Nets_First (It : Nets_Iterator) return Nets_Cursor
   is
      Inst : Instance;
      Num : Port_Nbr;
   begin
      Inst := Get_Self_Instance (It.M);
      loop
         if Inst = No_Instance then
            --  No instance.
            return Nets_Cursor'(Inst => No_Instance,
                                N => No_Net,
                                Num => 0);
         end if;
         Num := Get_Nbr_Outputs (Inst);
         if Num = 0 then
            --  No output for this instance.
            Inst := Get_Next_Instance (Inst);
         else
            return Nets_Cursor'(Inst => Inst,
                                N => Get_First_Output (Inst),
                                Num => Num);
         end if;
      end loop;
   end Nets_First;

   function Nets_Next (It : Nets_Iterator; Cur : Nets_Cursor)
                      return Nets_Cursor
   is
      pragma Unreferenced (It);
   begin
      if Cur.Num > 1 then
         --  Next net for the instance.
         return Nets_Cursor'(Inst => Cur.Inst,
                             N => Cur.N + 1,
                             Num => Cur.Num - 1);
      else
         --  Next instance.
         declare
            Inst : Instance;
            Num : Port_Nbr;
         begin
            Inst := Cur.Inst;
            loop
               Inst := Get_Next_Instance (Inst);
               exit when Inst = No_Instance;
               Num := Get_Nbr_Outputs (Inst);
               if Num /= 0 then
                  return Nets_Cursor'(Inst => Inst,
                                      N => Get_First_Output (Inst),
                                      Num => Num);
               end if;
            end loop;
         end;
         return Nets_Cursor'(Inst => No_Instance,
                             N => No_Net,
                             Num => 0);
      end if;
   end Nets_Next;

   function Nets_Has_Element (It : Nets_Iterator; Cur : Nets_Cursor)
                             return Boolean
   is
      pragma Unreferenced (It);
   begin
      return Cur.Num > 0 or Cur.Inst /= No_Instance;
   end Nets_Has_Element;

   function Nets_Element (It : Nets_Iterator; Cur : Nets_Cursor)
                         return Net
   is
      pragma Unreferenced (It);
   begin
      return Cur.N;
   end Nets_Element;

   function Nets (M : Module) return Nets_Iterator
   is
      pragma Assert (Is_Valid (M));
   begin
      return Nets_Iterator'(M => M);
   end Nets;

   function Sinks_First (It : Sinks_Iterator) return Sinks_Cursor is
   begin
      return Sinks_Cursor'(S => Get_First_Sink (It.N));
   end Sinks_First;

   function Sinks_Next (It : Sinks_Iterator; Cur : Sinks_Cursor)
                       return Sinks_Cursor
   is
      pragma Unreferenced (It);
   begin
      return Sinks_Cursor'(S => Get_Next_Sink (Cur.S));
   end Sinks_Next;

   function Sinks_Has_Element (It : Sinks_Iterator; Cur : Sinks_Cursor)
                              return Boolean
   is
      pragma Unreferenced (It);
   begin
      return Cur.S /= No_Input;
   end Sinks_Has_Element;

   function Sinks_Element (It : Sinks_Iterator; Cur : Sinks_Cursor)
                          return Input
   is
      pragma Unreferenced (It);
   begin
      return Cur.S;
   end Sinks_Element;

   function Sinks (N : Net) return Sinks_Iterator is
   begin
      pragma Assert (Is_Valid (N));
      return Sinks_Iterator'(N => N);
   end Sinks;

end Netlists.Iterators;
