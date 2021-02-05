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

package Netlists.Iterators is
   --  Iterators.

   --  Iterate over sub-modules.
   type Modules_Cursor is private;
   type Modules_Iterator is private with
     Iterable => (First => Modules_First,
                  Next => Modules_Next,
                  Has_Element => Modules_Has_Element,
                  Element => Modules_Element);
   function Sub_Modules (M : Module) return Modules_Iterator;

   function Modules_First (It : Modules_Iterator) return Modules_Cursor
     with Inline;
   function Modules_Next (It : Modules_Iterator; Cur : Modules_Cursor)
                         return Modules_Cursor
     with Inline;
   function Modules_Has_Element (It : Modules_Iterator; Cur : Modules_Cursor)
                                return Boolean
     with Inline;
   function Modules_Element (It : Modules_Iterator; Cur : Modules_Cursor)
                            return Module
     with Inline;

   --  Iterate over ports desc of a module.
   type Ports_Desc_Cursor is private;
   type Ports_Desc_Iterator is private with
     Iterable => (First => Ports_Desc_First,
                  Next => Ports_Desc_Next,
                  Has_Element => Ports_Desc_Has_Element,
                  Element => Ports_Desc_Element);
   function Ports_Desc (M : Module) return Ports_Desc_Iterator;

   function Ports_Desc_First (It : Ports_Desc_Iterator)
                             return Ports_Desc_Cursor;
   function Ports_Desc_Next (It : Ports_Desc_Iterator; Cur : Ports_Desc_Cursor)
                            return Ports_Desc_Cursor;
   function Ports_Desc_Has_Element
     (It : Ports_Desc_Iterator; Cur : Ports_Desc_Cursor) return Boolean;
   function Ports_Desc_Element
     (It : Ports_Desc_Iterator; Cur : Ports_Desc_Cursor) return Port_Desc;

   --  Iterate over param desc of a module
   type Params_Desc_Cursor is private;
   type Params_Desc_Iterator is private with
     Iterable => (First => Params_Desc_First,
                  Next => Params_Desc_Next,
                  Has_Element => Params_Desc_Has_Element,
                  Element => Params_Desc_Element);
   function Params_Desc (M : Module) return Params_Desc_Iterator;

   function Params_Desc_First (It : Params_Desc_Iterator)
                             return Params_Desc_Cursor;
   function Params_Desc_Next
     (It : Params_Desc_Iterator; Cur : Params_Desc_Cursor)
     return Params_Desc_Cursor;
   function Params_Desc_Has_Element
     (It : Params_Desc_Iterator; Cur : Params_Desc_Cursor) return Boolean;
   function Params_Desc_Element
     (It : Params_Desc_Iterator; Cur : Params_Desc_Cursor) return Param_Desc;

   --  Iterate over instances in a module, excluding the self-instance.
   type Instances_Cursor is private;
   type Instances_Iterator is private with
     Iterable => (First => Instances_First,
                  Next => Instances_Next,
                  Has_Element => Instances_Has_Element,
                  Element => Instances_Element);
   function Instances (M : Module) return Instances_Iterator;

   function Instances_First (It : Instances_Iterator) return Instances_Cursor;
   function Instances_Next (It : Instances_Iterator; Cur : Instances_Cursor)
                           return Instances_Cursor;
   function Instances_Has_Element
     (It : Instances_Iterator; Cur : Instances_Cursor) return Boolean;
   function Instances_Element
     (It : Instances_Iterator; Cur : Instances_Cursor) return Instance;

   --  Iterate over inputs of an instance.
   type Inputs_Cursor is private;
   type Inputs_Iterator is private with
     Iterable => (First => Inputs_First,
                  Next => Inputs_Next,
                  Has_Element => Inputs_Has_Element,
                  Element => Inputs_Element);
   function Inputs (Inst : Instance) return Inputs_Iterator;

   function Inputs_First (It : Inputs_Iterator) return Inputs_Cursor;
   function Inputs_Next (It : Inputs_Iterator; Cur : Inputs_Cursor)
                        return Inputs_Cursor;
   function Inputs_Has_Element (It : Inputs_Iterator; Cur : Inputs_Cursor)
                               return Boolean;
   function Inputs_Element (It : Inputs_Iterator; Cur : Inputs_Cursor)
                           return Input;

   --  Iterate over outputs of an instance.
   type Outputs_Cursor is private;
   type Outputs_Iterator is private with
     Iterable => (First => Outputs_First,
                  Next => Outputs_Next,
                  Has_Element => Outputs_Has_Element,
                  Element => Outputs_Element);
   function Outputs (Inst : Instance) return Outputs_Iterator;

   function Outputs_First (It : Outputs_Iterator) return Outputs_Cursor;
   function Outputs_Next (It : Outputs_Iterator; Cur : Outputs_Cursor)
                        return Outputs_Cursor;
   function Outputs_Has_Element (It : Outputs_Iterator; Cur : Outputs_Cursor)
                               return Boolean;
   function Outputs_Element (It : Outputs_Iterator; Cur : Outputs_Cursor)
                           return Net;

   --  Iterate over parameters of an instance.
   type Params_Cursor is private;
   type Params_Iterator is private with
     Iterable => (First => Params_First,
                  Next => Params_Next,
                  Has_Element => Params_Has_Element);
   function Params (Inst : Instance) return Params_Iterator;
   function Get_Param_Idx (Cur : Params_Cursor) return Param_Idx;

   function Params_First (It : Params_Iterator) return Params_Cursor;
   function Params_Next (It : Params_Iterator; Cur : Params_Cursor)
                        return Params_Cursor;
   function Params_Has_Element (It : Params_Iterator; Cur : Params_Cursor)
                               return Boolean;

   --  Iterate over nets of a module.
   type Nets_Cursor is private;
   type Nets_Iterator is private with
     Iterable => (First => Nets_First,
                  Next => Nets_Next,
                  Has_Element => Nets_Has_Element,
                  Element => Nets_Element);
   function Nets (M : Module) return Nets_Iterator;

   function Nets_First (It : Nets_Iterator) return Nets_Cursor;
   function Nets_Next (It : Nets_Iterator; Cur : Nets_Cursor)
                      return Nets_Cursor;
   function Nets_Has_Element (It : Nets_Iterator; Cur : Nets_Cursor)
                             return Boolean;
   function Nets_Element (It : Nets_Iterator; Cur : Nets_Cursor)
                         return Net;

   --  Iterate over sinks of a net.
   type Sinks_Cursor is private;
   type Sinks_Iterator is private with
     Iterable => (First => Sinks_First,
                  Next => Sinks_Next,
                  Has_Element => Sinks_Has_Element,
                  Element => Sinks_Element);
   function Sinks (N : Net) return Sinks_Iterator;

   function Sinks_First (It : Sinks_Iterator) return Sinks_Cursor;
   function Sinks_Next (It : Sinks_Iterator; Cur : Sinks_Cursor)
                       return Sinks_Cursor;
   function Sinks_Has_Element (It : Sinks_Iterator; Cur : Sinks_Cursor)
                              return Boolean;
   function Sinks_Element (It : Sinks_Iterator; Cur : Sinks_Cursor)
                          return Input;

private
   type Modules_Cursor is record
      M : Module;
   end record;

   type Modules_Iterator is record
      M : Module;
   end record;

   type Ports_Desc_Iterator is record
      M : Module;
   end record;

   type Ports_Desc_Cursor is record
      Idx : Port_Desc_Idx;
      Num : Port_Nbr;
   end record;

   type Params_Desc_Iterator is record
      M : Module;
   end record;

   type Params_Desc_Cursor is record
      Idx : Param_Idx;
      Num : Param_Nbr;
   end record;

   type Instances_Iterator is record
      M : Module;
   end record;

   type Instances_Cursor is record
      Inst : Instance;
   end record;

   type Inputs_Cursor is record
      Idx : Port_Idx;
      Nbr : Port_Nbr;
   end record;

   type Inputs_Iterator is record
      Inst : Instance;
   end record;

   type Outputs_Cursor is record
      Idx : Port_Idx;
      Nbr : Port_Nbr;
   end record;

   type Outputs_Iterator is record
      Inst : Instance;
   end record;

   type Params_Cursor is record
      Idx : Param_Idx;
      Nbr : Param_Nbr;
   end record;

   type Params_Iterator is record
      Inst : Instance;
   end record;

   type Nets_Cursor is record
      Inst : Instance;
      N : Net;
      Num : Port_Nbr;
   end record;

   type Nets_Iterator is record
      M : Module;
   end record;

   type Sinks_Cursor is record
      S : Input;
   end record;

   type Sinks_Iterator is record
      N : Net;
   end record;
end Netlists.Iterators;
