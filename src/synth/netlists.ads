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

with Types; use Types;
with Hash; use Hash;

package Netlists is
   --  Netlists.
   --
   --  A netlist is a graph of gates and nets.  This implementation has some
   --  particularities:
   --  * the nets are vectors of bits, and a net of one bit is in fact a
   --    vector of net 1.  Vectors only have a width, their bounds are
   --    from (width - 1 downto 0) or [width-1:0].
   --  * there is no separate data structures for nets, so nets are in
   --    fact the outputs of gates.  So there is no standalone nets, a gate
   --    is needed to have a net.

   --  Names.
   --  As there are many artificial and hierarchical names in a netlist, names
   --  are not flat: it is possible to create a new name using an existing one
   --  without copying the whole prefix.
   type Sname_Kind is
     (
      --  The name adds a suffix to an existing name.  Simple names (without
      --  prefix) are in this kind, with a null prefix.
      Sname_User,
      Sname_Artificial,

      --  Create a new version of an existing prefix.
      Sname_Version
     );
   pragma Convention (C, Sname_Kind);

   type Sname is private;
   No_Sname : constant Sname;

   --  Create an Sname.
   --  There is no unification: these routines always create a new name.  There
   --  is no check that the name already exists, so these routines may create
   --  a duplicate name.  Callers must ensure they create uniq names.
   function New_Sname_User (Id : Name_Id; Prefix : Sname) return Sname;
   function New_Sname_Artificial (Id : Name_Id; Prefix : Sname) return Sname;
   function New_Sname_Version (Ver : Uns32; Prefix : Sname) return Sname;

   --  Read the content of an Sname.
   function Get_Sname_Kind (Name : Sname) return Sname_Kind;
   function Get_Sname_Prefix (Name : Sname) return Sname;
   function Get_Sname_Suffix (Name : Sname) return Name_Id;
   function Get_Sname_Version (Name : Sname) return Uns32;

   --  Modifies an Sname.
   procedure Set_Sname_Prefix (Name : Sname; Prefix : Sname);

   --  TODO: procedure to free an Sname.

   --  Module.
   --
   --  A module represent an uninstantiated netlist.  It is composed of nets
   --  and instances.
   --
   --  From the outside, a module has ports (inputs and outputs), and
   --  optionally parameters.  A module must have at least one port.  Both
   --  ports and parameters have names.
   --
   --  From a module, you can get the list of ports, and the list of instances.
   --  Instances have names.
   --
   --  In a module, there is a special instance (the self one) one that
   --  represent the ports of the module itself, but with the opposite
   --  direction.  Using this trick, there is no difference between ports of
   --  instances and ports of the module itself.
   --
   --  In some cases, you also want to read an output port.  This is
   --  not possible in this model, so just add an 'output' gate that
   --  is a nop but provides a net.
   --
   --  Some modules are predefined and therefore have no inner description.
   --  These are the well known elementary gates.
   type Module is private;
   No_Module : constant Module;

   --  An instance is an instantiated module within a module.  It is
   --  connected.
   type Instance is private;
   No_Instance : constant Instance;

   type Instance_Array is array (Int32 range <>) of Instance;

   --  Hash INST (simply return its index).
   function Hash (Inst : Instance) return Hash_Value_Type;

   --  A net is an output of a gate or a sub-circuit.  A net can be connected
   --  to several inputs.
   type Net is private;
   No_Net : constant Net;

   --  So pervasive that it is worth defining this array here.
   type Net_Array is array (Int32 range <>) of Net;

   type Input is private;
   No_Input : constant Input;

   --  Witdh of a net, ie number of bits.
   --  No_Width (value 0) is reserved to mean unknown.  This is allowed only to
   --  describe the width of predefined gates (like and) so that the same
   --  module can be used for any width.
   subtype Width is Uns32;
   No_Width : constant Width := 0;

   type Port_Kind is (Port_In, Port_Out, Port_Inout);

   --  Each module has a numeric identifier that can be used to easily identify
   --  a module.  Gates (and, or, ...) have reserved identifiers.
   type Module_Id is new Uns32;

   --  Reserved id for no identifier.
   Id_None : constant Module_Id := 0;

   --  Unused instance: free instance but still linked.
   Id_Free : constant Module_Id := 1;

   --  Reserved id for a design (top-level module without ports that contains
   --  other modules).
   Id_Design : constant Module_Id := 2;

   --  First id for user.
   Id_User_None  : constant Module_Id := 128;
   Id_User_Parameters : constant Module_Id := 129;
   Id_User_First : constant Module_Id := Id_User_Parameters + 1;

   --  Port index.  Starts at 0.
   type Port_Nbr is new Uns32;
   subtype Port_Idx is Port_Nbr range 0 .. Port_Nbr'Last - 1;

   type Port_Desc is record
      --  Name of the port.
      Name : Sname;

      Dir : Port_Kind;

      --  Port width (number of bits).
      W : Width;
   end record;
   pragma Pack (Port_Desc);
   pragma Convention (C, Port_Desc);

   type Port_Desc_Array is array (Port_Idx range <>) of Port_Desc;

   type Param_Idx is new Uns32;
   No_Param_Idx : constant Param_Idx := 0;

   subtype Param_Nbr is Param_Idx range 0 .. Param_Idx'Last - 1;

   --  Type of a parameter.  As this is defined in a module, this is valid for
   --  all instances.
   --  NOTE: the corresponding C enum is built from this type using the
   --  'Param_' prefix and indentation.
   type Param_Type is
     (
      Param_Invalid,

      --  An unsigned 32 bit value.
      Param_Uns32,

      --  A Generic value (with a hint of the type).  This is a bit/logic
      --  vector.
      --  TODO: Replace Integer with Signed/Unsigned.
      Param_Pval_Vector,
      Param_Pval_String,
      Param_Pval_Integer,
      Param_Pval_Real,
      Param_Pval_Time_Ps,
      Param_Pval_Boolean
     );
   pragma Convention (C, Param_Type);

   subtype Param_Types_Pval is
     Param_Type range Param_Pval_Vector .. Param_Pval_Boolean;

   type Param_Desc is record
      --  Name of the parameter
      Name : Sname;

      --  Type of the parameter
      Typ : Param_Type;
   end record;

   type Param_Desc_Array is array (Param_Idx range <>) of Param_Desc;

   --  Parameter value.
   type Pval is private;
   No_Pval : constant Pval;

   --  Attribute of an instance.
   type Attribute is private;

   --  Subprograms for modules.
   function New_Design (Name : Sname) return Module;
   function New_User_Module (Parent : Module;
                             Name : Sname;
                             Id : Module_Id;
                             Nbr_Inputs : Port_Nbr;
                             Nbr_Outputs : Port_Nbr;
                             Nbr_Params : Param_Nbr := 0)
                            return Module;
   procedure Set_Input_Desc (M : Module; I : Port_Idx; Desc : Port_Desc);
   procedure Set_Output_Desc (M : Module; O : Port_Idx; Desc : Port_Desc);
   procedure Set_Ports_Desc (M : Module;
                             Input_Descs : Port_Desc_Array;
                             Output_Descs : Port_Desc_Array);
   procedure Set_Params_Desc (M : Module;
                              Params : Param_Desc_Array);

   --  Be sure the record is passed by reference.
   pragma Convention (C, Set_Input_Desc);
   pragma Convention (C, Set_Output_Desc);

   --  Create the self instance, once ports are defined.  This is required if
   --  the internal netlist will be defined.
   function Create_Self_Instance (M : Module) return Instance;

   function Get_Module_Name (M : Module) return Sname;
   function Get_Id (M : Module) return Module_Id;

   --  Number of fixed inputs/outputs.
   --  For gates with variable number of inputs (like Concatn), use the
   --  functions from Netlists.Utils.
   function Get_Nbr_Inputs (M : Module) return Port_Nbr;
   function Get_Nbr_Outputs (M : Module) return Port_Nbr;

   function Get_Nbr_Params (M : Module) return Param_Nbr;

   function Get_Input_Desc (M : Module; I : Port_Idx) return Port_Desc;
   function Get_Output_Desc (M : Module; O : Port_Idx) return Port_Desc;

   function Get_Param_Desc (M : Module; Param : Param_Idx) return Param_Desc;

   function Get_Self_Instance (M : Module) return Instance;
   function Get_First_Instance (M : Module) return Instance;

   --  Linked list of sub-modules.
   --  Use Modules to iterate.
   function Get_First_Sub_Module (M : Module) return Module;
   function Get_Next_Sub_Module (M : Module) return Module;

   --  Instance
   function New_Instance (Parent : Module; M : Module; Name : Sname)
                         return Instance;
   --  For instances non-fixed number of inputs/outputs/params.
   function New_Var_Instance (Parent : Module;
                              M : Module;
                              Name : Sname;
                              Nbr_Inputs : Port_Nbr;
                              Nbr_Outputs : Port_Nbr;
                              Nbr_Params : Param_Nbr)
                             return Instance;

   --  Remove and free the unconnected instance INST.
   procedure Remove_Instance (Inst : Instance);

   function Is_Self_Instance (I : Instance) return Boolean;
   function Get_Module (Inst : Instance) return Module;
   function Get_Instance_Name (Inst : Instance) return Sname;
   function Get_Instance_Parent (Inst : Instance) return Module;
   function Get_Output (Inst : Instance; Idx : Port_Idx) return Net;
   function Get_Input (Inst : Instance; Idx : Port_Idx) return Input;
   function Get_Next_Instance (Inst : Instance) return Instance;

   function Get_Param_Uns32 (Inst : Instance; Param : Param_Idx) return Uns32;
   procedure Set_Param_Uns32 (Inst : Instance; Param : Param_Idx; Val : Uns32);

   function Get_Param_Pval (Inst : Instance; Param : Param_Idx) return Pval;
   procedure Set_Param_Pval (Inst : Instance; Param : Param_Idx; Val : Pval);

   --  Each instance has a mark flag available for any algorithm.
   --  Please leave this flag clean for the next user.
   function Get_Mark_Flag (Inst : Instance) return Boolean;
   procedure Set_Mark_Flag (Inst : Instance; Flag : Boolean);

   --  Input
   function Get_Input_Parent (I : Input) return Instance;
   function Get_Port_Idx (I : Input) return Port_Idx;
   function Get_Driver (I : Input) return Net;
   function Get_Next_Sink (I : Input) return Input;

   --  Net (Output)
   function Get_Net_Parent (O : Net) return Instance;
   function Get_Port_Idx (O : Net) return Port_Idx;
   function Get_First_Sink (O : Net) return Input;
   function Get_Width (N : Net) return Width;

   --  Set the width of a net.  This operation is possible only if the width
   --  is unknown.
   procedure Set_Width (N : Net; W : Width);

   --  Connections.
   procedure Connect (I : Input; O : Net);
   procedure Disconnect (I : Input);

   --  Reconnect all sinks of OLD to N.
   procedure Redirect_Inputs (Old : Net; N : Net);

   --  For Pval.
   --  Create a 4-state Pval.  LEN is the number of bits (cannot be 0).
   function Create_Pval4 (Len : Uns32) return Pval;
   --  Create a 2-state Pval.  The value cannot have X or Z.
   function Create_Pval2 (Len : Uns32) return Pval;
   function Get_Pval_Length (P : Pval) return Uns32;

   --  OFF is the word offset, from 0 to (len - 1) / 32.
   function Read_Pval (P : Pval; Off : Uns32) return Logic_32;
   procedure Write_Pval (P : Pval; Off : Uns32; Val : Logic_32);

   --  Add an attribute to INST.
   procedure Set_Instance_Attribute
     (Inst : Instance; Id : Name_Id; Ptype : Param_Type; Pv : Pval);

   procedure Set_Input_Port_Attribute (M : Module;
                                       Port : Port_Idx;
                                       Id : Name_Id;
                                       Ptype : Param_Type;
                                       Pv : Pval);
   procedure Set_Output_Port_Attribute (M : Module;
                                        Port : Port_Idx;
                                        Id : Name_Id;
                                        Ptype : Param_Type;
                                        Pv : Pval);

   --  Return the first attribute for INST.  Returns No_Attribute if none.
   function Get_Instance_First_Attribute (Inst : Instance) return Attribute;
   function Get_Input_Port_First_Attribute (M : Module; Port : Port_Idx)
                                           return Attribute;
   function Get_Output_Port_First_Attribute (M : Module; Port : Port_Idx)
                                            return Attribute;

   --  Get name/type/value of an attribute.
   function Get_Attribute_Name (Attr : Attribute) return Name_Id;
   function Get_Attribute_Type (Attr : Attribute) return Param_Type;
   function Get_Attribute_Pval (Attr : Attribute) return Pval;

   --  Get the next attribute for the same instance.
   function Get_Attribute_Next (Attr : Attribute) return Attribute;

   --  Return True iff INST has at least one attribute.
   function Has_Instance_Attribute (Inst : Instance) return Boolean;

   --  Display some usage stats on the standard error.
   procedure Disp_Stats;
private
   type Sname is new Uns32 range 0 .. 2**30 - 1;
   No_Sname : constant Sname := 0;

   --  Just to confirm.
   for Port_Desc'Size use 64;

   --  We don't care about C compatible representation of Sname_Record.
   pragma Warnings (Off, "*convention*");
   type Sname_Record is record
      Kind : Sname_Kind;
      Prefix : Sname;
      Suffix : Uns32;
   end record;
   pragma Pack (Sname_Record);
   for Sname_Record'Size use 2*32;
   pragma Warnings (On, "*convention*");

   type Module is mod 2**30;
   No_Module : constant Module := 0;
   Free_Module : constant Module := 1;

   function Is_Valid (M : Module) return Boolean;

   type Port_Desc_Idx is new Uns32;
   No_Port_Desc_Idx : constant Port_Desc_Idx := 0;

   type Param_Desc_Idx is new Uns32;
   No_Param_Desc_Idx : constant Param_Desc_Idx := 0;

   type Input is new Uns32;
   No_Input : constant Input := 0;

   type Net is new Uns32;
   No_Net : constant Net := 0;

   type Instance is new Uns32;
   No_Instance : constant Instance := 0;

   type Attribute is new Uns32;
   No_Attribute : Attribute := 0;

   type Attribute_Record is record
      Name  : Name_Id;
      Val   : Pval;
      Typ   : Param_Type;
      Chain : Attribute;
   end record;

   type Module_Record is record
      Parent           : Module;
      Name             : Sname;
      Id               : Module_Id;
      First_Port_Desc  : Port_Desc_Idx;
      Nbr_Inputs       : Port_Nbr;
      Nbr_Outputs      : Port_Nbr;
      First_Param_Desc : Param_Desc_Idx;
      Nbr_Params       : Param_Nbr;

      --  First sub-module child.
      First_Sub_Module : Module;
      Last_Sub_Module  : Module;

      --  Sub-module brother.
      Next_Sub_Module : Module;

      --  List of instances.
      --  The self instance is the first instance.
      --  FIXME: use an array instead ?
      First_Instance : Instance;
      Last_Instance  : Instance;
   end record;

   function Get_First_Port_Desc (M : Module) return Port_Desc_Idx;
   function Get_First_Output (Inst : Instance) return Net;
   function Get_Port_Desc (Idx : Port_Desc_Idx) return Port_Desc;

   function Is_Valid (I : Instance) return Boolean;

   type Instance_Record is record
      --  The instance is instantiated in Parent.
      Parent   : Module;
      Has_Attr : Boolean;  --  Set when there is at least one attribute.
      Flag4    : Boolean;

      --  Instances are in a doubly-linked list.
      Prev_Instance : Instance;
      Next_Instance : Instance;

      --  For a self-instance, Klass is equal to Parent, and Name is No_Sname.
      Klass     : Module;
      Flag5     : Boolean;
      Flag6     : Boolean;

      Name      : Sname;
      Flag_Mark : Boolean;
      Flag2     : Boolean;

      First_Param  : Param_Idx;
      First_Input  : Input;
      First_Output : Net;
   end record;
   pragma Pack (Instance_Record);
   for Instance_Record'Size use 8*32;

   procedure Set_Next_Instance (Inst : Instance; Next : Instance);
   procedure Set_Prev_Instance (Inst : Instance; Prev : Instance);

   --  Procedures to rewrite the list of instances of a module:
   --  * first extract the chain of instances from module M (and reset the
   --    list of instances - so there is none),
   --  * then add the ones to keep.
   --  The list of instances is walked by using Get_Next_Instance.
   procedure Extract_All_Instances (M : Module; First_Instance : out Instance);
   procedure Append_Instance (M : Module; Inst : Instance);

   --  Extract INST from the list of instance of its module.
   --  Will still be connected, but won't appear anymore in the list of
   --  instances.
   --  Once extracted, the instance is not in a consistent state anymore.  So
   --  it should be either fully disconnected and freed or re-inserted in the
   --  parent module.
   procedure Extract_Instance (Inst : Instance);

   --  Mark INST as free.  Must be unconnected and removed from its module.
   procedure Free_Instance (Inst : Instance);

   type Input_Record is record
      Parent : Instance;
      Driver : Net;
      Next_Sink : Input;
   end record;

   function Is_Valid (N : Net) return Boolean;

   type Net_Record is record
      Parent : Instance;
      First_Sink : Input;
      W : Width;
   end record;

   type Pval is new Uns32;
   No_Pval : constant Pval := 0;

end Netlists;
