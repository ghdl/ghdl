--  Netlist.
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

with Types; use Types;

package Netlists is
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
   function New_Sname_User (Id : Name_Id) return Sname;
   function New_Sname_Artificial (Id : Name_Id) return Sname;
   function New_Sname (Prefix : Sname; Suffix : Name_Id) return Sname;
   function New_Sname_Version (Prefix : Sname; Ver : Uns32) return Sname;

   --  Read the content of an Sname.
   function Get_Sname_Kind (Name : Sname) return Sname_Kind;
   function Get_Sname_Prefix (Name : Sname) return Sname;
   function Get_Sname_Suffix (Name : Sname) return Name_Id;
   function Get_Sname_Version (Name : Sname) return Uns32;
   function Get_Sname_Num (Name : Sname) return Uns32;

   type Net is private;
   No_Net : constant Net;

   type Module is private;
   No_Module : constant Module;

   type Instance is private;
   No_Instance : constant Instance;

   --  Witdh of a net, ie number of bits.
   --  No_Width (value 0) is reserved to mean unknown.  This is allowed only to
   --  describe the width of predefined gates (like and) so that the same
   --  module can be used for any width.
   subtype Width is Uns32;
   No_Width : constant Width := 0;

   type Port_Kind is (Port_In, Port_Out, Port_Inout);

   --  Inout are considered as output.
   subtype Port_Outs is Port_Kind range Port_Out .. Port_Inout;

   --  Each module has a numeric identifier that can be used to easily identify
   --  a module.  Gates (and, or, ...) have reverved identifiers.
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
   Id_User_First : constant Module_Id := Id_User_None + 1;

   --  Port index.  Starts at 0.
   type Port_Nbr is new Uns32;
   subtype Port_Idx is Port_Nbr range 0 .. Port_Nbr'Last - 1;

   type Port_Desc is record
      --  Name of the port.
      Name : Sname;

      --  Port width (number of bits).
      W : Width;

      --  Direction.
      Dir : Port_Kind;

      --  For a bus: left and right bounds of the bus, ie [L:R].
      Left : Int32;
      Right : Int32;
   end record;

   type Port_Desc_Array is array (Port_Idx range <>) of Port_Desc;

   type Param_Idx is new Uns32;
   No_Param_Idx : constant Param_Idx := 0;

   subtype Param_Nbr is Param_Idx range 0 .. Param_Idx'Last - 1;

   type Param_Type is
     (Param_Invalid,

      Param_Uns32
      --  An unsigned 32 bit value.
     );
   pragma Convention (C, Param_Type);

   type Param_Desc is record
      --  Name of the parameter
      Name : Sname;

      --  Type of the parameter
      Typ : Param_Type;
   end record;

   type Param_Desc_Array is array (Param_Idx range <>) of Param_Desc;

   --  Module.
   --
   --  A module represent an uninstantiated netlist.  It is composed of nets
   --  and instances
   --
   --  From the outside, a module has ports (inputs and outputs), and
   --  optionally parameters.  A module must have at least one port.
   --
   --  In a module, there is a special instance (the self one) one that
   --  represent the ports of the module itself, but with the opposite
   --  direction.  Using this trick, there is no difference between ports of
   --  instances and ports of the module itself.
   function New_Design (Name : Sname) return Module;
   function New_User_Module (Parent : Module;
                             Name : Sname;
                             Id : Module_Id;
                             Nbr_Inputs : Port_Nbr;
                             Nbr_Outputs : Port_Nbr;
                             Nbr_Params : Param_Nbr := 0)
                            return Module;
   procedure Set_Port_Desc (M : Module;
                            Input_Descs : Port_Desc_Array;
                            Output_Descs : Port_Desc_Array);
   procedure Set_Param_Desc (M : Module;
                             Params : Param_Desc_Array);

   --  Create the self instance, once ports are defined.  This is required if
   --  the internal netlist will be defined.
   function Create_Self_Instance (M : Module) return Instance;

   function Get_Module_Name (M : Module) return Sname;
   function Get_Name (M : Module) return Sname renames Get_Module_Name;
   function Get_Id (M : Module) return Module_Id;

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

   type Input is private;
   No_Input : constant Input;

   --  Instance
   function New_Instance (Parent : Module; M : Module; Name : Sname)
                         return Instance;

   --  Mark INST as free, but keep it in the module.
   --  Use Remove_Free_Instances for a cleanup.
   procedure Free_Instance (Inst : Instance);

   function Is_Self_Instance (I : Instance) return Boolean;
   function Get_Module (Inst : Instance) return Module;
   function Get_Instance_Name (Inst : Instance) return Sname;
   function Get_Name (Inst : Instance) return Sname renames Get_Instance_Name;
   function Get_Instance_Parent (Inst : Instance) return Module;
   function Get_Parent (Inst : Instance) return Module
     renames Get_Instance_Parent;
   function Get_Output (Inst : Instance; Idx : Port_Idx) return Net;
   function Get_Input (Inst : Instance; Idx : Port_Idx) return Input;
   function Get_Next_Instance (Inst : Instance) return Instance;

   function Get_Param_Uns32 (Inst : Instance; Param : Param_Idx) return Uns32;
   procedure Set_Param_Uns32 (Inst : Instance; Param : Param_Idx; Val : Uns32);

   --  Input
   function Get_Input_Parent (I : Input) return Instance;
   function Get_Parent (I : Input) return Instance renames Get_Input_Parent;
   function Get_Port_Idx (I : Input) return Port_Idx;
   function Get_Driver (I : Input) return Net;
   function Get_Next_Sink (I : Input) return Input;

   --  Net (Output)
   function Get_Net_Parent (O : Net) return Instance;
   function Get_Parent (O : Net) return Instance renames Get_Net_Parent;
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
private
   type Sname is new Uns32 range 0 .. 2**30 - 1;
   No_Sname : constant Sname := 0;

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

   type Module is new Uns32;
   No_Module : constant Module := 0;
   Free_Module : constant Module := 1;

   function Is_Valid (M : Module) return Boolean;

   type Port_Desc_Idx is new Uns32;
   No_Port_Desc_Idx : constant Port_Desc_Idx := 0;

   type Param_Desc_Idx is new Uns32;
   No_Param_Desc_Idx : constant Param_Desc_Idx := 0;

   type Module_Record is record
      Parent : Module;
      Name : Sname;
      Id : Module_Id;
      First_Port_Desc : Port_Desc_Idx;
      Nbr_Inputs : Port_Nbr;
      Nbr_Outputs : Port_Nbr;
      First_Param_Desc : Param_Desc_Idx;
      Nbr_Params : Param_Nbr;

      --  First sub-module child.
      First_Sub_Module : Module;
      Last_Sub_Module : Module;

      --  Sub-module brother.
      Next_Sub_Module : Module;

      --  The self instance is the first instance.
      First_Instance : Instance;
      Last_Instance : Instance;
   end record;

   function Get_First_Port_Desc (M : Module) return Port_Desc_Idx;
   function Get_First_Output (Inst : Instance) return Net;
   function Get_Port_Desc (Idx : Port_Desc_Idx) return Port_Desc;

   type Instance is new Uns32;
   No_Instance : constant Instance := 0;

   function Is_Valid (I : Instance) return Boolean;

   type Instance_Record is record
      --  The instance is instantiated in Parent.
      Parent : Module;
      Next_Instance : Instance;

      --  For a self-instance, Klass is equal to Parent, and Name is No_Sname.
      Klass : Module;
      Name : Sname;

      First_Param : Param_Idx;
      First_Input : Input;
      First_Output : Net;
   end record;

   --  Procedures to rewrite the instances of a module: first extract the chain
   --  of instances from module M, then add the ones to keep.
   procedure Extract_All_Instances (M : Module; First_Instance : out Instance);
   procedure Append_Instance (M : Module; Inst : Instance);

   type Input is new Uns32;
   No_Input : constant Input := 0;

   type Input_Record is record
      Parent : Instance;
      Driver : Net;
      Next_Sink : Input;
   end record;

   type Net is new Uns32;
   No_Net : constant Net := 0;

   function Is_Valid (N : Net) return Boolean;

   type Net_Record is record
      Parent : Instance;
      First_Sink : Input;
      W : Width;
   end record;
end Netlists;
