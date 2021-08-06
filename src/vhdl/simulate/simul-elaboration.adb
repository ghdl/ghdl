--  Elaboration
--  Copyright (C) 2014 Tristan Gingold
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

with Simple_IO;
with Str_Table;
with Errorout; use Errorout;
with Vhdl.Errors; use Vhdl.Errors;
with Vhdl.Evaluation;
with Simul.Execution; use Simul.Execution;
with Vhdl.Utils; use Vhdl.Utils;
with Libraries;
with Name_Table;
with Simul.File_Operation;
with Vhdl.Nodes_Utils; use Vhdl.Nodes_Utils;
with Vhdl.Sem_Lib; use Vhdl.Sem_Lib;
with Simul.Elaboration.AMS; use Simul.Elaboration.AMS;
with Areapools; use Areapools;
with Grt.Errors;
with Grt.Options;

package body Simul.Elaboration is

   procedure Elaborate_Dependence (Design_Unit: Iir_Design_Unit);

   procedure Elaborate_Concurrent_Statement_Part
     (Instance : Block_Instance_Acc; Stmt_Chain: Iir);
   procedure Elaborate_Type_Definition
     (Instance : Block_Instance_Acc; Def : Iir);
   procedure Elaborate_Nature_Definition
     (Instance : Block_Instance_Acc; Def : Iir);

   function Elaborate_Default_Value
     (Instance : Block_Instance_Acc; Decl : Iir)
     return Iir_Value_Literal_Acc;

   procedure Elaborate_Generic_Clause
     (Instance : Block_Instance_Acc; Generic_Chain : Iir);
   procedure Elaborate_Generic_Map_Aspect
     (Target_Instance : Block_Instance_Acc;
      Local_Instance : Block_Instance_Acc;
      Generics : Iir;
      Map : Iir);

   --  CONF is the block_configuration for components of ARCH.
   function Elaborate_Architecture (Arch : Iir_Architecture_Body;
                                    Conf : Iir_Block_Configuration;
                                    Parent_Instance : Block_Instance_Acc;
                                    Stmt : Iir;
                                    Generic_Map : Iir;
                                    Port_Map : Iir)
     return Block_Instance_Acc;

   procedure Create_Object (Instance : Block_Instance_Acc;
                            Slot : Object_Slot_Type;
                            Num : Object_Slot_Type := 1) is
   begin
      --  Check elaboration order.
      --  Note: this is not done for package since objects from package are
      --  commons (same scope), and package annotation order can be different
      --  from package elaboration order (eg: body).
      if Slot /= Instance.Elab_Objects + 1
        or else Instance.Objects (Slot) /= null
      then
         Error_Msg_Elab ("bad elaboration order");
         raise Internal_Error;
      end if;
      Instance.Elab_Objects := Slot + Num - 1;
   end Create_Object;

   procedure Create_Object (Instance : Block_Instance_Acc; Decl : Iir)
   is
      Slot : constant Object_Slot_Type := Get_Info (Decl).Slot;
   begin
      Create_Object (Instance, Slot, 1);
   end Create_Object;

   procedure Destroy_Object (Instance : Block_Instance_Acc; Decl : Iir)
   is
      Info : constant Sim_Info_Acc := Get_Info (Decl);
      Slot : constant Object_Slot_Type := Info.Slot;
   begin
      if Slot /= Instance.Elab_Objects
        or else Info.Obj_Scope /= Instance.Block_Scope
      then
         Error_Msg_Elab ("bad destroy order");
         raise Internal_Error;
      end if;
      --  Clear the slot (this is necessary for ranges).
      Instance.Objects (Slot) := null;
      Instance.Elab_Objects := Slot - 1;
   end Destroy_Object;

   procedure Create_Signal (Instance : Block_Instance_Acc; Decl : Iir)
   is
      Slot : constant Object_Slot_Type := Get_Info (Decl).Slot;
   begin
      Create_Object (Instance, Slot, 3);
   end Create_Signal;

   -- Create a new signal, using DEFAULT as initial value.
   -- Set its number.
   procedure Elaborate_Signal (Block: Block_Instance_Acc;
                               Signal: Iir;
                               Default : Iir_Value_Literal_Acc)
   is
      function Create_Signal (Lit: Iir_Value_Literal_Acc)
                             return Iir_Value_Literal_Acc
      is
         Res : Iir_Value_Literal_Acc;
      begin
         case Lit.Kind is
            when Iir_Value_Array =>
               Res := Create_Array_Value (Lit.Val_Array.Len,
                                          Lit.Bounds.Nbr_Dims);
               Res.Bounds.D := Lit.Bounds.D;
               Res := Unshare_Bounds (Res, Global_Pool'Access);

               for I in Lit.Val_Array.V'Range loop
                  Res.Val_Array.V (I) := Create_Signal (Lit.Val_Array.V (I));
               end loop;
            when Iir_Value_Record =>
               Res := Create_Record_Value
                 (Lit.Val_Record.Len, Instance_Pool);
               for I in Lit.Val_Record.V'Range loop
                  Res.Val_Record.V (I) := Create_Signal (Lit.Val_Record.V (I));
               end loop;

            when Iir_Value_Scalars =>
               Res := Create_Signal_Value (null);

            when Iir_Value_Signal
              | Iir_Value_Range
              | Iir_Value_File
              | Iir_Value_Access
              | Iir_Value_Protected
              | Iir_Value_Quantity
              | Iir_Value_Terminal
              | Iir_Value_Instance =>
               raise Internal_Error;
         end case;
         return Res;
      end Create_Signal;

      Slot : constant Object_Slot_Type := Get_Info (Signal).Slot;
      Sig : Iir_Value_Literal_Acc;
      Def : Iir_Value_Literal_Acc;
   begin
      Sig := Create_Signal (Default);
      Def := Unshare (Default, Global_Pool'Access);
      Block.Objects (Slot) := Sig;
      Block.Objects (Slot + 1) := Def;
      Block.Objects (Slot + 2) := Unshare (Default, Global_Pool'Access);

      case Get_Kind (Signal) is
         when Iir_Kind_Interface_Signal_Declaration =>
            --  Driver.
            case Get_Mode (Signal) is
               when Iir_Unknown_Mode =>
                  raise Internal_Error;
               when Iir_Linkage_Mode =>
                  Signals_Table.Append ((Mode_Linkage,
                                         Signal, Sig, Def, Block));
               when Iir_Buffer_Mode =>
                  Signals_Table.Append ((Mode_Buffer,
                                         Signal, Sig, Def, Block));
               when Iir_Out_Mode =>
                  Signals_Table.Append ((Mode_Out,
                                         Signal, Sig, Def, Block));
               when Iir_Inout_Mode =>
                  Signals_Table.Append ((Mode_Inout,
                                         Signal, Sig, Def, Block));
               when Iir_In_Mode =>
                  Signals_Table.Append ((Mode_In,
                                         Signal, Sig, Def, Block));
            end case;
         when Iir_Kind_Signal_Declaration =>
            Signals_Table.Append ((Mode_Signal, Signal, Sig, Def, Block));
         when others =>
            Error_Kind ("elaborate_signal", Signal);
      end case;
   end Elaborate_Signal;

   function Execute_Time_Attribute (Instance : Block_Instance_Acc; Attr : Iir)
                                   return Ghdl_I64
   is
      Param : constant Iir := Get_Parameter (Attr);
      Res : Ghdl_I64;
      Val : Iir_Value_Literal_Acc;
   begin
      if Param = Null_Iir then
         Res := 0;
      else
         Val := Execute_Expression (Instance, Param);
         Res := Val.I64;
      end if;
      return Res;
   end Execute_Time_Attribute;

   procedure Elaborate_Implicit_Signal
     (Instance: Block_Instance_Acc; Signal: Iir; Kind : Mode_Signal_Type)
   is
      Info : constant Sim_Info_Acc := Get_Info (Signal);
      Prefix : Iir_Value_Literal_Acc;
      T : Ghdl_I64;
      Sig : Iir_Value_Literal_Acc;
      Init : Iir_Value_Literal_Acc;
   begin
      if Kind = Mode_Transaction then
         T := 0;
         Init := Create_B1_Value (False);
      else
         T := Execute_Time_Attribute (Instance, Signal);
         Init := Create_B1_Value (False);
      end if;
      Create_Signal (Instance, Signal);
      Sig := Create_Signal_Value (null);
      Init := Unshare (Init, Global_Pool'Access);
      Instance.Objects (Info.Slot) := Sig;
      Instance.Objects (Info.Slot + 1) := Init;

      Prefix := Execute_Name (Instance, Get_Prefix (Signal), True);
      Prefix := Unshare_Bounds (Prefix, Global_Pool'Access);
      case Kind is
         when Mode_Stable =>
            Signals_Table.Append ((Kind => Mode_Stable,
                                   Decl => Signal,
                                   Sig => Sig,
                                   Val => Init,
                                   Instance => Instance,
                                   Time => Std_Time (T),
                                   Prefix => Prefix));
         when Mode_Quiet =>
            Signals_Table.Append ((Kind => Mode_Quiet,
                                   Decl => Signal,
                                   Sig => Sig,
                                   Val => Init,
                                   Instance => Instance,
                                   Time => Std_Time (T),
                                   Prefix => Prefix));
         when Mode_Transaction =>
            Signals_Table.Append ((Kind => Mode_Transaction,
                                   Decl => Signal,
                                   Sig => Sig,
                                   Val => Init,
                                   Instance => Instance,
                                   Time => 0,
                                   Prefix => Prefix));
         when others =>
            raise Internal_Error;
      end case;
   end Elaborate_Implicit_Signal;

   function Create_Delayed_Signal (Pfx : Iir_Value_Literal_Acc)
                                  return Iir_Value_Literal_Acc
   is
      Res : Iir_Value_Literal_Acc;
   begin
      case Pfx.Kind is
         when Iir_Value_Array =>
            Res := Create_Array_Value (Pfx.Val_Array.Len,
                                       Pfx.Bounds.Nbr_Dims,
                                       Global_Pool'Access);
            Res.Bounds.D := Pfx.Bounds.D;

            for I in Pfx.Val_Array.V'Range loop
               Res.Val_Array.V (I) := Create_Delayed_Signal
                 (Pfx.Val_Array.V (I));
            end loop;
         when Iir_Value_Record =>
            Res := Create_Record_Value (Pfx.Val_Record.Len,
                                        Global_Pool'Access);
            for I in Pfx.Val_Record.V'Range loop
               Res.Val_Record.V (I) := Create_Delayed_Signal
                 (Pfx.Val_Record.V (I));
            end loop;
         when Iir_Value_Signal =>
            Res := Create_Signal_Value (null);
         when others =>
            raise Internal_Error;
      end case;
      return Res;
   end Create_Delayed_Signal;

   procedure Elaborate_Delayed_Signal
     (Instance: Block_Instance_Acc; Signal: Iir)
   is
      Info : constant Sim_Info_Acc := Get_Info (Signal);
      Prefix : Iir_Value_Literal_Acc;
      Sig : Iir_Value_Literal_Acc;
      Init : Iir_Value_Literal_Acc;
      T : Ghdl_I64;
   begin
      Prefix := Execute_Name (Instance, Get_Prefix (Signal), True);
      Prefix := Unshare_Bounds (Prefix, Global_Pool'Access);

      T := Execute_Time_Attribute (Instance, Signal);

      Sig := Create_Delayed_Signal (Prefix);
      Create_Signal (Instance, Signal);
      Instance.Objects (Info.Slot) := Sig;

      Init := Execute_Signal_Name (Instance, Get_Prefix (Signal), Signal_Val);
      Init := Unshare (Init, Global_Pool'Access); --  Create a full copy.
      Instance.Objects (Info.Slot + 1) := Init;

      Signals_Table.Append ((Kind => Mode_Delayed,
                             Decl => Signal,
                             Sig => Sig,
                             Val => Init,
                             Instance => Instance,
                             Time => Std_Time (T),
                             Prefix => Prefix));
   end Elaborate_Delayed_Signal;

   --  Create a block instance to instantiate OBJ (block, component,
   --  architecture, generate) in FATHER.  STMT is the statement/declaration
   --  at the origin of the instantiation (it is generally the same as OBJ,
   --  except for component where STMT is the component instantation
   --  statement).
   function Create_Block_Instance (Father : Block_Instance_Acc;
                                   Obj : Iir;
                                   Stmt : Iir)
                                  return Block_Instance_Acc
   is
      Obj_Info : constant Sim_Info_Acc := Get_Info (Obj);
      Res : Block_Instance_Acc;
   begin
      Nbr_Block_Instances := Nbr_Block_Instances + 1;

      Res := new Block_Instance_Type'
        (Max_Objs => Obj_Info.Nbr_Objects,
         Id => Nbr_Block_Instances,
         Block_Scope => Obj_Info,
         Uninst_Scope => null,
         Up_Block => Father,
         Label => Stmt,
         Bod => Null_Iir,
         Stmt => Obj,
         Parent => Father,
         Children => null,
         Brother => null,
         Marker => Empty_Marker,
         Objects => (others => null),
         Elab_Objects => 0,
         In_Wait_Flag => False,
         Actuals_Ref => null,
         Result => null);

      if Father /= null then
         --  Prepend the instance to the chain of children.
         case Obj_Info.Kind is
            when Kind_Block
              | Kind_Process =>
               Res.Brother := Father.Children;
               Father.Children := Res;
            when others =>
               null;
         end case;
      end if;

      return Res;
   end Create_Block_Instance;

   procedure Elaborate_Package_Declaration
     (Block : Block_Instance_Acc; Decl : Iir)
   is
      Package_Info : constant Sim_Info_Acc := Get_Info (Decl);
      Instance : Block_Instance_Acc;
      Hdr : Iir;
      Bod : Iir;
   begin
      if Block /= Global_Instances then
         --  Packages in library unit can be elaborated in a different order.
         Create_Object (Block, Package_Info.Pkg_Slot);
      end if;

      Instance := Create_Block_Instance (Block, Decl, Decl);

      pragma Assert (Block.Block_Scope = Package_Info.Pkg_Parent);
      Block.Objects (Package_Info.Pkg_Slot) :=
        Create_Instance_Value (Instance);

      if Trace_Elaboration then
         Report_Msg (Msgid_Note, Errorout.Elaboration, No_Source_Coord,
                     "elaborating %n", (1 => +Decl));
      end if;

      case Iir_Kinds_Package_Declaration (Get_Kind (Decl)) is
         when Iir_Kind_Package_Instantiation_Declaration =>
            Hdr := Decl;
         when Iir_Kind_Package_Declaration =>
            Hdr := Get_Package_Header (Decl);
      end case;
      if Hdr /= Null_Iir then
         Elaborate_Generic_Clause (Instance, Get_Generic_Chain (Hdr));
         Elaborate_Generic_Map_Aspect
           (Instance, Instance,
            Get_Generic_Chain (Hdr),
            Get_Generic_Map_Aspect_Chain (Hdr));
      end if;

      -- Elaborate objects declarations.
      Elaborate_Declarative_Part (Instance, Get_Declaration_Chain (Decl));

      if Get_Kind (Decl) = Iir_Kind_Package_Instantiation_Declaration then
         Bod := Get_Instance_Package_Body (Decl);
         --  Elaborate the body now.
         if Bod /= Null_Iir then
            --  Macro-expanded.
            Elaborate_Declarative_Part
              (Instance, Get_Declaration_Chain (Bod));
         else
            --  Shared body.
            declare
               Uninst : constant Iir := Get_Uninstantiated_Package_Decl (Decl);
            begin
               Bod := Get_Package_Body (Uninst);
               Instance.Uninst_Scope := Get_Info (Uninst);
               if Is_Valid (Bod) then
                  --  Body is optional.
                  Elaborate_Declarative_Part
                    (Instance, Get_Declaration_Chain (Bod));
               end if;
            end;
         end if;
      end if;
   end Elaborate_Package_Declaration;

   procedure Elaborate_Package_Body (Block : Block_Instance_Acc; Decl: Iir)
   is
      Package_Info : constant Sim_Info_Acc := Get_Info (Decl);
      Instance : constant Block_Instance_Acc :=
        Block.Objects (Package_Info.Pkg_Slot).Instance;
   begin
      if Trace_Elaboration then
         Report_Msg (Msgid_Note, Errorout.Elaboration, No_Source_Coord,
                     "elaborating %n", (1 => +Decl));
      end if;

      -- Elaborate objects declarations.
      Elaborate_Declarative_Part (Instance, Get_Declaration_Chain (Decl));
   end Elaborate_Package_Body;

   procedure Elaborate_Configuration_Declaration (Decl : Iir)
   is
      Config_Info : constant Sim_Info_Acc := Get_Info (Decl);
      Instance : Block_Instance_Acc;
   begin
      if Config_Info = null then
         --  Not a user defined configuration.  No objects.
         pragma Assert (Get_Identifier (Decl) = Null_Identifier);
         return;
      end if;

      Instance := Create_Block_Instance (Global_Instances, Decl, Decl);

      Global_Instances.Objects (Config_Info.Pkg_Slot) :=
        Create_Instance_Value (Instance);

      -- Elaborate objects declarations.
      Elaborate_Declarative_Part (Instance, Get_Declaration_Chain (Decl));
   end Elaborate_Configuration_Declaration;

   -- Elaborate all packages which DESIGN_UNIT depends on.
   -- The packages are elaborated only once.  The body, if the package needs
   -- one, can be loaded during the elaboration.
   -- Recursive function.
   -- FIXME: handle pathological cases of recursion.
   -- Due to the rules of analysis, it is not possible to have a circulare
   -- dependence.
   procedure Elaborate_Dependence (Design_Unit: Iir_Design_Unit)
   is
      Depend_List : constant Iir_List := Get_Dependence_List (Design_Unit);
      Depend_It : List_Iterator;
      Design: Iir;
      Library_Unit: Iir;
   begin
      Depend_It := List_Iterate_Safe (Depend_List);
      while Is_Valid (Depend_It) loop
         Design := Get_Element (Depend_It);
         if Get_Kind (Design) = Iir_Kind_Entity_Aspect_Entity then
            --  During Sem, the architecture may be still unknown, and the
            --  dependency is therefore the aspect.
            Library_Unit := Strip_Denoting_Name (Get_Architecture (Design));
            case Get_Kind (Library_Unit) is
               when Iir_Kind_Architecture_Body =>
                  Design := Get_Design_Unit (Library_Unit);
               when Iir_Kind_Design_Unit =>
                  Design := Library_Unit;
                  Library_Unit := Get_Library_Unit (Design);
               when others =>
                  Error_Kind ("elaborate_dependence(1)", Library_Unit);
            end case;
         else
            Library_Unit := Get_Library_Unit (Design);
         end if;
         -- Elaborates only non-elaborated packages.
         case Get_Kind (Library_Unit) is
            when Iir_Kind_Package_Declaration =>
               declare
                  Info : constant Sim_Info_Acc := Get_Info (Library_Unit);
                  Body_Design: Iir_Design_Unit;
               begin
                  if not Is_Uninstantiated_Package (Library_Unit)
                    and then Global_Instances.Objects (Info.Pkg_Slot) = null
                  then
                     --  Package not yet elaborated.

                     --  Load the body now, as it can add objects in the
                     --  package instance.
                     --  Don't try to load optionnal but obsolete package body.
                     Body_Design := Libraries.Find_Secondary_Unit
                       (Design, Null_Identifier);
                     if Body_Design /= Null_Iir
                       and then
                       (Get_Need_Body (Library_Unit)
                          or else Get_Date (Body_Design) /= Date_Obsolete)
                     then
                        Load_Design_Unit (Body_Design, Design_Unit);
                     else
                        Body_Design := Null_Iir;
                     end if;

                     --  First the packages on which DESIGN depends.
                     Elaborate_Dependence (Design);

                     --  Then the declaration.
                     Elaborate_Package_Declaration
                       (Global_Instances, Library_Unit);

                     --  And then the body (if any).
                     if Body_Design = Null_Iir then
                        if Get_Need_Body (Library_Unit) then
                           Error_Msg_Elab
                             ("no package body for `" &
                                Image_Identifier (Library_Unit) & ''');
                        end if;
                     else
                        -- Note: the body can elaborate some packages.
                        Elaborate_Dependence (Body_Design);

                        Elaborate_Package_Body
                          (Global_Instances, Get_Library_Unit (Body_Design));
                     end if;
                  end if;
               end;
            when Iir_Kind_Package_Instantiation_Declaration =>
               declare
                  Info : constant Sim_Info_Acc := Get_Info (Library_Unit);
               begin
                  if Global_Instances.Objects (Info.Pkg_Slot) = null then
                     --  Package not yet elaborated.

                     --  First the packages on which DESIGN depends.
                     Elaborate_Dependence (Design);

                     --  Then the declaration.
                     Elaborate_Package_Declaration
                       (Global_Instances, Library_Unit);
                  end if;
               end;
            when Iir_Kind_Entity_Declaration
              | Iir_Kind_Configuration_Declaration
              | Iir_Kind_Architecture_Body =>
               Elaborate_Dependence (Design);
            when Iir_Kind_Package_Body =>
               --  For package instantiation.
               Elaborate_Dependence (Design);
            when Iir_Kind_Context_Declaration =>
               Elaborate_Dependence (Design);
            when others =>
               Error_Kind ("elaborate_dependence", Library_Unit);
         end case;
         Next (Depend_It);
      end loop;
   end Elaborate_Dependence;

   function Create_Protected_Object (Block: Block_Instance_Acc; Decl: Iir)
                                    return Iir_Value_Literal_Acc
   is
      Bod : constant Iir := Execution.Get_Protected_Type_Body_Origin (Decl);
      Bod_Info : constant Sim_Info_Acc := Get_Info (Bod);
      Inst : Block_Instance_Acc;
      Res : Iir_Value_Literal_Acc;
   begin
      Protected_Table.Increment_Last;
      Res := Create_Protected_Value (Protected_Table.Last);

      Inst := Create_Subprogram_Instance (Block, null, Decl);
      if Bod_Info /= Get_Info (Decl) then
         Inst.Uninst_Scope := Bod_Info;
      end if;
      Protected_Table.Table (Res.Prot) := Inst;

      --  Temporary put the instancce on the stack in case of function calls
      --  during the elaboration of the protected object.
      Current_Process.Instance := Inst;

      Elaborate_Declarative_Part (Inst, Get_Declaration_Chain (Bod));

      Current_Process.Instance := Block;

      return Res;
   end Create_Protected_Object;

   --  Create an value_literal for DECL (defined in BLOCK) and set it with
   --  its default values. Nodes are shared.
   function Create_Value_For_Type
     (Block: Block_Instance_Acc; Decl: Iir; Init : Init_Value_Kind)
      return Iir_Value_Literal_Acc
   is
      Res : Iir_Value_Literal_Acc;
      Bounds : Iir_Value_Literal_Acc;
   begin
      case Get_Kind (Decl) is
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Physical_Type_Definition =>
            case Init is
               when Init_Value_Default =>
                  Bounds := Execute_Bounds (Block, Decl);
                  Res := Bounds.Left;
               when Init_Value_Any =>
                  case Kind_Scalar_Types (Get_Info (Get_Base_Type (Decl)).Kind)
                     is
                     when Kind_Bit_Type =>
                        Res := Create_B1_Value (False);
                     when Kind_E8_Type
                       | Kind_Log_Type =>
                        Res := Create_E8_Value (0);
                     when Kind_E32_Type =>
                        Res := Create_E32_Value (0);
                     when Kind_I64_Type =>
                        Res := Create_I64_Value (0);
                     when Kind_F64_Type =>
                        Res := Create_F64_Value (0.0);
                  end case;
               when Init_Value_Signal =>
                  Res := Create_Signal_Value (null);
            end case;

         when Iir_Kind_Array_Subtype_Definition =>
            Res := Create_Array_Bounds_From_Type (Block, Decl, True);
            declare
               El_Type : constant Iir := Get_Element_Subtype (Decl);
               El_Val : Iir_Value_Literal_Acc;
            begin
               if Res.Val_Array.Len > 0 then
                  --  Aliases the elements, for speed.  If modified, the
                  --  value will first be copied which will unalias it.
                  El_Val := Create_Value_For_Type (Block, El_Type, Init);
                  for I in 1 .. Res.Val_Array.Len loop
                     Res.Val_Array.V (I) := El_Val;
                  end loop;
               end if;
            end;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            declare
               List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Decl);
               El : Iir_Element_Declaration;
            begin
               Res := Create_Record_Value
                 (Iir_Index32 (Get_Nbr_Elements (List)));

               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  Res.Val_Record.V (1 + Get_Element_Position (El)) :=
                    Create_Value_For_Type (Block, Get_Type (El), Init);
               end loop;
            end;
         when Iir_Kind_Access_Type_Definition
           | Iir_Kind_Access_Subtype_Definition =>
            return Create_Access_Value (null);
         when Iir_Kind_Protected_Type_Declaration =>
            return Create_Protected_Object (Block, Decl);
         when others =>
            Error_Kind ("create_value_for_type", Decl);
      end case;
      return Res;
   end Create_Value_For_Type;

   procedure Init_To_Default
     (Targ : Iir_Value_Literal_Acc; Block: Block_Instance_Acc; Atype : Iir) is
   begin
      case Get_Kind (Atype) is
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Enumeration_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Subtype_Definition
           | Iir_Kind_Physical_Type_Definition =>
            declare
               Bounds : Iir_Value_Literal_Acc;
            begin
               Bounds := Execute_Bounds (Block, Atype);
               Store (Targ, Bounds.Left);
            end;

         when Iir_Kind_Array_Subtype_Definition
           | Iir_Kind_Array_Type_Definition =>
            declare
               El_Type : constant Iir := Get_Element_Subtype (Atype);
            begin
               for I in 1 .. Targ.Val_Array.Len loop
                  Init_To_Default (Targ.Val_Array.V (I), Block, El_Type);
               end loop;
            end;
         when Iir_Kind_Record_Type_Definition
           | Iir_Kind_Record_Subtype_Definition =>
            declare
               List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Get_Base_Type (Atype));
               El : Iir_Element_Declaration;
            begin
               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  Init_To_Default (Targ.Val_Record.V (1 + Iir_Index32 (I)),
                                   Block, Get_Type (El));
               end loop;
            end;
         when Iir_Kind_Access_Type_Definition
           | Iir_Kind_Access_Subtype_Definition =>
            Store (Targ, Null_Lit);
         when others =>
            Error_Kind ("Init_To_Default", Atype);
      end case;
   end Init_To_Default;

   function Create_Terminal_Object (Block: Block_Instance_Acc;
                                    Decl : Iir;
                                    Def: Iir)
                                   return Iir_Value_Literal_Acc
   is
      Res : Iir_Value_Literal_Acc;
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Scalar_Nature_Definition =>
            Res := Create_Terminal_Value
              (Create_Scalar_Terminal (Decl, Block));
         when others =>
            Error_Kind ("create_terminal_object", Def);
      end case;
      return Res;
   end Create_Terminal_Object;

   procedure Create_Terminal (Instance : Block_Instance_Acc; Decl : Iir)
   is
      Slot : constant Object_Slot_Type := Get_Info (Decl).Slot;
   begin
      if Slot + 1 = Instance.Elab_Objects then
         --  Reference terminal of nature declaration may have already been
         --  elaborated.
         return;
      end if;
      if Slot /= Instance.Elab_Objects then
         Error_Msg_Elab ("bad elaboration order");
         raise Internal_Error;
      end if;
      Instance.Objects (Slot) :=
        Create_Terminal_Object (Instance, Decl, Get_Nature (Decl));
      Instance.Elab_Objects := Slot + 1;
   end Create_Terminal;

   function Create_Quantity_Object (Block: Block_Instance_Acc;
                                    Decl : Iir;
                                    Def: Iir)
                                   return Iir_Value_Literal_Acc
   is
      Res : Iir_Value_Literal_Acc;
      Kind : Quantity_Kind;
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Floating_Subtype_Definition =>
            case Iir_Kinds_Quantity_Declaration (Get_Kind (Decl)) is
               when Iir_Kind_Across_Quantity_Declaration =>
                  Kind := Quantity_Across;
               when Iir_Kind_Through_Quantity_Declaration =>
                  Kind := Quantity_Through;
               when Iir_Kind_Free_Quantity_Declaration =>
                  Kind := Quantity_Free;
               when Iir_Kinds_Source_Quantity_Declaration =>
                  raise Internal_Error; --  TODO.
            end case;
            Res := Create_Quantity_Value
              (Create_Scalar_Quantity (Kind, Decl, Block));
         when others =>
            Error_Kind ("create_quantity_object", Def);
      end case;
      return Res;
   end Create_Quantity_Object;

   function Create_Quantity (Instance : Block_Instance_Acc; Decl : Iir)
     return Iir_Value_Literal_Acc
   is
      Slot : constant Object_Slot_Type := Get_Info (Decl).Slot;
      Res : Iir_Value_Literal_Acc;
   begin
      if Slot /= Instance.Elab_Objects then
         Error_Msg_Elab ("bad elaboration order");
         raise Internal_Error;
      end if;
      Res := Create_Quantity_Object (Instance, Decl, Get_Type (Decl));
      Instance.Objects (Slot) := Res;
      Instance.Elab_Objects := Slot + 1;
      return Res;
   end Create_Quantity;

   procedure Elaborate_Range_Expression
     (Instance : Block_Instance_Acc; Rc: Iir_Range_Expression)
   is
      Range_Info : constant Sim_Info_Acc := Get_Info (Rc);
      Val : Iir_Value_Literal_Acc;
   begin
      if Range_Info.Obj_Scope /= Instance.Block_Scope
        or else Instance.Objects (Range_Info.Slot) /= null
      then
         --  A range expression may have already been created, for example
         --  when severals objects are created with the same subtype:
         --    variable v, v1 : bit_vector (x to y);
         return;
      end if;
      if False
        and then (Range_Info.Obj_Scope /= Instance.Block_Scope
                    or else Range_Info.Slot < Instance.Elab_Objects)
      then
         --  FIXME: the test is wrong for packages.
         --  The range was already elaborated.
         --  ?? Is that possible
         raise Internal_Error;
         return;
      end if;
      Create_Object (Instance, Rc);
      Val := Create_Range_Value
        (Execute_Expression (Instance, Get_Left_Limit (Rc)),
         Execute_Expression (Instance, Get_Right_Limit (Rc)),
         Get_Direction (Rc));
      Check_Range_Constraints (Instance, Val, Rc, Rc);
      Instance.Objects (Range_Info.Slot) := Unshare (Val, Instance_Pool);
   end Elaborate_Range_Expression;

   procedure Elaborate_Range_Constraint
     (Instance : Block_Instance_Acc; Rc: Iir) is
   begin
      case Get_Kind (Rc) is
         when Iir_Kind_Range_Expression =>
            Elaborate_Range_Expression (Instance, Rc);
         when Iir_Kind_Range_Array_Attribute
           | Iir_Kind_Reverse_Range_Array_Attribute =>
            null;
         when others =>
            Error_Kind ("elaborate_range_constraint", Rc);
      end case;
   end Elaborate_Range_Constraint;

   --  Create the bounds of a scalar type definition.
   --  Elaborate_Range_Constraint cannot be used, as it checks bounds (and
   --  here we create the bounds).
   procedure Elaborate_Type_Range
     (Instance : Block_Instance_Acc; Rc: Iir_Range_Expression)
   is
      Range_Info : constant Sim_Info_Acc := Get_Info (Rc);
      Val : Iir_Value_Literal_Acc;
   begin
      Create_Object (Instance, Rc);
      Val := Create_Range_Value
        (Execute_Expression (Instance, Get_Left_Limit (Rc)),
         Execute_Expression (Instance, Get_Right_Limit (Rc)),
         Get_Direction (Rc));
      Instance.Objects (Range_Info.Slot) := Unshare (Val, Instance_Pool);
   end Elaborate_Type_Range;

   --  DECL is a subtype indication.
   --  Elaborate DECL only if it is anonymous.
   procedure Elaborate_Subtype_Indication_If_Anonymous
     (Instance : Block_Instance_Acc; Decl : Iir) is
   begin
      if Is_Anonymous_Type_Definition (Decl) then
         Elaborate_Subtype_Indication (Instance, Decl);
      end if;
   end Elaborate_Subtype_Indication_If_Anonymous;

   --  LRM93 12.3.1.3  Subtype Declarations
   --  The elaboration of a subtype indication creates a subtype.
   procedure Elaborate_Subtype_Indication
     (Instance : Block_Instance_Acc; Ind : Iir)
   is
   begin
      case Get_Kind (Ind) is
         when Iir_Kind_Enumeration_Type_Definition
           | Iir_Kind_Array_Type_Definition
           | Iir_Kind_File_Type_Definition
           | Iir_Kind_Access_Type_Definition
           | Iir_Kind_Record_Type_Definition =>
            Elaborate_Type_Definition (Instance, Ind);
         when Iir_Kind_Array_Subtype_Definition =>
            --  LRM93 12.3.1.3
            --  The elaboration of an index constraint consists of the
            --  declaration of each of the discrete ranges in the index
            --  constraint in some order that is not defined by the language.
            declare
               St_Indexes : constant Iir_Flist := Get_Index_Subtype_List (Ind);
               St_El : Iir;
            begin
               for I in Flist_First .. Flist_Last (St_Indexes) loop
                  St_El := Get_Index_Type (St_Indexes, I);
                  Elaborate_Subtype_Indication_If_Anonymous (Instance, St_El);
               end loop;
               Elaborate_Subtype_Indication_If_Anonymous
                 (Instance, Get_Element_Subtype (Ind));
            end;
         when Iir_Kind_Record_Subtype_Definition =>
            null;
         when Iir_Kind_Access_Subtype_Definition =>
            null;
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Enumeration_Subtype_Definition =>
            Elaborate_Range_Constraint (Instance, Get_Range_Constraint (Ind));
         when Iir_Kind_Physical_Subtype_Definition =>
            Elaborate_Range_Constraint (Instance, Get_Range_Constraint (Ind));
         when others =>
            Error_Kind ("elaborate_subtype_indication", Ind);
      end case;
   end Elaborate_Subtype_Indication;

   --  LRM93 12.3.1.2  Type Declarations.
   procedure Elaborate_Type_Definition
     (Instance : Block_Instance_Acc; Def : Iir)
   is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Enumeration_Type_Definition =>
            --  Elaboration of an enumeration type definition has not effect
            --  other than the creation of the corresponding type.
            Elaborate_Type_Range (Instance, Get_Range_Constraint (Def));
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Floating_Type_Definition
           | Iir_Kind_Physical_Type_Definition =>
            null;
         when Iir_Kind_Integer_Subtype_Definition
           | Iir_Kind_Floating_Subtype_Definition
           | Iir_Kind_Physical_Subtype_Definition =>
            --  Elaboration of an integer, floating point, or physical type
            --  definition consists of the elaboration of the corresponding
            --  range constraint.
            Elaborate_Subtype_Indication_If_Anonymous (Instance, Def);
            --  Elaboration of a physical unit declaration has no effect other
            --  than to create the unit defined by the unit declaration.
            null;
         when Iir_Kind_Array_Type_Definition =>
            --  Elaboration of an unconstrained array type definition consists
            --  of the elaboration of the element subtype indication of the
            --  array type.
            Elaborate_Subtype_Indication_If_Anonymous
              (Instance, Get_Element_Subtype (Def));
         when Iir_Kind_Access_Type_Definition =>
            --  Elaboration of an access type definition consists of the
            --  elaboration of the corresponding subtype indication.
            Elaborate_Subtype_Indication_If_Anonymous
              (Instance, Get_Designated_Type (Def));
         when Iir_Kind_File_Type_Definition =>
            --  GHDL: There is nothing about elaboration of a file type
            --  definition.  FIXME ??
            null;
         when Iir_Kind_Record_Type_Definition =>
            --  Elaboration of a record type definition consists of the
            --  elaboration of the equivalent single element declarations in
            --  the given order.
            declare
               List : constant Iir_Flist :=
                 Get_Elements_Declaration_List (Def);
               El : Iir_Element_Declaration;
            begin
               for I in Flist_First .. Flist_Last (List) loop
                  El := Get_Nth_Element (List, I);
                  --  Elaboration of an element declaration consists of
                  --  elaboration of the element subtype indication.
                  Elaborate_Subtype_Indication_If_Anonymous
                    (Instance, Get_Type (El));
               end loop;
            end;
         when Iir_Kind_Protected_Type_Declaration =>
            Elaborate_Declarative_Part
              (Instance, Get_Declaration_Chain (Def));

         when Iir_Kind_Incomplete_Type_Definition =>
            null;
         when others =>
            Error_Kind ("elaborate_type_definition", Def);
      end case;
   end Elaborate_Type_Definition;

   --  LRM93 12.3.1.2  Type Declarations.
   procedure Elaborate_Type_Declaration
     (Instance : Block_Instance_Acc; Decl : Iir_Type_Declaration)
   is
      Def : Iir;
      Base_Type : Iir_Array_Type_Definition;
   begin
      --  Elaboration of a type declaration generally consists of the
      --  elaboration of the definition of the type and the creation of that
      --  type.
      Def := Get_Type_Definition (Decl);
      if Def = Null_Iir then
         --  FIXME: can this happen ?
         raise Program_Error;
      end if;
      if Get_Kind (Def) = Iir_Kind_Array_Subtype_Definition then
         Base_Type := Get_Base_Type (Def);
         --  For a constrained array type declaration, however,
         --  elaboration consists of the elaboration of the equivalent
         --  anonymous unconstrained array type [...]
         Elaborate_Subtype_Indication_If_Anonymous (Instance, Base_Type);
         --  [...] followed by the elaboration of the named subtype
         --  of that unconstrained type.
         Elaborate_Subtype_Indication (Instance, Def);
      else
         Elaborate_Type_Definition (Instance, Def);
      end if;
   end Elaborate_Type_Declaration;

   procedure Elaborate_Nature_Definition
     (Instance : Block_Instance_Acc; Def : Iir)
   is
   begin
      case Get_Kind (Def) is
         when Iir_Kind_Scalar_Nature_Definition =>
            Elaborate_Subtype_Indication (Instance, Get_Across_Type (Def));
            Elaborate_Subtype_Indication (Instance, Get_Through_Type (Def));
         when others =>
            Error_Kind ("elaborate_nature_definition", Def);
      end case;
   end Elaborate_Nature_Definition;

   --  LRM93 12.2.1  The Generic Clause
   --  LRM08 14.3.2  Generic clause
   procedure Elaborate_Generic_Clause
     (Instance : Block_Instance_Acc; Generic_Chain : Iir)
   is
      Decl : Iir_Interface_Constant_Declaration;
   begin
      --  LRM08 14.3.2 Generic clause
      --  Elaboration of a generic clause consists of the elaboration of each
      --  of the equivalent single generic declarations contained in the
      --  clause, in the order given.
      Decl := Generic_Chain;
      while Decl /= Null_Iir loop
         case Get_Kind (Decl) is
            when Iir_Kind_Interface_Constant_Declaration =>
               --  LRM93 12.2.2 The generic clause
               --  The elaboration of a generic declaration consists of
               --  elaborating the subtype indication and then creating a
               --  generic constant of that subtype.
               Elaborate_Subtype_Indication_If_Anonymous
                 (Instance, Get_Type (Decl));
               Create_Object (Instance, Decl);
               --  The value of a generic constant is not defined until a
               --  subsequent generic map aspect is evaluated, or in the
               --  absence of a generic map aspect, until the default
               --  expression associated with the generic constant is evaluated
               --  to determine the value of the constant.
            when Iir_Kind_Interface_Package_Declaration =>
               Create_Object (Instance, Get_Info (Decl).Pkg_Slot);
            when Iir_Kinds_Interface_Subprogram_Declaration
              | Iir_Kind_Interface_Type_Declaration =>
               --  Macro-expanded.
               null;
            when others =>
               Error_Kind ("elaborate_generic_clause", Decl);
         end case;
         Decl := Get_Chain (Decl);
      end loop;
   end Elaborate_Generic_Clause;

   --  LRM93 12.2.3  The Port Clause
   procedure Elaborate_Port_Clause
     (Instance : Block_Instance_Acc; Port_Chain : Iir)
   is
      Decl : Iir_Interface_Signal_Declaration;
   begin
      Decl := Port_Chain;
      while Decl /= Null_Iir loop
         --  LRM93 12.2.3
         --  The elaboration of a port declaration consists of elaborating the
         --  subtype indication and then creating a port of that subtype.
         Elaborate_Subtype_Indication_If_Anonymous (Instance, Get_Type (Decl));

         --  Simply increase an index to check that the port was created.
         Create_Signal (Instance, Decl);

         Decl := Get_Chain (Decl);
      end loop;
   end Elaborate_Port_Clause;

   --  LRM93 12.2.2  The generic Map Aspect
   procedure Elaborate_Generic_Map_Aspect
     (Target_Instance : Block_Instance_Acc;
      Local_Instance : Block_Instance_Acc;
      Generics : Iir;
      Map : Iir)
   is
      Assoc : Iir;
      Gen : Iir;
      Inter : Iir_Interface_Constant_Declaration;
      Value : Iir;
      Val : Iir_Value_Literal_Acc;
      Last_Individual : Iir_Value_Literal_Acc;
      Marker : Mark_Type;
   begin
      --  Elaboration of a generic map aspect consists of elaborating the
      --  generic association list.

      --  Elaboration of a generic association list consists of the
      --  elaboration of each generic association element in the
      --  association list.
      Assoc := Map;
      Gen := Generics;
      Mark (Marker, Expr_Pool);
      while Assoc /= Null_Iir loop
         --  Elaboration of a generic association element consists of the
         --  elaboration of the formal part and the evaluation of the actual
         --  part.
         --  FIXME:  elaboration of the formal part.
         Inter := Get_Association_Interface (Assoc, Gen);
         case Get_Kind (Assoc) is
            when Iir_Kind_Association_Element_Open =>
               --  The generic association list contains an implicit
               --  association element for each generic constant that is not
               --  explicitly associated with an actual [GHDL: done trought
               --  annotations] or that is associated with the reserved word
               --  OPEN; the actual part of such an implicit association
               --  element is the default expression appearing in the
               --  declaration of that generic constant.
               Value := Get_Default_Value (Inter);
               if Value /= Null_Iir then
                  Val := Execute_Expression (Target_Instance, Value);
               else
                  Val := Create_Value_For_Type
                    (Target_Instance, Get_Type (Inter),
                     Init_Value_Default);
               end if;
            when Iir_Kind_Association_Element_By_Expression =>
               Value := Get_Actual (Assoc);
               Val := Execute_Association_Expression
                 (Local_Instance, Value, Target_Instance);
            when Iir_Kind_Association_Element_By_Individual =>
               Val := Create_Value_For_Type
                 (Local_Instance, Get_Actual_Type (Assoc), Init_Value_Any);

               Last_Individual := Unshare (Val, Instance_Pool);
               Target_Instance.Objects (Get_Info (Inter).Slot) :=
                 Last_Individual;
               goto Continue;
            when Iir_Kind_Association_Element_Package =>
               declare
                  Actual : constant Iir :=
                    Strip_Denoting_Name (Get_Actual (Assoc));
                  Info : constant Sim_Info_Acc := Get_Info (Actual);
                  Pkg_Block : Block_Instance_Acc;
               begin
                  Pkg_Block := Get_Instance_By_Scope (Local_Instance, Info);
                  Target_Instance.Objects (Get_Info (Inter).Pkg_Slot) :=
                    Create_Instance_Value (Pkg_Block);
               end;

               goto Continue;
            when Iir_Kind_Association_Element_Subprogram
              | Iir_Kind_Association_Element_Type =>
               goto Continue;
            when others =>
               Error_Kind ("elaborate_generic_map_aspect", Assoc);
         end case;

         if Get_Whole_Association_Flag (Assoc) then
            --  It is an error if the value of the actual does not belong to
            --  the subtype denoted by the subtype indication of the formal.
            --  If the subtype denoted by the subtype indication of the
            --  declaration of the formal is a constrained array subtype, then
            --  an implicit subtype conversion is performed prior to this
            --  check.
            --  It is also an error if the type of the formal is an array type
            --  and the value of each element of the actual does not belong to
            --  the element subtype of the formal.
            Implicit_Array_Conversion
              (Target_Instance, Val, Get_Type (Inter), Inter);
            Check_Constraints (Target_Instance, Val, Get_Type (Inter), Inter);

            --  The generic constant or subelement or slice thereof designated
            --  by the formal part is then initialized with the value
            --  resulting from the evaluation of the corresponding actual part.
            Target_Instance.Objects (Get_Info (Inter).Slot) :=
              Unshare (Val, Instance_Pool);
         else
            declare
               Targ : Iir_Value_Literal_Acc;
               Is_Sig : Boolean;
            begin
               Execute_Name_With_Base
                 (Target_Instance, Get_Formal (Assoc),
                  Last_Individual, Targ, Is_Sig);
               Store (Targ, Val);
            end;
         end if;

         <<Continue>> null;
         Release (Marker, Expr_Pool);
         Next_Association_Interface (Assoc, Gen);
      end loop;
   end Elaborate_Generic_Map_Aspect;

   --  LRM93 12.2.3  The Port Clause
   procedure Elaborate_Port_Declaration
     (Instance : Block_Instance_Acc;
      Decl : Iir_Interface_Signal_Declaration;
      Default_Value : Iir_Value_Literal_Acc)
   is
      Val : Iir_Value_Literal_Acc;
   begin
      if Default_Value = null then
         Val := Elaborate_Default_Value (Instance, Decl);
      else
         Val := Default_Value;
      end if;
      Elaborate_Signal (Instance, Decl, Val);
   end Elaborate_Port_Declaration;

   procedure Elab_Connect
     (Formal_Instance : Block_Instance_Acc;
      Local_Instance : Block_Instance_Acc;
      Actual_Expr : Iir_Value_Literal_Acc;
      Assoc : Iir_Association_Element_By_Expression;
      Inter : Iir)
   is
      Formal : constant Iir := Get_Association_Formal (Assoc, Inter);
      Actual : constant Iir := Get_Actual (Assoc);
      Local_Expr : Iir_Value_Literal_Acc;
      Formal_Expr : Iir_Value_Literal_Acc;
   begin
      Formal_Expr := Execute_Name (Formal_Instance, Formal, True);
      Formal_Expr := Unshare_Bounds (Formal_Expr, Global_Pool'Access);
      if Actual_Expr = null then
         Local_Expr := Execute_Name (Local_Instance, Actual, True);
         Local_Expr := Unshare_Bounds (Local_Expr, Global_Pool'Access);
      else
         Local_Expr := Actual_Expr;
      end if;

      Connect_Table.Append ((Formal => Formal_Expr,
                             Formal_Instance => Formal_Instance,
                             Actual => Local_Expr,
                             Actual_Instance => Local_Instance,
                             Inter => Inter,
                             Assoc => Assoc));
   end Elab_Connect;

   --  LRM93 12.2.3  The Port Clause
   --  LRM93 12.2.4  The Port Map Aspect
   procedure Elaborate_Port_Map_Aspect
     (Formal_Instance : Block_Instance_Acc;
      Actual_Instance : Block_Instance_Acc;
      Ports : Iir;
      Map : Iir)
   is
      Assoc : Iir;
      Port : Iir;
      Inter : Iir_Interface_Signal_Declaration;
      Actual_Expr : Iir_Value_Literal_Acc;
      Init_Expr : Iir_Value_Literal_Acc;
      Actual : Iir;
      Formal : Iir;
   begin
      if Ports = Null_Iir then
         return;
      end if;

      --  Elaboration of a port map aspect consists of elaborating the port
      --  association list.
      if Map = Null_Iir then
         -- No port association, elaborate the port clause.
         --  Elaboration of a port clause consists of the elaboration of each
         --  of the equivalent signal port declaration in the clause, in the
         --  order given.
         Inter := Ports;
         while Inter /= Null_Iir loop
            Elaborate_Port_Declaration (Formal_Instance, Inter, null);
            Inter := Get_Chain (Inter);
         end loop;
         return;
      end if;

      Assoc := Map;
      Port := Ports;
      while Assoc /= Null_Iir loop
         --  Elaboration of a port association list consists of the elaboration
         --  of each port association element in the association list whose
         --  actual is not the reserved word OPEN.
         Inter := Get_Association_Interface (Assoc, Port);
         case Get_Kind (Assoc) is
            when Iir_Kinds_Association_Element_By_Actual =>
               if Get_Actual_Conversion (Assoc) = Null_Iir
                 and then Get_Formal_Conversion (Assoc) = Null_Iir
               then
                  Actual := Get_Actual (Assoc);
                  Formal := Get_Association_Formal (Assoc, Inter);
                  if Is_Signal_Name (Actual) then
                     --  Association with a signal
                     Init_Expr := Execute_Signal_Name
                       (Actual_Instance, Actual, Signal_Val);
                     Implicit_Array_Conversion
                       (Formal_Instance, Init_Expr, Get_Type (Formal), Actual);
                     Init_Expr := Unshare_Bounds
                       (Init_Expr, Global_Pool'Access);
                     Actual_Expr := null;
                  else
                     --  Association with an expression
                     Init_Expr := Execute_Association_Expression
                       (Actual_Instance, Actual, Formal_Instance);
                     Implicit_Array_Conversion
                       (Formal_Instance, Init_Expr, Get_Type (Formal), Actual);
                     Init_Expr := Unshare (Init_Expr, Global_Pool'Access);
                     Actual_Expr := Init_Expr;
                  end if;
               else
                  --  The actual doesn't define the constraints of the formal.
                  if Get_Whole_Association_Flag (Assoc) then
                     Init_Expr := Elaborate_Default_Value
                       (Formal_Instance, Inter);
                     Actual_Expr := null;
                  end if;
               end if;

               if Get_Whole_Association_Flag (Assoc)
                 and then Get_Collapse_Signal_Flag (Assoc)
               then
                  pragma Assert (Get_Actual_Conversion (Assoc) = Null_Iir);
                  pragma Assert (Get_Formal_Conversion (Assoc) = Null_Iir);
                  pragma Assert (Is_Signal_Name (Get_Actual (Assoc)));
                  declare
                     Slot : constant Object_Slot_Type :=
                       Get_Info (Inter).Slot;
                     Actual_Sig : Iir_Value_Literal_Acc;
                     Default_Value : Iir;
                     Val : Iir_Value_Literal_Acc;
                  begin
                     Actual_Sig :=
                       Execute_Name (Actual_Instance, Actual, True);
                     Implicit_Array_Conversion
                       (Formal_Instance, Actual_Sig,
                        Get_Type (Inter), Actual);
                     Formal_Instance.Objects (Slot) := Unshare_Bounds
                       (Actual_Sig, Global_Pool'Access);
                     Formal_Instance.Objects (Slot + 1) := Init_Expr;
                     if Get_Mode (Inter) = Iir_Out_Mode then
                        Default_Value := Get_Default_Value (Inter);
                        if Default_Value /= Null_Iir then
                           Val := Execute_Expression_With_Type
                             (Formal_Instance, Default_Value,
                              Get_Type (Inter));
                           Val := Unshare (Val, Global_Pool'Access);
                        else
                           Val := Unshare (Init_Expr, Global_Pool'Access);
                           Init_To_Default
                             (Val, Formal_Instance, Get_Type (Inter));
                        end if;
                        Formal_Instance.Objects (Slot + 2) := Val;
                        Store (Formal_Instance.Objects (Slot + 1), Val);
                     else
                        -- Always set a value to the driver.
                        Formal_Instance.Objects (Slot + 2) := Init_Expr;
                     end if;
                  end;
               else
                  if Get_Whole_Association_Flag (Assoc) then
                     Elaborate_Signal (Formal_Instance, Inter, Init_Expr);
                  end if;

                  --  Elaboration of a port association element consists of the
                  --  elaboration of the formal part; the port or subelement
                  --  or slice thereof designated by the formal part is then
                  --  associated with the signal or expression designated
                  --  by the actual part.
                  Elab_Connect (Formal_Instance, Actual_Instance, Actual_Expr,
                                Assoc, Inter);
               end if;

            when Iir_Kind_Association_Element_Open =>
               --  Note that an open cannot be associated with a formal that
               --  is associated individually.
               Elaborate_Port_Declaration (Formal_Instance, Inter, null);

            when Iir_Kind_Association_Element_By_Individual =>
               Init_Expr := Create_Value_For_Type
                 (Formal_Instance, Get_Actual_Type (Assoc), Init_Value_Any);
               Elaborate_Signal (Formal_Instance, Inter, Init_Expr);

            when others =>
               Error_Kind ("elaborate_port_map_aspect", Assoc);
         end case;
         Next_Association_Interface (Assoc, Port);
      end loop;
   end Elaborate_Port_Map_Aspect;

   --  LRM93 12.2  Elaboration of a block header
   --  Elaboration of a block header consists of the elaboration of the
   --  generic clause, the generic map aspect, the port clause, and the port
   --  map aspect, in that order.
   procedure Elaborate_Block_Header
     (Instance : Block_Instance_Acc; Header : Iir_Block_Header)
   is
   begin
      Elaborate_Generic_Clause (Instance, Get_Generic_Chain (Header));
      Elaborate_Generic_Map_Aspect
        (Instance, Instance,
         Get_Generic_Chain (Header),
         Get_Generic_Map_Aspect_Chain (Header));
      Elaborate_Port_Clause (Instance, Get_Port_Chain (Header));
      Elaborate_Port_Map_Aspect
        (Instance, Instance,
         Get_Port_Chain (Header), Get_Port_Map_Aspect_Chain (Header));
   end Elaborate_Block_Header;

   procedure Elaborate_Guard_Signal
     (Instance : Block_Instance_Acc; Guard : Iir)
   is
      Sig, Val : Iir_Value_Literal_Acc;
      Info : constant Sim_Info_Acc := Get_Info (Guard);
   begin
      Create_Signal (Instance, Guard);

      Sig := Create_Signal_Value (null);
      Val := Unshare (Create_B1_Value (False), Instance_Pool);
      Instance.Objects (Info.Slot) := Sig;
      Instance.Objects (Info.Slot + 1) := Val;

      Signals_Table.Append ((Kind => Mode_Guard,
                             Decl => Guard,
                             Sig => Sig,
                             Val => Val,
                             Instance => Instance));
   end Elaborate_Guard_Signal;

   --  LRM93 12.4.1  Block statements.
   procedure Elaborate_Block_Statement
     (Instance : Block_Instance_Acc; Block : Iir_Block_Statement)
   is
      Header : Iir_Block_Header;
      Ninstance : Block_Instance_Acc;  -- FIXME
      Guard : Iir;
   begin
      Ninstance := Create_Block_Instance (Instance, Block, Block);

      Guard := Get_Guard_Decl (Block);
      if Guard /= Null_Iir then
         --  LRM93 12.6.4 (3)
         --  The value of each implicit GUARD signal is set to the result of
         --  evaluating the corresponding guard expression.
         --  GHDL: done by grt when the guard signal is created.
         Elaborate_Guard_Signal (Ninstance, Guard);
      end if;

      --  Elaboration of a block statement consists of the elaboration of the
      --  block header, if present [...]
      Header := Get_Block_Header (Block);
      if Header /= Null_Iir then
         Elaborate_Block_Header (Ninstance, Header);
      end if;

      --  [...] followed by the elaboration of the block declarative part [...]
      Elaborate_Declarative_Part (Ninstance,
                                  Get_Declaration_Chain (Block));
      --  [...] followed by the elaboration of the block statement part.
      Elaborate_Concurrent_Statement_Part
        (Ninstance, Get_Concurrent_Statement_Chain (Block));
      --  Elaboration of a block statement may occur under the control of a
      --  configuration declaration.
      --  In particular, a block configuration, whether implicit or explicit,
      --  within a configuration declaration may supply a sequence of
      --  additionnal implicit configuration specification to be applied
      --  during the elaboration of the corresponding block statement.
      --  If a block statement is being elaborated under the control of a
      --  configuration declaration, then the sequence of implicit
      --  configuration specifications supplied by the block configuration
      --  is elaborated as part of the block declarative part, following all
      --  other declarative items in that part.
      --  The sequence of implicit configuration specifications supplied by a
      --  block configuration, whether implicit or explicit, consists of each
      --  of the configuration specifications implied by component
      --  configurations occurring immediatly within the block configuration,
      --  and in the order in which the component configurations themselves
      --  appear.
      -- FIXME.
   end Elaborate_Block_Statement;

   function Create_Default_Association (Formal_Chain : Iir;
                                        Local_Chain : Iir;
                                        Node : Iir)
                                        return Iir
   is
      Nbr_Formals : Natural;
   begin
      --  LRM93 5.2.2
      --  The default binding indication includes a default generic map
      --  aspect if the design entity implied by the entity aspect contains
      --  formal generic.
      --
      --  LRM93 5.2.2
      --  The default binding indication includes a default port map aspect if
      --  the design entity implied by the entity aspect contains formal ports.
      if Formal_Chain = Null_Iir then
         if Local_Chain /= Null_Iir then
            Error_Msg_Sem (+Node, "cannot create default map aspect");
         end if;
         return Null_Iir;
      end if;
      Nbr_Formals := Get_Chain_Length (Formal_Chain);
      declare
         Assoc_List : Iir_Array (0 .. Nbr_Formals - 1) := (others => Null_Iir);
         Assoc : Iir;
         Local : Iir;
         Formal : Iir;
         Pos : Natural;
         First, Last : Iir;
      begin
         --  LRM93 5.2.2
         --  The default generic map aspect associates each local generic in
         --  the corresponding component instantiation (if any) with a formal
         --  of the same simple name.
         Local := Local_Chain;
         while Local /= Null_Iir loop
            Formal := Formal_Chain;
            Pos := 0;
            while Formal /= Null_Iir loop
               exit when Get_Identifier (Formal) = Get_Identifier (Local);
               Formal := Get_Chain (Formal);
               Pos := Pos + 1;
            end loop;
            if Formal = Null_Iir then
               --  LRM93 5.2.2
               --  It is an error if such a formal does not exist, or if
               --  its mode and type are not appropriate for such an
               --  association.
               --  FIXME: mode/type check.
               Error_Msg_Sem (+Node, "cannot associate local %n", +Local);
               exit;
            end if;
            if Assoc_List (Pos) /= Null_Iir then
               raise Internal_Error;
            end if;
            Assoc_List (Pos) := Local;

            Local := Get_Chain (Local);
         end loop;

         Chain_Init (First, Last);
         Formal := Formal_Chain;
         for I in Assoc_List'Range loop
            if Assoc_List (I) = Null_Iir then
               --  LRM93 5.2.2
               --  Any remaining unassociated formals are associated with the
               --  actual designator any.
               Assoc := Create_Iir (Iir_Kind_Association_Element_Open);
            else
               Assoc :=
                 Create_Iir (Iir_Kind_Association_Element_By_Expression);
               Set_Actual (Assoc, Assoc_List (I));
            end if;
            Set_Whole_Association_Flag (Assoc, True);
            Set_Formal (Assoc, Formal);
            Chain_Append (First, Last, Assoc);

            Formal := Get_Chain (Formal);
         end loop;
         return First;
      end;
   end Create_Default_Association;

   --  LRM93 12.4.3
   function Is_Fully_Bound (Conf : Iir) return Boolean
   is
      Binding : Iir;
   begin
      if Conf = Null_Iir then
         return False;
      end if;
      case Get_Kind (Conf) is
         when Iir_Kind_Configuration_Specification
           | Iir_Kind_Component_Configuration =>
            Binding := Get_Binding_Indication (Conf);
            if Binding = Null_Iir then
               return False;
            end if;
            if Get_Kind (Get_Entity_Aspect (Binding))
              = Iir_Kind_Entity_Aspect_Open
            then
               return False;
            end if;
         when others =>
            null;
      end case;
      return True;
   end Is_Fully_Bound;

   procedure Elaborate_Component_Instantiation
     (Instance : Block_Instance_Acc;
      Stmt : Iir_Component_Instantiation_Statement)
   is
      Frame : Block_Instance_Acc;
   begin
      if Is_Component_Instantiation (Stmt) then
         declare
            Component : constant Iir :=
              Get_Named_Entity (Get_Instantiated_Unit (Stmt));
         begin
            --  Elaboration of a component instantiation statement that
            --  instanciates a component declaration has no effect unless the
            --  component instance is either fully bound to a design entity
            --  defined by an entity declaration and architecture body or is
            --  bound to a configuration of such a design entity.
            --  FIXME: in fact the component is created.

            --  If a component instance is so bound, then elaboration of the
            --  corresponding component instantiation statement consists of the
            --  elaboration of the implied block statement representing the
            --  component instance and [...]
            Frame := Create_Block_Instance (Instance, Component, Stmt);

            Elaborate_Generic_Clause (Frame, Get_Generic_Chain (Component));
            Elaborate_Generic_Map_Aspect
              (Frame, Instance,
               Get_Generic_Chain (Component),
               Get_Generic_Map_Aspect_Chain (Stmt));
            Elaborate_Port_Clause (Frame, Get_Port_Chain (Component));
            Elaborate_Port_Map_Aspect
              (Frame, Instance,
               Get_Port_Chain (Component), Get_Port_Map_Aspect_Chain (Stmt));
         end;
      else
         --  Direct instantiation
         declare
            Aspect : constant Iir := Get_Instantiated_Unit (Stmt);
            Arch : Iir;
            Config : Iir;
         begin
            case Iir_Kinds_Entity_Aspect (Get_Kind (Aspect)) is
               when Iir_Kind_Entity_Aspect_Entity =>
                  Arch := Get_Architecture (Aspect);
                  if Arch = Null_Iir then
                     Arch := Libraries.Get_Latest_Architecture
                       (Get_Entity (Aspect));
                  else
                     Arch := Strip_Denoting_Name (Arch);
                  end if;
                  Config := Get_Library_Unit
                    (Get_Default_Configuration_Declaration (Arch));
               when Iir_Kind_Entity_Aspect_Configuration =>
                  Config := Get_Configuration (Aspect);
                  Arch := Get_Block_Specification
                    (Get_Block_Configuration (Config));
               when Iir_Kind_Entity_Aspect_Open =>
                  return;
            end case;
            Config := Get_Block_Configuration (Config);

            Frame := Elaborate_Architecture
              (Arch, Config, Instance, Stmt,
               Get_Generic_Map_Aspect_Chain (Stmt),
               Get_Port_Map_Aspect_Chain (Stmt));
         end;
      end if;
   end Elaborate_Component_Instantiation;

   procedure Elaborate_Generate_Statement_Body
     (Instance : Block_Instance_Acc; Bod : Iir)is
   begin
      Elaborate_Declarative_Part
        (Instance, Get_Declaration_Chain (Bod));
      Elaborate_Concurrent_Statement_Part
        (Instance, Get_Concurrent_Statement_Chain (Bod));
   end Elaborate_Generate_Statement_Body;

   --  LRM93 12.4.2 Generate Statements
   procedure Elaborate_If_Generate_Statement
     (Instance : Block_Instance_Acc; Generate : Iir_Generate_Statement)
   is
      Clause : Iir;
      Cond : Iir;
      Bod : Iir;
      Ninstance : Block_Instance_Acc;
      Lit : Iir_Value_Literal_Acc;
   begin
      --  LRM93 12.4.2
      --  For a generate statement with an if generation scheme, elaboration
      --  consists of the evaluation of the boolean expression, followed by
      --  the generation of exactly one block statement if the expression
      --  evaluates to TRUE, and no block statement otherwise.
      Clause := Generate;
      while Clause /= Null_Iir loop
         Cond := Get_Condition (Clause);
         if Cond /= Null_Iir then
            Lit := Execute_Expression (Instance, Cond);
         end if;
         if Cond = Null_Iir or else Lit.B1 = True then
            --  LRM93 12.4.2
            --  If generated, the block statement has the following form:
            --  1.  The block label is the same as the label of the generate
            --      statement.
            --  2.  The block declarative part consists of a copy of the
            --      declarative items contained within the generate statement.
            --  3.  The block statement part consists of a copy of the
            --      concurrent statement contained within the generate
            --      statement.
            Bod := Get_Generate_Statement_Body (Clause);
            Ninstance := Create_Block_Instance (Instance, Bod, Bod);
            Elaborate_Generate_Statement_Body (Ninstance, Bod);

            exit;
         end if;
         Clause := Get_Generate_Else_Clause (Clause);
      end loop;
   end Elaborate_If_Generate_Statement;

   --  LRM93 12.4.2 Generate Statements
   procedure Elaborate_For_Generate_Statement
     (Instance : Block_Instance_Acc; Generate : Iir_Generate_Statement)
   is
      Iter : constant Iir := Get_Parameter_Specification (Generate);
      Bod : constant Iir := Get_Generate_Statement_Body (Generate);
      Ninstance : Block_Instance_Acc;
      Sub_Instance : Block_Instance_Acc;
      Bound, Index : Iir_Value_Literal_Acc;
   begin
      --  LRM93 12.4.2
      --  For a generate statement with a for generation scheme, elaboration
      --  consists of the elaboration of the discrete range

      Ninstance := Create_Block_Instance (Instance, Bod, Bod);
      Elaborate_Declaration (Ninstance, Iter);
      Bound := Execute_Bounds (Ninstance, Get_Type (Iter));

      --  Index is the iterator value.
      Index := Unshare (Ninstance.Objects (Get_Info (Iter).Slot),
                        Current_Pool);

      --  Initialize the iterator.
      Store (Index, Bound.Left);

      if not Is_In_Range (Index, Bound) then
         --  Well, this instance should have never been built.
         --  Should be destroyed ??
         return;
      end if;

      loop
         Sub_Instance := Create_Block_Instance (Ninstance, Bod, Iter);

         --  FIXME: this is needed to copy iterator type (if any).  But this
         --  elaborates the subtype several times (what about side effects).
         Elaborate_Declaration (Sub_Instance, Iter);

         --  Store index.
         Store (Sub_Instance.Objects (Get_Info (Iter).Slot), Index);

         Elaborate_Generate_Statement_Body (Sub_Instance, Bod);

         exit when Is_Equal (Index, Bound.Right);
         Update_Loop_Index (Index, Bound);
      end loop;
      --  FIXME: destroy index ?
   end Elaborate_For_Generate_Statement;

   procedure Elaborate_Case_Generate_Statement
     (Instance : Block_Instance_Acc; Stmt : Iir)
   is
      Lit : Iir_Value_Literal_Acc;
      Assoc : Iir;
      Bod : Iir;
      Ninstance : Block_Instance_Acc;
   begin
      Lit := Execute_Expression (Instance, Get_Expression (Stmt));
      Assoc := Get_Case_Statement_Alternative_Chain (Stmt);

      while Assoc /= Null_Iir loop
         if not Get_Same_Alternative_Flag (Assoc) then
            Bod := Get_Associated_Block (Assoc);
         end if;

         if Is_In_Choice (Instance, Assoc, Lit) then
            Ninstance := Create_Block_Instance (Instance, Bod, Bod);
            Elaborate_Generate_Statement_Body (Ninstance, Bod);
            return;
         end if;

         Assoc := Get_Chain (Assoc);
      end loop;
      raise Internal_Error;
   end Elaborate_Case_Generate_Statement;

   procedure Elaborate_Process_Statement
     (Instance : Block_Instance_Acc; Stmt : Iir)
   is
      Proc_Instance : Block_Instance_Acc;
   begin
      Proc_Instance := Create_Block_Instance (Instance, Stmt, Stmt);

      Processes_Table.Append (Proc_Instance);

      --  Processes aren't elaborated here.  They are elaborated
      --  just before simulation.
   end Elaborate_Process_Statement;

   procedure Elaborate_Psl_Directive
     (Instance : Block_Instance_Acc; Stmt : Iir)
   is
   begin
      --  Create the state vector (and initialize it).
      --  Create the bool flag (for cover)
      --  Create the process
      --  Create the finalizer
      PSL_Table.Append (PSL_Entry'(Instance, Stmt, null, False));

      if Get_Kind (Stmt) = Iir_Kind_Psl_Endpoint_Declaration then
         declare
            Info : constant Sim_Info_Acc := Get_Info (Stmt);
         begin
            Create_Object (Instance, Info.Slot);
            Instance.Objects (Info.Slot) :=
              Unshare (Lit_Boolean_False, Instance_Pool);
         end;
      end if;
   end Elaborate_Psl_Directive;

   --  LRM93 12.4  Elaboration of a Statement Part.
   procedure Elaborate_Concurrent_Statement_Part
     (Instance : Block_Instance_Acc; Stmt_Chain: Iir)
   is
      Stmt : Iir;
   begin
      --  Concurrent statements appearing in the statement part of a block
      --  must be elaborated before execution begins.
      --  Elaboration of the statement part of a block consists of the
      --  elaboration of each concurrent statement in the order given.
      Stmt := Stmt_Chain;
      while Stmt /= Null_Iir loop
         case Get_Kind (Stmt) is
            when Iir_Kind_Block_Statement =>
               Elaborate_Block_Statement (Instance, Stmt);

            when Iir_Kind_Sensitized_Process_Statement
              | Iir_Kind_Process_Statement =>
               Elaborate_Process_Statement (Instance, Stmt);

            when Iir_Kind_Component_Instantiation_Statement =>
               Elaborate_Component_Instantiation (Instance, Stmt);

            when Iir_Kind_If_Generate_Statement =>
               Elaborate_If_Generate_Statement (Instance, Stmt);

            when Iir_Kind_For_Generate_Statement =>
               Elaborate_For_Generate_Statement (Instance, Stmt);

            when Iir_Kind_Case_Generate_Statement =>
               Elaborate_Case_Generate_Statement (Instance, Stmt);

            when Iir_Kind_Simple_Simultaneous_Statement =>
               Add_Characteristic_Expression
                 (Explicit,
                  Build (Op_Plus,
                         Instance, Get_Simultaneous_Right (Stmt),
                         Build (Op_Minus,
                                Instance, Get_Simultaneous_Left (Stmt))));

            when Iir_Kind_Psl_Default_Clock
              | Iir_Kind_Psl_Declaration =>
               null;

            when Iir_Kind_Psl_Cover_Directive
              | Iir_Kind_Psl_Assert_Directive
              | Iir_Kind_Psl_Assume_Directive
              | Iir_Kind_Psl_Endpoint_Declaration =>
               Elaborate_Psl_Directive (Instance, Stmt);

            when Iir_Kind_Concurrent_Simple_Signal_Assignment
              | Iir_Kind_Concurrent_Selected_Signal_Assignment
              | Iir_Kind_Concurrent_Conditional_Signal_Assignment =>
               --  In case concurrent signal assignemnts were not
               --  canonicalized.
               null;

            when others =>
               Error_Kind ("elaborate_concurrent_statement_part", Stmt);
         end case;
         Stmt := Get_Chain (Stmt);
      end loop;

      --  Put children in order (were prepended, so in reverse order).
      declare
         Last, Child : Block_Instance_Acc;
         Next_Child : Block_Instance_Acc;
      begin
         Child := Instance.Children;
         Last := null;
         while Child /= null loop
            Next_Child := Child.Brother;
            Child.Brother := Last;
            Last := Child;
            Child := Next_Child;
         end loop;
         Instance.Children := Last;
      end;
   end Elaborate_Concurrent_Statement_Part;

   --  Compute the default value for declaration DECL, using either
   --  DEFAULT_VALUE if not null, or the implicit default value for DECL.
   --  DECL must have a type.
   function Elaborate_Default_Value (Instance : Block_Instance_Acc; Decl : Iir)
                                    return Iir_Value_Literal_Acc
   is
      Default_Value : constant Iir := Get_Default_Value (Decl);
      Val : Iir_Value_Literal_Acc;
   begin
      if Default_Value /= Null_Iir then
         Val := Execute_Expression_With_Type
           (Instance, Default_Value, Get_Type (Decl));
      else
         Val := Create_Value_For_Type
           (Instance, Get_Type (Decl), Init_Value_Default);
      end if;
      return Val;
   end Elaborate_Default_Value;

   --  LRM93 12.3.1.1  Subprogram Declaration and Bodies
   procedure Elaborate_Interface_List
     (Instance : Block_Instance_Acc; Inter_Chain : Iir)
   is
      Inter : Iir;
   begin
      --  elaboration of the parameter interface list
      --  this in turn involves the elaboration of the subtype indication of
      --  each interface element to determine the subtype of each formal
      --  parameter of the subprogram.
      Inter := Inter_Chain;
      while Inter /= Null_Iir loop
         case Get_Kind (Inter) is
            when Iir_Kind_Interface_Signal_Declaration
              | Iir_Kind_Interface_Variable_Declaration
              | Iir_Kind_Interface_Constant_Declaration
              | Iir_Kind_Interface_File_Declaration =>
               Elaborate_Subtype_Indication_If_Anonymous
                 (Instance, Get_Type (Inter));
            when others =>
               Error_Kind ("elaborate_interface_list", Inter);
         end case;
         Inter := Get_Chain (Inter);
      end loop;
   end Elaborate_Interface_List;

   --  LRM93 12.3.1.1  Subprogram Declaration and Bodies
   procedure Elaborate_Subprogram_Declaration
     (Instance : Block_Instance_Acc; Decl : Iir)
   is
   begin
      --  Elaboration of a subprogram declaration involves the elaboration
      --  of the parameter interface list of the subprogram declaration; [...]
      Elaborate_Interface_List
        (Instance, Get_Interface_Declaration_Chain (Decl));

      --  Elaboration of a subprogram body has no effect other than to
      --  establish that the body can, from then on, be used for the
      --  execution of calls of the subprogram.
      --  FIXME
      null;
   end Elaborate_Subprogram_Declaration;

   procedure Elaborate_Component_Configuration
     (Stmt : Iir_Component_Instantiation_Statement;
      Comp_Instance : Block_Instance_Acc;
      Conf : Iir_Component_Configuration)
   is
      Component : constant Iir_Component_Declaration :=
        Get_Named_Entity (Get_Instantiated_Unit (Stmt));
      Entity : Iir_Entity_Declaration;
      Arch_Name : Name_Id;
      Arch_Design : Iir_Design_Unit;
      Arch : Iir_Architecture_Body;
      Arch_Frame : Block_Instance_Acc;
      pragma Unreferenced (Arch_Frame);
      Generic_Map_Aspect_Chain : Iir;
      Port_Map_Aspect_Chain : Iir;
      Binding : Iir_Binding_Indication;
      Aspect : Iir;
      Sub_Conf : Iir;
   begin
      if Trace_Elaboration then
         Simple_IO.Put ("configure component ");
         Simple_IO.Put (Name_Table.Image (Get_Label (Stmt)));
         Simple_IO.Put (": ");
         Simple_IO.Put_Line (Image_Identifier (Component));
      end if;

      --  Elaboration of a component instantiation statement that instanciates
      --  a component declaration has no effect unless the component instance
      --  is either fully bound to a design entity defined by an entity
      --  declaration and architecture body or is bound to a configuration of
      --  such a design entity.
      if not Is_Fully_Bound (Conf) then
         Warning_Msg_Elab
           (Warnid_Binding, Stmt, "%n not bound", +Stmt);
         return;
      end if;

      if Trace_Elaboration then
         Simple_IO.Put_Line
           (" using " & Disp_Node (Conf) & " from " & Disp_Location (Conf));
      end if;

      --  If a component instance is so bound, then elaboration of the
      --  corresponding component instantiation statement consists of the
      --  elaboration of the implied block statement representing the
      --  component instance and [...]
      --  FIXME: extract frame.

      --  and (within that block) the implied block statement representing the
      --  design entity to which the component instance is so bound.
      Arch := Null_Iir;
      Arch_Name := Null_Identifier;
      Binding := Get_Binding_Indication (Conf);
      Aspect := Get_Entity_Aspect (Binding);

      case Get_Kind (Conf) is
         when Iir_Kind_Component_Configuration =>
            Sub_Conf := Get_Block_Configuration (Conf);
         when Iir_Kind_Configuration_Specification =>
            Sub_Conf := Null_Iir;
         when others =>
            raise Internal_Error;
      end case;

      case Get_Kind (Aspect) is
         when Iir_Kind_Design_Unit =>
            raise Internal_Error;
         when Iir_Kind_Entity_Aspect_Entity =>
            Entity := Get_Entity (Aspect);
            if Get_Architecture (Aspect) /= Null_Iir then
               Arch_Name := Get_Identifier
                 (Strip_Denoting_Name (Get_Architecture (Aspect)));
            end if;
         when Iir_Kind_Entity_Aspect_Configuration =>
            if Sub_Conf /= Null_Iir then
               raise Internal_Error;
            end if;
            declare
               Conf : constant Iir := Get_Configuration (Aspect);
            begin
               Entity := Get_Entity (Conf);
               Sub_Conf := Get_Block_Configuration (Conf);
               Arch := Get_Named_Entity (Get_Block_Specification (Sub_Conf));
            end;
         when others =>
            Error_Kind ("elaborate_component_declaration0", Aspect);
      end case;

      if Arch = Null_Iir then
         if Arch_Name = Null_Identifier then
            Arch := Libraries.Get_Latest_Architecture (Entity);
            if Arch = Null_Iir then
               Error_Msg_Elab
                 (Stmt, "no architecture analysed for %n", +Entity);
            end if;
            Arch_Name := Get_Identifier (Arch);
         end if;
         Arch_Design := Load_Secondary_Unit
           (Get_Design_Unit (Entity), Arch_Name, Stmt);
         if Arch_Design = Null_Iir then
            Error_Msg_Elab
              (Stmt, "no architecture %i for %n", (+Arch_Name, +Entity));
         end if;
         Arch := Get_Library_Unit (Arch_Design);
      end if;

      Generic_Map_Aspect_Chain := Get_Generic_Map_Aspect_Chain (Binding);
      Port_Map_Aspect_Chain := Get_Port_Map_Aspect_Chain (Binding);

      if Generic_Map_Aspect_Chain = Null_Iir then
         --  LRM93 5.2.2
         --  The default binding indication includes a default generic map
         --  aspect if the design entity implied by the entity aspect contains
         --  formal generic
         --  GHDL: this condition is checked by create_default_association.
         Generic_Map_Aspect_Chain :=
           Create_Default_Association (Get_Generic_Chain (Entity),
                                       Get_Generic_Chain (Component),
                                       Stmt);
      end if;

      if Port_Map_Aspect_Chain = Null_Iir then
         Port_Map_Aspect_Chain :=
           Create_Default_Association (Get_Port_Chain (Entity),
                                       Get_Port_Chain (Component),
                                       Stmt);
      end if;

      if Sub_Conf = Null_Iir then
         Sub_Conf := Get_Default_Configuration_Declaration (Arch);
         Sub_Conf := Get_Block_Configuration (Get_Library_Unit (Sub_Conf));
      end if;

      --  FIXME: Use Sub_Conf instead of Arch for Stmt ? (But need to add
      --  info for block configuration).
      Arch_Frame := Elaborate_Architecture
        (Arch, Sub_Conf, Comp_Instance, Arch,
         Generic_Map_Aspect_Chain, Port_Map_Aspect_Chain);
   end Elaborate_Component_Configuration;

   procedure Elaborate_Block_Configuration
     (Conf : Iir_Block_Configuration; Instance : Block_Instance_Acc);

   procedure Apply_Block_Configuration_To_Iterative_Generate
     (Stmt : Iir; Conf_Chain : Iir; Instance : Block_Instance_Acc)
   is
      Scheme : constant Iir := Get_Parameter_Specification (Stmt);
      Bounds : constant Iir_Value_Literal_Acc :=
        Execute_Bounds (Instance, Get_Type (Scheme));

      Sub_Instances : Block_Instance_Acc_Array
        (0 .. Instance_Slot_Type (Bounds.Length - 1));

      type Sub_Conf_Type is array (0 .. Instance_Slot_Type (Bounds.Length - 1))
        of Boolean;
      Sub_Conf : Sub_Conf_Type := (others => False);

      Child : Block_Instance_Acc;

      Item : Iir;
      Prev_Item : Iir;
      Default_Item : Iir := Null_Iir;
      Spec : Iir;
      Expr : Iir_Value_Literal_Acc;
      Ind : Instance_Slot_Type;
   begin
      --  Gather children.
      Child := Instance.Children;
      for I in Sub_Instances'Range loop
         Sub_Instances (I) := Child;
         Child := Child.Brother;
      end loop;
      --  All children must have been handled.
      pragma Assert (Child = null);

      --  Apply configuration items
      Item := Conf_Chain;
      while Item /= Null_Iir loop
         Spec := Strip_Denoting_Name (Get_Block_Specification (Item));
         Prev_Item := Get_Prev_Block_Configuration (Item);

         case Get_Kind (Spec) is
            when Iir_Kind_Slice_Name =>
               Expr := Execute_Bounds (Instance, Get_Suffix (Spec));
               Ind := Instance_Slot_Type
                 (Get_Index_Offset (Execute_Low_Limit (Expr), Bounds, Spec));
               for I in 1 .. Instance_Slot_Type (Expr.Length) loop
                  Sub_Conf (Ind + I - 1) := True;
                  Elaborate_Block_Configuration
                    (Item, Sub_Instances (Ind + I - 1));
               end loop;
            when Iir_Kind_Indexed_Name =>
               if Get_Index_List (Spec) = Iir_Flist_Others then
                  --  Must be the only default block configuration
                  pragma Assert (Default_Item = Null_Iir);
                  Default_Item := Item;
               else
                  Expr := Execute_Expression
                    (Instance, Get_Nth_Element (Get_Index_List (Spec), 0));
                  Ind := Instance_Slot_Type
                    (Get_Index_Offset (Expr, Bounds, Spec));
                  Sub_Conf (Ind) := True;
                  Elaborate_Block_Configuration (Item, Sub_Instances (Ind));
               end if;
            when Iir_Kind_Generate_Statement_Body =>
               --  Must be the only block configuration
               pragma Assert (Item = Conf_Chain);
               pragma Assert (Prev_Item = Null_Iir);
               for I in Sub_Instances'Range loop
                  Sub_Conf (I) := True;
                  Elaborate_Block_Configuration (Item, Sub_Instances (I));
               end loop;
            when others =>
               raise Internal_Error;
         end case;
         Item := Prev_Item;
      end loop;

      --  Default configuration.
      if Default_Item /= Null_Iir then
         for I in Sub_Instances'Range loop
            if not Sub_Conf (I) then
               Elaborate_Block_Configuration
                 (Default_Item, Sub_Instances (I));
            end if;
         end loop;
      end if;
   end Apply_Block_Configuration_To_Iterative_Generate;

   procedure Elaborate_Block_Configuration
     (Conf : Iir_Block_Configuration; Instance : Block_Instance_Acc)
   is
      Blk_Info : constant Sim_Info_Acc := Get_Info (Instance.Stmt);
      Sub_Instances : Block_Instance_Acc_Array
        (0 .. Blk_Info.Nbr_Instances - 1);
      type Iir_Array is array (Instance_Slot_Type range <>) of Iir;
      Sub_Conf : Iir_Array (0 .. Blk_Info.Nbr_Instances - 1) :=
        (others => Null_Iir);

      Item : Iir;
   begin
      --  Gather children.
      declare
         Child : Block_Instance_Acc;
         Info : Sim_Info_Acc;
      begin
         Child := Instance.Children;
         while Child /= null loop
            Info := Get_Info (Child.Label);
            if Info.Kind = Kind_Block then
               pragma Assert (Info.Inst_Slot /= Invalid_Instance_Slot);
               pragma Assert (Sub_Instances (Info.Inst_Slot) = null);
               Sub_Instances (Info.Inst_Slot) := Child;
            end if;
            Child := Child.Brother;
         end loop;
      end;

      --  Associate configuration items with subinstance.  Gather items for
      --  for-generate statements.
      Item := Get_Configuration_Item_Chain (Conf);
      while Item /= Null_Iir loop
         case Get_Kind (Item) is
            when Iir_Kind_Block_Configuration =>
               declare
                  Spec : Iir;
                  Gen : Iir_Generate_Statement;
                  Info : Sim_Info_Acc;
               begin
                  Spec := Strip_Denoting_Name (Get_Block_Specification (Item));
                  case Get_Kind (Spec) is
                     when Iir_Kind_Slice_Name
                       | Iir_Kind_Indexed_Name
                       | Iir_Kind_Selected_Name =>
                        --  Block configuration for a generate statement.
                        Gen := Get_Named_Entity (Get_Prefix (Spec));
                        Info := Get_Info (Gen);
                        Set_Prev_Block_Configuration
                          (Item, Sub_Conf (Info.Inst_Slot));
                        Sub_Conf (Info.Inst_Slot) := Item;
                     when Iir_Kind_Parenthesis_Name =>
                        Gen := Get_Named_Entity (Spec);
                        Info := Get_Info (Gen);
                        if Sub_Instances (Info.Inst_Slot) /= null
                          and then Sub_Instances (Info.Inst_Slot).Label = Gen
                        then
                           pragma Assert
                             (Sub_Conf (Info.Inst_Slot) = Null_Iir);
                           Sub_Conf (Info.Inst_Slot) := Item;
                        end if;
                     when Iir_Kind_Generate_Statement_Body =>
                        Info := Get_Info (Spec);
                        pragma Assert (Sub_Conf (Info.Inst_Slot) = Null_Iir);
                        Sub_Conf (Info.Inst_Slot) := Item;
                     when Iir_Kind_Block_Statement =>
                        --  Block configuration for a block statement.
                        Info := Get_Info (Spec);
                        pragma Assert (Sub_Conf (Info.Inst_Slot) = Null_Iir);
                        Sub_Conf (Info.Inst_Slot) := Item;
                     when others =>
                        Error_Kind ("elaborate_block_configuration1", Spec);
                  end case;
               end;

            when Iir_Kind_Component_Configuration =>
               declare
                  List : constant Iir_Flist :=
                    Get_Instantiation_List (Item);
                  El : Iir;
                  Info : Sim_Info_Acc;
               begin
                  --  The list has been expanded by canon.
                  pragma Assert (List /= Iir_Flist_All
                                   and then List /= Iir_Flist_Others);
                  for I in Flist_First .. Flist_Last (List) loop
                     El := Get_Nth_Element (List, I);
                     Info := Get_Info (Get_Named_Entity (El));
                     pragma Assert (Sub_Conf (Info.Inst_Slot) = Null_Iir);
                     Sub_Conf (Info.Inst_Slot) := Item;
                  end loop;
               end;

            when others =>
               Error_Kind ("elaborate_block_configuration", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;

      --  Configure sub instances.
      for I in Sub_Instances'Range loop
         declare
            Sub_Inst : constant Block_Instance_Acc := Sub_Instances (I);
            Stmt : Iir;
         begin
            if Sub_Inst /= null then
               Stmt := Sub_Inst.Label;
               case Get_Kind (Stmt) is
                  when Iir_Kind_Generate_Statement_Body =>
                     Stmt := Get_Parent (Stmt);
                     case Get_Kind (Stmt) is
                        when Iir_Kind_For_Generate_Statement =>
                           Apply_Block_Configuration_To_Iterative_Generate
                             (Stmt, Sub_Conf (I), Sub_Inst);
                        when Iir_Kind_If_Generate_Statement
                          | Iir_Kind_If_Generate_Else_Clause
                          | Iir_Kind_Case_Generate_Statement =>
                           Elaborate_Block_Configuration
                             (Sub_Conf (I), Sub_Inst);
                        when others =>
                           raise Internal_Error;
                     end case;
               when Iir_Kind_Block_Statement =>
                  Elaborate_Block_Configuration (Sub_Conf (I), Sub_Inst);
               when Iir_Kind_Component_Instantiation_Statement =>
                  if Is_Component_Instantiation (Stmt) then
                     Elaborate_Component_Configuration
                       (Stmt, Sub_Inst, Sub_Conf (I));
                  else
                     --  Nothing to do for entity instantiation, will be
                     --  done during elaboration of statements.
                     null;
                  end if;
               when others =>
                  Error_Kind ("elaborate_block_configuration", Stmt);
               end case;
            end if;
         end;
      end loop;
   end Elaborate_Block_Configuration;

   procedure Elaborate_Alias_Declaration
     (Instance : Block_Instance_Acc; Decl : Iir_Object_Alias_Declaration)
   is
      Alias_Type : Iir;
      Res : Iir_Value_Literal_Acc;
   begin
      --  LRM93 12.3.1.5
      --  Elaboration of an alias declaration consists of the elaboration
      --  of the subtype indication to establish the subtype associated
      --  with the alias, folloed by the creation of the alias as an
      --  alternative name for the named entity.
      --  The creation of an alias for an array object involves a check
      --  that the subtype associated with the alias includes a matching
      --  element for each element of the named object.
      --  It is an error if this check fails.
      Alias_Type := Get_Type (Decl);
      Elaborate_Subtype_Indication_If_Anonymous (Instance, Alias_Type);
      Create_Object (Instance, Decl);
      Res := Execute_Name (Instance, Get_Name (Decl), True);
      Implicit_Array_Conversion (Instance, Res, Alias_Type, Get_Name (Decl));
      Instance.Objects (Get_Info (Decl).Slot) :=
        Unshare_Bounds (Res, Instance_Pool);
   end Elaborate_Alias_Declaration;

   --  LRM93 12.3.2.3  Disconnection Specifications
   procedure Elaborate_Disconnection_Specification
     (Instance : Block_Instance_Acc;
      Decl : Iir_Disconnection_Specification)
   is
      Time_Val : Iir_Value_Literal_Acc;
      Time : Iir_Value_Time;
      List : Iir_Flist;
      Sig : Iir;
      Val : Iir_Value_Literal_Acc;
   begin
      --  LRM93 12.3.2.3
      --  Elaboration of a disconnection specification proceeds as follows:
      --  2. The time expression is evaluated to determine the disconnection
      --     time for drivers of the affected signals.
      Time_Val := Execute_Expression (Instance, Get_Expression (Decl));
      Time := Time_Val.I64;

      --  LRM93 5.3
      --  The time expression in a disconnection specification must be static
      --  and must evaluate to a non-negative value.

      if Time < 0 then
         Error_Msg_Sem (+Decl, "time must be non-negative");
      end if;

      --  LRM93 12.3.2.3
      --  1. The guarded signal specification is elaborated in order to
      --     identify the signals affected by the disconnection specification.
      --
      --  3. The diconnection time is associated with each affected signal for
      --     later use in constructing disconnection statements in the
      --     equivalent processes for guarded assignments to the affected
      --     signals.
      List := Get_Signal_List (Decl);
      case List is
         when Iir_Flist_All
           | Iir_Flist_Others =>
            Error_Kind ("elaborate_disconnection_specification", Decl);
         when others =>
            for I in Flist_First .. Flist_Last (List) loop
               Sig := Get_Nth_Element (List, I);
               Val := Execute_Name (Instance, Sig, True);
               Disconnection_Table.Append ((Sig => Val, Time => Time));
            end loop;
      end case;
   end Elaborate_Disconnection_Specification;

   procedure Elaborate_Branch_Quantity_Declaration
     (Instance : Block_Instance_Acc; Decl : Iir)
   is
      Terminal_Plus, Terminal_Minus : Iir;
      Plus, Minus : Iir_Value_Literal_Acc;
      Res : Iir_Value_Literal_Acc;
   begin
      Res := Create_Quantity (Instance, Decl);

      Terminal_Plus := Get_Plus_Terminal (Decl);
      Plus := Execute_Name (Instance, Terminal_Plus, True);
      Terminal_Minus := Get_Minus_Terminal (Decl);
      if Terminal_Minus = Null_Iir then
         --  Get the reference of the nature
         --  FIXME: select/index
         Terminal_Minus := Get_Reference (Get_Nature (Terminal_Plus));
      end if;
      Minus := Execute_Name (Instance, Terminal_Minus, True);

      case Iir_Kinds_Branch_Quantity_Declaration (Get_Kind (Decl)) is
         when Iir_Kind_Across_Quantity_Declaration =>
            --  Expr: q - P'ref + M'ref
            Add_Characteristic_Expression
              (Structural,
               Build
                 (Op_Plus, Res.Quantity,
                  Build (Op_Minus,
                         Get_Terminal_Reference (Plus.Terminal),
                         Build (Op_Plus,
                                Get_Terminal_Reference (Minus.Terminal)))));
         when Iir_Kind_Through_Quantity_Declaration =>
            --  P'Contrib <- P'Contrib + q
            --  M'Contrib <- M'Contrib - q
            Append_Characteristic_Expression
              (Plus.Terminal, Build (Op_Plus, Res.Quantity));
            Append_Characteristic_Expression
              (Minus.Terminal, Build (Op_Minus, Res.Quantity));
      end case;
   end Elaborate_Branch_Quantity_Declaration;

   procedure Elaborate_Declaration_Type
     (Instance : Block_Instance_Acc; Decl: Iir)
   is
      Ind : constant Iir := Get_Subtype_Indication (Decl);
   begin
      if Ind = Null_Iir or else Get_Kind (Ind) in Iir_Kinds_Denoting_Name then
         return;
      end if;
      Elaborate_Subtype_Indication (Instance, Ind);
   end Elaborate_Declaration_Type;

   --  LRM93 12.3.1  Elaboration of a declaration
   procedure Elaborate_Declaration (Instance : Block_Instance_Acc; Decl : Iir)
   is
      Expr_Mark : Mark_Type;
      Val : Iir_Value_Literal_Acc;
   begin
      Mark (Expr_Mark, Expr_Pool);

      --  Elaboration of a declaration has the effect of creating the declared
      --  item.  For each declaration, the language rules (in particular scope
      --  and visibility rules) are such that it is either impossible or
      --  illegal to use a given item before the elaboration of its
      --  corresponding declaration.
      --  Similarly, it is illegal to call a subprogram before its
      --  corresponding body is elaborated.
      case Get_Kind (Decl) is
         when Iir_Kind_Function_Declaration
           | Iir_Kind_Procedure_Declaration =>
            if Get_Implicit_Definition (Decl) in Iir_Predefined_Explicit
              and then not Is_Second_Subprogram_Specification (Decl)
            then
               Elaborate_Subprogram_Declaration (Instance, Decl);
            end if;
         when Iir_Kind_Anonymous_Type_Declaration =>
            Elaborate_Type_Definition (Instance, Get_Type_Definition (Decl));
         when Iir_Kind_Type_Declaration =>
            Elaborate_Type_Declaration (Instance, Decl);
         when Iir_Kind_Subtype_Declaration =>
            Elaborate_Subtype_Indication (Instance, Get_Type (Decl));
         when Iir_Kind_Iterator_Declaration =>
            Elaborate_Declaration_Type (Instance, Decl);
            Val := Create_Value_For_Type
              (Instance, Get_Type (Decl), Init_Value_Default);
            Create_Object (Instance, Decl);
            Instance.Objects (Get_Info (Decl).Slot) :=
              Unshare (Val, Instance_Pool);
         when Iir_Kind_Signal_Declaration =>
            Elaborate_Declaration_Type (Instance, Decl);
            Val := Elaborate_Default_Value (Instance, Decl);
            Create_Signal (Instance, Decl);
            Elaborate_Signal (Instance, Decl, Val);
         when Iir_Kind_Variable_Declaration =>
            Elaborate_Declaration_Type (Instance, Decl);
            Val := Elaborate_Default_Value (Instance, Decl);
            Create_Object (Instance, Decl);
            Instance.Objects (Get_Info (Decl).Slot) :=
              Unshare (Val, Instance_Pool);
         when Iir_Kind_Constant_Declaration =>
            declare
               Deferred_Decl : constant Iir := Get_Deferred_Declaration (Decl);
               First_Decl : Iir;
            begin
               if Deferred_Decl = Null_Iir
                 or else Get_Deferred_Declaration_Flag (Decl)
               then
                  --  Create the object (except for full declaration of a
                  --  deferred constant).
                  Elaborate_Declaration_Type (Instance, Decl);
                  Create_Object (Instance, Decl);
               end if;
               --  Initialize the value (except for a deferred declaration).
               if Deferred_Decl = Null_Iir then
                  First_Decl := Decl;
               elsif not Get_Deferred_Declaration_Flag (Decl) then
                  First_Decl := Deferred_Decl;
               else
                  First_Decl := Null_Iir;
               end if;
               if First_Decl /= Null_Iir then
                  Val := Execute_Expression_With_Type
                    (Instance, Get_Default_Value (Decl),
                     Get_Type (First_Decl));
                  Instance.Objects (Get_Info (First_Decl).Slot) :=
                    Unshare (Val, Instance_Pool);
               end if;
            end;
         when Iir_Kind_File_Declaration =>
            --  LRM93 12.3.1.4
            --  Elaboration of a file object declaration consists of the
            --  elaboration of the subtype indication...
            null;  -- FIXME ??
            --  ...followed by the creation of object.
            Create_Object (Instance, Decl);
            --  If the file object declaration contains file_open_information,
            --  then the implicit call to FILE_OPEN is then executed.
            Instance.Objects (Get_Info (Decl).Slot) := Unshare
              (File_Operation.Elaborate_File_Declaration (Instance, Decl),
               Instance_Pool);
         when Iir_Kind_Object_Alias_Declaration =>
            Elaborate_Alias_Declaration (Instance, Decl);
         when Iir_Kind_Component_Declaration =>
            --  LRM93 12.3.1.7
            --  Elaboration of a component declaration has no effect other
            --  than to create a template for instantiating component
            --  instances.
            null;
         when Iir_Kind_Function_Body
           | Iir_Kind_Procedure_Body =>
            null;
         when Iir_Kind_Configuration_Specification =>
            --  Elaboration of a configuration specification proceeds as
            --  follows:
            --  1. The component specification is elaborated in order to
            --     determine which component instances are affected by the
            --     configuration specification.
            --  GHDL: this is done during sem.

            --  2. The binding indication is elaborated to identify the design
            --     entity to which the affected component instances will be
            --     bound.
            --  GHDL: this is already done during sem, according to rules
            --     defined by section 5.3.1.1

            --  3. The binding information is associated with each affected
            --     component instance label for later use in instantiating
            --     those component instances.
            --  GHDL: this is done during step 1.

            --  As part of this elaboration process, a check is made that both
            --  the entity declaration and the corresponding architecture body
            --  implied by the binding indication exist whithin the specified
            --  library.
            --  It is an error if this check fails.
            --  GHDL: this is already done during sem, according to rules
            --     defined by section 5.3.1.1
            null;

         when Iir_Kind_Attribute_Declaration =>
            --  LRM93 12.3.1.6
            --  Elaboration of an attribute declaration has no effect other
            --  than to create a template for defining attributes of items.
            null;

         when Iir_Kind_Attribute_Specification =>
            --  LRM93 12.3.2.1
            --  Elaboration of an attribute specification proceeds as follows:
            --  1. The entity specification is elaborated in order to
            --     determine which items are affected by the attribute
            --     specification.
            --  GHDL: done by sem.

            declare
               Attr_Decl : constant Iir :=
                 Get_Named_Entity (Get_Attribute_Designator (Decl));
               Attr_Type : constant Iir := Get_Type (Attr_Decl);
               Value : Iir_Attribute_Value;
               Val : Iir_Value_Literal_Acc;
            begin
               Value := Get_Attribute_Value_Spec_Chain (Decl);
               while Value /= Null_Iir loop
                  Elaborate_Subtype_Indication_If_Anonymous
                    (Instance, Get_Type (Value));
                  --  2. The expression is evaluated to determine the value
                  --     of the attribute.
                  --     It is an error if the value of the expression does not
                  --     belong to the subtype of the attribute; if the
                  --     attribute is of an array type, then an implicit
                  --     subtype conversion is first performed on the value,
                  --     unless the attribute's subtype indication denotes an
                  --     unconstrained array type.
                  Val := Execute_Expression (Instance, Get_Expression (Decl));
                  Check_Constraints (Instance, Val, Attr_Type, Decl);

                  --  3. A new instance of the designated attribute is created
                  --     and associated with each of the affected items.
                  --
                  --  4. Each new attribute instance is assigned the value of
                  --     the expression.
                  Create_Object (Instance, Value);
                  Instance.Objects (Get_Info (Value).Slot) :=
                    Unshare (Val, Instance_Pool);

                  Value := Get_Spec_Chain (Value);
               end loop;
            end;

         when Iir_Kind_Disconnection_Specification =>
            Elaborate_Disconnection_Specification (Instance, Decl);

         when Iir_Kind_Use_Clause =>
            null;

         when Iir_Kind_Signal_Attribute_Declaration =>
            declare
               Attr : Iir;
            begin
               Attr := Get_Signal_Attribute_Chain (Decl);
               while Is_Valid (Attr) loop
                  case Iir_Kinds_Signal_Attribute (Get_Kind (Attr)) is
                     when Iir_Kind_Delayed_Attribute =>
                        Elaborate_Delayed_Signal (Instance, Attr);
                     when Iir_Kind_Stable_Attribute =>
                        Elaborate_Implicit_Signal
                          (Instance, Attr, Mode_Stable);
                     when Iir_Kind_Quiet_Attribute =>
                        Elaborate_Implicit_Signal
                          (Instance, Attr, Mode_Quiet);
                     when Iir_Kind_Transaction_Attribute =>
                        Elaborate_Implicit_Signal
                          (Instance, Attr, Mode_Transaction);
                  end case;
                  Attr := Get_Attr_Chain (Attr);
               end loop;
            end;

         when Iir_Kind_Non_Object_Alias_Declaration =>
            null;
         when Iir_Kind_Group_Template_Declaration
           | Iir_Kind_Group_Declaration =>
            null;
         when Iir_Kind_Protected_Type_Body =>
            null;

         when Iir_Kind_Package_Declaration
           | Iir_Kind_Package_Instantiation_Declaration =>
            Elaborate_Package_Declaration (Instance, Decl);
         when Iir_Kind_Package_Body =>
            Elaborate_Package_Body (Instance, Decl);

         when Iir_Kind_Nature_Declaration =>
            Elaborate_Nature_Definition (Instance, Get_Nature (Decl));
            Create_Terminal (Instance, Get_Chain (Decl));

         when Iir_Kind_Terminal_Declaration =>
            Create_Terminal (Instance, Decl);

         when Iir_Kinds_Branch_Quantity_Declaration =>
            Elaborate_Branch_Quantity_Declaration (Instance, Decl);

         when Iir_Kind_Psl_Default_Clock =>
            null;

         when others =>
            Error_Kind ("elaborate_declaration", Decl);
      end case;

      Release (Expr_Mark, Expr_Pool);
   end Elaborate_Declaration;

   procedure Destroy_Iterator_Declaration
     (Instance : Block_Instance_Acc; Decl : Iir)
   is
      Obj_Type : constant Iir := Get_Type (Decl);
      Constraint : Iir;
      Cons_Info : Sim_Info_Acc;
   begin
      if Get_Kind (Decl) /= Iir_Kind_Iterator_Declaration then
         raise Internal_Error;
      end if;
      Destroy_Object (Instance, Decl);

      if Get_Kind (Obj_Type) = Iir_Kind_Range_Array_Attribute
        or else not Is_Anonymous_Type_Definition (Obj_Type)
      then
         return;
      end if;

      Constraint := Get_Range_Constraint (Obj_Type);
      if Get_Kind (Constraint) /= Iir_Kind_Range_Expression then
         return;
      end if;
      Cons_Info := Get_Info (Constraint);
      if Cons_Info.Obj_Scope = Instance.Block_Scope
        and then Cons_Info.Slot = Instance.Elab_Objects
      then
         Destroy_Object (Instance, Constraint);
      end if;
   end Destroy_Iterator_Declaration;

   procedure Finalize_Declarative_Part
     (Instance : Block_Instance_Acc; Decl_Chain : Iir)
   is
      Decl : Iir;
      Val : Iir_Value_Literal_Acc;
   begin
      Decl := Decl_Chain;
      while Decl /= Null_Iir loop
         case Get_Kind (Decl) is
            when Iir_Kind_File_Declaration =>
               --  LRM93 3.4.1
               --  An implicit call to FILE_CLOSE exists in a subprogram body
               --  for every file object declared in the corresponding
               --  subprogram declarative part.
               --  Each such call associates a unique file object with the
               --  formal parameter F and is called whenever the corresponding
               --  subprogram completes its execution.
               Val := Instance.Objects (Get_Info (Decl).Slot);
               if Get_Text_File_Flag (Get_Type (Decl)) then
                  File_Operation.File_Close_Text (Val, Null_Iir);
                  File_Operation.File_Destroy_Text (Val);
               else
                  File_Operation.File_Close_Binary (Val, Null_Iir);
                  File_Operation.File_Destroy_Binary (Val);
               end if;
            when others =>
               null;
         end case;
         Decl := Get_Chain (Decl);
      end loop;
   end Finalize_Declarative_Part;

   --  LRM93 12.3  Elaboration of a Declarative Part
   procedure Elaborate_Declarative_Part
     (Instance : Block_Instance_Acc; Decl_Chain : Iir)
   is
      Decl : Iir;
   begin
      --  The elaboration of a declarative part consists of the elaboration
      --  of the declarative items, if any, in the order in which they are
      --  given in the declarative part.
      --  [Exception for 'foreign ]
      Decl := Decl_Chain;
      while Decl /= Null_Iir loop
         --  In certain cases, the elaboration of a declarative item involves
         --  the evaluation of expressions that appear within the declarative
         --  item.
         --  The value of any object denoted by a primary in such an expression
         --  must be defined at the time the primary is read.
         --  In addition, if a primary in such an expression is a function call
         --  then the value of any object denoted or appearing as part of an
         --  actual designator in the function call must be defined at the
         --  time the expression is evaluated.
         --  FIXME: check this.
         Elaborate_Declaration (Instance, Decl);
         Decl := Get_Chain (Decl);
      end loop;
   end Elaborate_Declarative_Part;

   function Elaborate_Architecture (Arch : Iir_Architecture_Body;
                                    Conf : Iir_Block_Configuration;
                                    Parent_Instance : Block_Instance_Acc;
                                    Stmt : Iir;
                                    Generic_Map : Iir;
                                    Port_Map : Iir)
     return Block_Instance_Acc
   is
      Entity : constant Iir_Entity_Declaration := Get_Entity (Arch);
      Instance : Block_Instance_Acc;
      Expr_Mark : Mark_Type;
   begin
      Mark (Expr_Mark, Expr_Pool);

      if Trace_Elaboration then
         Simple_IO.Put ("elaborating ");
         Simple_IO.Put (Image_Identifier (Arch));
         Simple_IO.Put (" of ");
         Simple_IO.Put_Line (Image_Identifier (Entity));
      end if;

      Instance := Create_Block_Instance (Parent_Instance, Arch, Stmt);
      Instance.Block_Scope := Get_Info (Entity);
      Instance.Up_Block := Global_Instances;

      --  LRM93 12.1
      --  Elaboration of a block statement involves first elaborating each not
      --  yet elaborated package containing declarations referenced by the
      --  block.
      Elaborate_Dependence (Get_Design_Unit (Arch));

      Elaborate_Generic_Clause (Instance, Get_Generic_Chain (Entity));
      Elaborate_Generic_Map_Aspect (Instance, Parent_Instance,
                                    Get_Generic_Chain (Entity), Generic_Map);
      Elaborate_Port_Clause (Instance, Get_Port_Chain (Entity));
      Elaborate_Port_Map_Aspect (Instance, Parent_Instance,
                                 Get_Port_Chain (Entity), Port_Map);

      Elaborate_Declarative_Part
        (Instance, Get_Declaration_Chain (Entity));
      Elaborate_Declarative_Part (Instance, Get_Declaration_Chain (Arch));
      Elaborate_Concurrent_Statement_Part
        (Instance, Get_Concurrent_Statement_Chain (Entity));
      Elaborate_Concurrent_Statement_Part
        (Instance, Get_Concurrent_Statement_Chain (Arch));

      --  Configure the unit.  This will create sub units.
      Elaborate_Block_Configuration (Conf, Instance);

      Release (Expr_Mark, Expr_Pool);

      return Instance;
   end Elaborate_Architecture;

   function Override_Generic (Formal : Iir; Str : String) return Iir
   is
      use Vhdl.Evaluation;
      Formal_Type : constant Iir := Get_Type (Formal);
      Formal_Btype : constant Iir := Get_Base_Type (Formal_Type);
      Res : Iir;
   begin
      case Get_Kind (Formal_Btype) is
         when Iir_Kind_Integer_Type_Definition
           | Iir_Kind_Enumeration_Type_Definition =>
            Res := Eval_Value_Attribute (Str, Formal_Type, Formal);
            if not Eval_Is_In_Bound (Res, Formal_Type) then
               Error_Msg_Elab ("override for %n is out of bounds", +Formal);
               return Null_Iir;
            end if;
            return Res;
         when Iir_Kind_Array_Type_Definition =>
            if Is_One_Dimensional_Array_Type (Formal_Btype) then
               declare
                  use Str_Table;
                  Str8 : String8_Id;
                  Ntype : Iir;
               begin
                  Str8 := Create_String8;
                  Append_String8_String (Str);
                  Res := Create_Iir (Iir_Kind_String_Literal8);
                  Set_String8_Id (Res, Str8);
                  --  FIXME: check characters are in the type.
                  Set_String_Length (Res, Str'Length);
                  Set_Expr_Staticness (Res, Locally);
                  Ntype := Create_Unidim_Array_By_Length
                    (Get_Base_Type (Formal_Type), Str'Length, Res);
                  Set_Type (Res, Ntype);
                  Set_Literal_Subtype (Res, Ntype);
                  return Res;
               end;
            end if;
         when others =>
            null;
      end case;
      Error_Msg_Elab ("unhandled override for %n", +Formal);
      return Null_Iir;
   end Override_Generic;

   procedure Override_Generics
     (Map : in out Iir; First : Grt.Options.Generic_Override_Acc)
   is
      use Grt.Options;
      Over : Generic_Override_Acc;
      Id : Name_Id;
      Gen : Iir;
      Prev : Iir;
      Val : Iir;
      Assoc : Iir;
   begin
      Over := First;
      Prev := Null_Iir;
      while Over /= null loop
         Id := Name_Table.Get_Identifier (Over.Name.all);

         --  Find existing association in map.  There should be one association
         --  for each generic.
         Gen := Map;
         while Gen /= Null_Iir loop
            exit when Get_Identifier (Get_Formal (Map)) = Id;
            Prev := Gen;
            Gen := Get_Chain (Gen);
         end loop;

         if Gen = Null_Iir then
            Error_Msg_Elab ("no generic %n for -g", +Id);
         else
            --  Replace the association with one for the override value.
            Val := Override_Generic (Get_Formal (Map), Over.Value.all);
            if Val /= Null_Iir then
               Assoc :=
                 Create_Iir (Iir_Kind_Association_Element_By_Expression);
               Set_Actual (Assoc, Val);
               Set_Whole_Association_Flag (Assoc, True);
               Set_Formal (Assoc, Get_Formal (Map));

               Set_Chain (Assoc, Get_Chain (Gen));
               if Prev = Null_Iir then
                  Map := Assoc;
               else
                  Set_Chain (Prev, Assoc);
               end if;
            end if;
         end if;
         Over := Over.Next;
      end loop;
   end Override_Generics;

   procedure Check_No_Unconstrained (Ports : Iir; Map : Iir)
   is
      Assoc : Iir;
      Port : Iir;
      Formal : Iir;
   begin
      Assoc := Map;
      Port := Ports;
      while Assoc /= Null_Iir loop
         if Get_Kind (Assoc) = Iir_Kind_Association_Element_Open then
            Formal := Get_Association_Interface (Assoc, Port);
            if Get_Default_Value (Formal) = Null_Iir
              and then not Is_Fully_Constrained_Type (Get_Type (Formal))
            then
               Error_Msg_Elab
                 (Formal, "top-level %n must have a value", +Formal);
            end if;
         end if;
         Next_Association_Interface (Assoc, Port);
      end loop;
   end Check_No_Unconstrained;

   -- Elaborate a design.
   procedure Elaborate_Design (Design: Iir_Design_Unit)
   is
      Unit : constant Iir := Get_Library_Unit (Design);
      Conf_Unit : Iir_Design_Unit;
      Conf : Iir_Block_Configuration;
      Arch_Unit : Iir_Design_Unit;
      Arch : Iir_Architecture_Body;
      Entity : Iir_Entity_Declaration;
      Generic_Map : Iir;
      Port_Map : Iir;
   begin
      Global_Instances :=
        new Block_Instance_Type'(Max_Objs => Global_Info.Nbr_Objects,
                                 In_Wait_Flag => False,
                                 Id => 0,
                                 Block_Scope => Global_Info,
                                 Uninst_Scope => null,
                                 Up_Block => null,
                                 Label => Null_Iir,
                                 Bod => Null_Iir,
                                 Stmt => Null_Iir,
                                 Parent => null,
                                 Children => null,
                                 Brother => null,
                                 Marker => Empty_Marker,
                                 Actuals_Ref => null,
                                 Result => null,
                                 Elab_Objects => 0,
                                 Objects => (others => null));

      --  Use a 'fake' process to execute code during elaboration.
      Current_Process := No_Process;

      Instance_Pool := Global_Pool'Access;

      pragma Assert (Is_Empty (Expr_Pool));

      --  Find architecture and configuration for the top unit
      case Get_Kind (Unit) is
         when Iir_Kind_Architecture_Body =>
            Arch := Unit;
            Conf_Unit := Get_Default_Configuration_Declaration (Unit);
         when Iir_Kind_Configuration_Declaration =>
            Conf_Unit := Design;
            Arch := Get_Named_Entity
              (Get_Block_Specification (Get_Block_Configuration (Unit)));
            Elaborate_Dependence (Design);
            Elaborate_Configuration_Declaration (Unit);
         when others =>
            Error_Kind ("elaborate_design", Unit);
      end case;

      Arch_Unit := Get_Design_Unit (Arch);
      Entity := Get_Entity (Arch);

      pragma Assert (Is_Empty (Expr_Pool));

      Elaborate_Dependence (Arch_Unit);

      --  Sanity check: memory area for expressions must be empty.
      pragma Assert (Is_Empty (Expr_Pool));

      --  Use default values for top entity generics and ports.
      Generic_Map := Create_Default_Association
        (Get_Generic_Chain (Entity), Null_Iir, Entity);
      Port_Map := Create_Default_Association
        (Get_Port_Chain (Entity), Null_Iir, Entity);
      Override_Generics (Generic_Map, Grt.Options.First_Generic_Override);

      Check_No_Unconstrained (Get_Generic_Chain (Entity), Generic_Map);
      Check_No_Unconstrained (Get_Port_Chain (Entity), Port_Map);

      --  Stop now in case of errors.
      if Nbr_Errors /= 0 then
         Grt.Errors.Fatal_Error;
      end if;

      --  Elaborate from the top configuration.
      Conf := Get_Block_Configuration (Get_Library_Unit (Conf_Unit));
      Top_Instance := Elaborate_Architecture
        (Arch, Conf, null, Arch, Generic_Map, Port_Map);

      Current_Process := null;

      Instance_Pool := null;

      --  Stop now in case of errors.
      if Nbr_Errors /= 0 then
         Grt.Errors.Fatal_Error;
      end if;

      --  Sanity check: memory area for expressions must be empty.
      pragma Assert (Is_Empty (Expr_Pool));
   end Elaborate_Design;

end Simul.Elaboration;
