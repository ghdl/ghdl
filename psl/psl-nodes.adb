--  This is in fact -*- Ada -*-
with Ada.Unchecked_Conversion;
with GNAT.Table;
with PSL.Errors;
with PSL.Hash;

package body PSL.Nodes is
   --  Suppress the access check of the table base.  This is really safe to
   --  suppress this check because the table base cannot be null.
   pragma Suppress (Access_Check);

   --  Suppress the index check on the table.
   --  Could be done during non-debug, since this may catch errors (reading
   --  Null_Node.
   --pragma Suppress (Index_Check);

   type Format_Type is
     (
      Format_Short,
      Format_Medium
     );

   pragma Unreferenced (Format_Type, Format_Short, Format_Medium);

   -- Common fields are:
   --   Flag1 : Boolean
   --   Flag2 : Boolean
   --   Flag3 : Boolean
   --   Flag4 : Boolean
   --   Flag5 : Boolean
   --   Flag6 : Boolean
   --   Nkind : Kind_Type
   --   State1 : Bit2_Type
   --   State2 : Bit2_Type
   --   Location : Int32
   --   Field1 : Int32
   --   Field2 : Int32
   --   Field3 : Int32
   --   Field4 : Int32

   -- Fields of Format_Short:
   --   Field5 : Int32
   --   Field6 : Int32

   -- Fields of Format_Medium:
   --   Odigit1 : Bit3_Type
   --   Odigit2 : Bit3_Type
   --   State3 : Bit2_Type
   --   State4 : Bit2_Type
   --   Field5 : Int32
   --   Field6 : Int32
   --   Field7 : Int32 (location)
   --   Field8 : Int32 (field1)
   --   Field9 : Int32 (field2)
   --   Field10 : Int32 (field3)
   --   Field11 : Int32 (field4)
   --   Field12 : Int32 (field5)

   type State_Type is range 0 .. 3;
   type Bit3_Type is range 0 .. 7;

   type Node_Record is record
      Kind : Nkind;
      Flag1 : Boolean;
      Flag2 : Boolean;
      Flag3 : Boolean;
      Flag4 : Boolean;
      Flag5 : Boolean;
      Flag6 : Boolean;
      Flag7 : Boolean;
      Flag8 : Boolean;
      Flag9 : Boolean;
      Flag10 : Boolean;
      Flag11 : Boolean;
      Flag12 : Boolean;
      Flag13 : Boolean;
      Flag14 : Boolean;
      Flag15 : Boolean;
      Flag16 : Boolean;
      State1 : State_Type;
      B3_1 : Bit3_Type;
      Flag17 : Boolean;
      Flag18 : Boolean;
      Flag19 : Boolean;

      Location : Int32;
      Field1 : Int32;
      Field2 : Int32;
      Field3 : Int32;
      Field4 : Int32;
      Field5 : Int32;
      Field6 : Int32;
   end record;
   pragma Pack (Node_Record);
   for Node_Record'Size use 8 * 32;

   package Nodet is new GNAT.Table
     (Table_Component_Type => Node_Record,
      Table_Index_Type => Node,
      Table_Low_Bound => 1,
      Table_Initial => 1024,
      Table_Increment => 100);

   Init_Node : constant Node_Record := (Kind => N_Error,
                                        Flag1 => False,
                                        Flag2 => False,
                                        State1 => 0,
                                        B3_1 => 0,
                                        Location => 0,
                                        Field1 => 0,
                                        Field2 => 0,
                                        Field3 => 0,
                                        Field4 => 0,
                                        Field5 => 0,
                                        Field6 => 0,
                                        others => False);

   Free_Nodes : Node := Null_Node;


   function Get_Last_Node return Node is
   begin
      return Nodet.Last;
   end Get_Last_Node;

   function Int32_To_Uns32 is new Ada.Unchecked_Conversion
     (Source => Int32, Target => Uns32);

   function Uns32_To_Int32 is new Ada.Unchecked_Conversion
     (Source => Uns32, Target => Int32);

   function Int32_To_NFA is new Ada.Unchecked_Conversion
     (Source => Int32, Target => NFA);

   function NFA_To_Int32 is new Ada.Unchecked_Conversion
     (Source => NFA, Target => Int32);

   procedure Set_Kind (N : Node; K : Nkind) is
   begin
      Nodet.Table (N).Kind := K;
   end Set_Kind;

   function Get_Kind (N : Node) return Nkind is
   begin
      return Nodet.Table (N).Kind;
   end Get_Kind;


   procedure Set_Flag1 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag1 := Flag;
   end Set_Flag1;

   function Get_Flag1 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag1;
   end Get_Flag1;

   procedure Set_Flag2 (N : Node; Flag : Boolean) is
   begin
      Nodet.Table (N).Flag2 := Flag;
   end Set_Flag2;

   function Get_Flag2 (N : Node) return Boolean is
   begin
      return Nodet.Table (N).Flag2;
   end Get_Flag2;


   procedure Set_State1 (N : Node; S : State_Type) is
   begin
      Nodet.Table (N).State1 := S;
   end Set_State1;

   function Get_State1 (N : Node) return State_Type is
   begin
      return Nodet.Table (N).State1;
   end Get_State1;


   function Get_Location (N : Node) return Location_Type is
   begin
      return Location_Type (Nodet.Table (N).Location);
   end Get_Location;

   procedure Set_Location (N : Node; Loc : Location_Type) is
   begin
      Nodet.Table (N).Location := Int32 (Loc);
   end Set_Location;


   procedure Set_Field1 (N : Node; V : Int32) is
   begin
      Nodet.Table (N).Field1 := V;
   end Set_Field1;

   function Get_Field1 (N : Node) return Int32 is
   begin
      return Nodet.Table (N).Field1;
   end Get_Field1;


   procedure Set_Field2 (N : Node; V : Int32) is
   begin
      Nodet.Table (N).Field2 := V;
   end Set_Field2;

   function Get_Field2 (N : Node) return Int32 is
   begin
      return Nodet.Table (N).Field2;
   end Get_Field2;


   function Get_Field3 (N : Node) return Int32 is
   begin
      return Nodet.Table (N).Field3;
   end Get_Field3;

   procedure Set_Field3 (N : Node; V : Int32) is
   begin
      Nodet.Table (N).Field3 := V;
   end Set_Field3;


   function Get_Field4 (N : Node) return Int32 is
   begin
      return Nodet.Table (N).Field4;
   end Get_Field4;

   procedure Set_Field4 (N : Node; V : Int32) is
   begin
      Nodet.Table (N).Field4 := V;
   end Set_Field4;


   function Get_Field5 (N : Node) return Int32 is
   begin
      return Nodet.Table (N).Field5;
   end Get_Field5;

   procedure Set_Field5 (N : Node; V : Int32) is
   begin
      Nodet.Table (N).Field5 := V;
   end Set_Field5;


   function Get_Field6 (N : Node) return Int32 is
   begin
      return Nodet.Table (N).Field6;
   end Get_Field6;

   procedure Set_Field6 (N : Node; V : Int32) is
   begin
      Nodet.Table (N).Field6 := V;
   end Set_Field6;

   procedure Set_Field7 (N : Node; V : Int32) is
   begin
      Nodet.Table (N + 1).Field1 := V;
   end Set_Field7;

   function Get_Field7 (N : Node) return Int32 is
   begin
      return Nodet.Table (N + 1).Field1;
   end Get_Field7;


   function Create_Node (Kind : Nkind) return Node
   is
      Res : Node;
   begin
      if Free_Nodes /= Null_Node then
         Res := Free_Nodes;
         Free_Nodes := Node (Get_Field1 (Res));
      else
         Nodet.Increment_Last;
         Res := Nodet.Last;
      end if;
      Nodet.Table (Res) := Init_Node;
      Set_Kind (Res, Kind);
      return Res;
   end Create_Node;

   procedure Free_Node (N : Node)
   is
   begin
      Set_Kind (N, N_Error);
      Set_Field1 (N, Int32 (Free_Nodes));
      Free_Nodes := N;
   end Free_Node;

   procedure Failed (Msg : String; N : Node)
   is
   begin
      Errors.Error_Kind (Msg, N);
   end Failed;

   procedure Init is
   begin
      Nodet.Init;
      if Create_Node (N_False) /= False_Node then
         raise Internal_Error;
      end if;
      if Create_Node (N_True) /= True_Node then
         raise Internal_Error;
      end if;
      if Create_Node (N_Number) /= One_Node then
         raise Internal_Error;
      end if;
      Set_Value (One_Node, 1);
      if Create_Node (N_EOS) /= EOS_Node then
         raise Internal_Error;
      end if;
      Set_Hash (EOS_Node, 0);
      PSL.Hash.Init;
   end Init;

   function Get_Psl_Type (N : Node) return PSL_Types is
   begin
      case Get_Kind (N) is
         when N_And_Prop
           | N_Or_Prop
           | N_Log_Imp_Prop
           | N_Always
           | N_Never
           | N_Eventually
           | N_Next
           | N_Next_E
           | N_Next_A
           | N_Next_Event
           | N_Next_Event_A
           | N_Next_Event_E
           | N_Before
           | N_Until
           | N_Abort
           | N_Strong
           | N_Property_Parameter
           | N_Property_Instance =>
            return Type_Property;
         when N_Braced_SERE
           | N_Concat_SERE
           | N_Fusion_SERE
           | N_Within_SERE
           | N_Overlap_Imp_Seq
           | N_Imp_Seq
           | N_And_Seq
           | N_Or_Seq
           | N_Match_And_Seq
           | N_Star_Repeat_Seq
           | N_Goto_Repeat_Seq
           | N_Equal_Repeat_Seq
           | N_Plus_Repeat_Seq
           | N_Clock_Event
           | N_Sequence_Instance
           | N_Endpoint_Instance
           | N_Sequence_Parameter =>
            return Type_Sequence;
         when N_Name =>
            return Get_Psl_Type (Get_Decl (N));
         when N_HDL_Expr =>
            --  FIXME.
            return Type_Boolean;
         when N_Or_Bool
           | N_And_Bool
           | N_Not_Bool
           | N_Imp_Bool
           | N_False
           | N_True
           | N_Boolean_Parameter =>
            return Type_Boolean;
         when N_Number
           | N_Const_Parameter =>
            return Type_Numeric;
         when N_Vmode
           | N_Vunit
           | N_Vprop
           | N_Hdl_Mod_Name
           | N_Assert_Directive
           | N_Sequence_Declaration
           | N_Endpoint_Declaration
           | N_Property_Declaration
           | N_Actual
           | N_Name_Decl
           | N_Error
           | N_EOS =>
            PSL.Errors.Error_Kind ("get_psl_type", N);
      end case;
   end Get_Psl_Type;

   procedure Reference_Failed (Msg : String; N : Node) is
   begin
      Failed (Msg, N);
   end Reference_Failed;
   pragma Unreferenced (Reference_Failed);

   pragma Unreferenced (Set_Field7, Get_Field7);
   --  Subprograms.
   procedure Check_Kind_For_Identifier (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Vmode
           | N_Vunit
           | N_Vprop
           | N_Hdl_Mod_Name
           | N_Property_Declaration
           | N_Sequence_Declaration
           | N_Endpoint_Declaration
           | N_Const_Parameter
           | N_Boolean_Parameter
           | N_Property_Parameter
           | N_Sequence_Parameter
           | N_Name
           | N_Name_Decl =>
            null;
         when others =>
            Failed ("Get/Set_Identifier", N);
      end case;
   end Check_Kind_For_Identifier;

   function Get_Identifier (N : Node) return Name_Id is
   begin
      Check_Kind_For_Identifier (N);
      return Name_Id (Get_Field1 (N));
   end Get_Identifier;

   procedure Set_Identifier (N : Node; Id : Name_Id) is
   begin
      Check_Kind_For_Identifier (N);
      Set_Field1 (N, Int32 (Id));
   end Set_Identifier;

   procedure Check_Kind_For_Chain (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Vmode
           | N_Vunit
           | N_Vprop
           | N_Assert_Directive
           | N_Property_Declaration
           | N_Sequence_Declaration
           | N_Endpoint_Declaration
           | N_Const_Parameter
           | N_Boolean_Parameter
           | N_Property_Parameter
           | N_Sequence_Parameter
           | N_Actual
           | N_Name_Decl =>
            null;
         when others =>
            Failed ("Get/Set_Chain", N);
      end case;
   end Check_Kind_For_Chain;

   function Get_Chain (N : Node) return Node is
   begin
      Check_Kind_For_Chain (N);
      return Node (Get_Field2 (N));
   end Get_Chain;

   procedure Set_Chain (N : Node; Chain : Node) is
   begin
      Check_Kind_For_Chain (N);
      Set_Field2 (N, Int32 (Chain));
   end Set_Chain;

   procedure Check_Kind_For_Instance (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Vmode
           | N_Vunit
           | N_Vprop =>
            null;
         when others =>
            Failed ("Get/Set_Instance", N);
      end case;
   end Check_Kind_For_Instance;

   function Get_Instance (N : Node) return Node is
   begin
      Check_Kind_For_Instance (N);
      return Node (Get_Field3 (N));
   end Get_Instance;

   procedure Set_Instance (N : Node; Instance : Node) is
   begin
      Check_Kind_For_Instance (N);
      Set_Field3 (N, Int32 (Instance));
   end Set_Instance;

   procedure Check_Kind_For_Item_Chain (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Vmode
           | N_Vunit
           | N_Vprop =>
            null;
         when others =>
            Failed ("Get/Set_Item_Chain", N);
      end case;
   end Check_Kind_For_Item_Chain;

   function Get_Item_Chain (N : Node) return Node is
   begin
      Check_Kind_For_Item_Chain (N);
      return Node (Get_Field4 (N));
   end Get_Item_Chain;

   procedure Set_Item_Chain (N : Node; Item : Node) is
   begin
      Check_Kind_For_Item_Chain (N);
      Set_Field4 (N, Int32 (Item));
   end Set_Item_Chain;

   procedure Check_Kind_For_Prefix (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Hdl_Mod_Name =>
            null;
         when others =>
            Failed ("Get/Set_Prefix", N);
      end case;
   end Check_Kind_For_Prefix;

   function Get_Prefix (N : Node) return Node is
   begin
      Check_Kind_For_Prefix (N);
      return Node (Get_Field2 (N));
   end Get_Prefix;

   procedure Set_Prefix (N : Node; Prefix : Node) is
   begin
      Check_Kind_For_Prefix (N);
      Set_Field2 (N, Int32 (Prefix));
   end Set_Prefix;

   procedure Check_Kind_For_Label (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Assert_Directive =>
            null;
         when others =>
            Failed ("Get/Set_Label", N);
      end case;
   end Check_Kind_For_Label;

   function Get_Label (N : Node) return Name_Id is
   begin
      Check_Kind_For_Label (N);
      return Name_Id (Get_Field1 (N));
   end Get_Label;

   procedure Set_Label (N : Node; Id : Name_Id) is
   begin
      Check_Kind_For_Label (N);
      Set_Field1 (N, Int32 (Id));
   end Set_Label;

   procedure Check_Kind_For_String (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Assert_Directive =>
            null;
         when others =>
            Failed ("Get/Set_String", N);
      end case;
   end Check_Kind_For_String;

   function Get_String (N : Node) return Node is
   begin
      Check_Kind_For_String (N);
      return Node (Get_Field3 (N));
   end Get_String;

   procedure Set_String (N : Node; Str : Node) is
   begin
      Check_Kind_For_String (N);
      Set_Field3 (N, Int32 (Str));
   end Set_String;

   procedure Check_Kind_For_Property (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Assert_Directive
           | N_Property_Declaration
           | N_Clock_Event
           | N_Always
           | N_Never
           | N_Eventually
           | N_Strong
           | N_Imp_Seq
           | N_Overlap_Imp_Seq
           | N_Next
           | N_Next_A
           | N_Next_E
           | N_Next_Event
           | N_Next_Event_A
           | N_Next_Event_E
           | N_Abort =>
            null;
         when others =>
            Failed ("Get/Set_Property", N);
      end case;
   end Check_Kind_For_Property;

   function Get_Property (N : Node) return Node is
   begin
      Check_Kind_For_Property (N);
      return Node (Get_Field4 (N));
   end Get_Property;

   procedure Set_Property (N : Node; Property : Node) is
   begin
      Check_Kind_For_Property (N);
      Set_Field4 (N, Int32 (Property));
   end Set_Property;

   procedure Check_Kind_For_NFA (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Assert_Directive =>
            null;
         when others =>
            Failed ("Get/Set_NFA", N);
      end case;
   end Check_Kind_For_NFA;

   function Get_NFA (N : Node) return NFA is
   begin
      Check_Kind_For_NFA (N);
      return Int32_To_NFA (Get_Field5 (N));
   end Get_NFA;

   procedure Set_NFA (N : Node; P : NFA) is
   begin
      Check_Kind_For_NFA (N);
      Set_Field5 (N, NFA_To_Int32 (P));
   end Set_NFA;

   procedure Check_Kind_For_Global_Clock (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Property_Declaration =>
            null;
         when others =>
            Failed ("Get/Set_Global_Clock", N);
      end case;
   end Check_Kind_For_Global_Clock;

   function Get_Global_Clock (N : Node) return Node is
   begin
      Check_Kind_For_Global_Clock (N);
      return Node (Get_Field3 (N));
   end Get_Global_Clock;

   procedure Set_Global_Clock (N : Node; Clock : Node) is
   begin
      Check_Kind_For_Global_Clock (N);
      Set_Field3 (N, Int32 (Clock));
   end Set_Global_Clock;

   procedure Check_Kind_For_Parameter_List (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Property_Declaration
           | N_Sequence_Declaration
           | N_Endpoint_Declaration =>
            null;
         when others =>
            Failed ("Get/Set_Parameter_List", N);
      end case;
   end Check_Kind_For_Parameter_List;

   function Get_Parameter_List (N : Node) return Node is
   begin
      Check_Kind_For_Parameter_List (N);
      return Node (Get_Field5 (N));
   end Get_Parameter_List;

   procedure Set_Parameter_List (N : Node; E : Node) is
   begin
      Check_Kind_For_Parameter_List (N);
      Set_Field5 (N, Int32 (E));
   end Set_Parameter_List;

   procedure Check_Kind_For_Sequence (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Sequence_Declaration
           | N_Endpoint_Declaration
           | N_Imp_Seq
           | N_Overlap_Imp_Seq
           | N_Star_Repeat_Seq
           | N_Goto_Repeat_Seq
           | N_Plus_Repeat_Seq
           | N_Equal_Repeat_Seq =>
            null;
         when others =>
            Failed ("Get/Set_Sequence", N);
      end case;
   end Check_Kind_For_Sequence;

   function Get_Sequence (N : Node) return Node is
   begin
      Check_Kind_For_Sequence (N);
      return Node (Get_Field3 (N));
   end Get_Sequence;

   procedure Set_Sequence (N : Node; S : Node) is
   begin
      Check_Kind_For_Sequence (N);
      Set_Field3 (N, Int32 (S));
   end Set_Sequence;

   procedure Check_Kind_For_Actual (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Const_Parameter
           | N_Boolean_Parameter
           | N_Property_Parameter
           | N_Sequence_Parameter
           | N_Actual =>
            null;
         when others =>
            Failed ("Get/Set_Actual", N);
      end case;
   end Check_Kind_For_Actual;

   function Get_Actual (N : Node) return Node is
   begin
      Check_Kind_For_Actual (N);
      return Node (Get_Field3 (N));
   end Get_Actual;

   procedure Set_Actual (N : Node; E : Node) is
   begin
      Check_Kind_For_Actual (N);
      Set_Field3 (N, Int32 (E));
   end Set_Actual;

   procedure Check_Kind_For_Declaration (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Sequence_Instance
           | N_Endpoint_Instance
           | N_Property_Instance =>
            null;
         when others =>
            Failed ("Get/Set_Declaration", N);
      end case;
   end Check_Kind_For_Declaration;

   function Get_Declaration (N : Node) return Node is
   begin
      Check_Kind_For_Declaration (N);
      return Node (Get_Field1 (N));
   end Get_Declaration;

   procedure Set_Declaration (N : Node; Decl : Node) is
   begin
      Check_Kind_For_Declaration (N);
      Set_Field1 (N, Int32 (Decl));
   end Set_Declaration;

   procedure Check_Kind_For_Association_Chain (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Sequence_Instance
           | N_Endpoint_Instance
           | N_Property_Instance =>
            null;
         when others =>
            Failed ("Get/Set_Association_Chain", N);
      end case;
   end Check_Kind_For_Association_Chain;

   function Get_Association_Chain (N : Node) return Node is
   begin
      Check_Kind_For_Association_Chain (N);
      return Node (Get_Field2 (N));
   end Get_Association_Chain;

   procedure Set_Association_Chain (N : Node; Chain : Node) is
   begin
      Check_Kind_For_Association_Chain (N);
      Set_Field2 (N, Int32 (Chain));
   end Set_Association_Chain;

   procedure Check_Kind_For_Formal (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Actual =>
            null;
         when others =>
            Failed ("Get/Set_Formal", N);
      end case;
   end Check_Kind_For_Formal;

   function Get_Formal (N : Node) return Node is
   begin
      Check_Kind_For_Formal (N);
      return Node (Get_Field4 (N));
   end Get_Formal;

   procedure Set_Formal (N : Node; E : Node) is
   begin
      Check_Kind_For_Formal (N);
      Set_Field4 (N, Int32 (E));
   end Set_Formal;

   procedure Check_Kind_For_Boolean (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Clock_Event
           | N_Next_Event
           | N_Next_Event_A
           | N_Next_Event_E
           | N_Abort
           | N_Not_Bool =>
            null;
         when others =>
            Failed ("Get/Set_Boolean", N);
      end case;
   end Check_Kind_For_Boolean;

   function Get_Boolean (N : Node) return Node is
   begin
      Check_Kind_For_Boolean (N);
      return Node (Get_Field3 (N));
   end Get_Boolean;

   procedure Set_Boolean (N : Node; B : Node) is
   begin
      Check_Kind_For_Boolean (N);
      Set_Field3 (N, Int32 (B));
   end Set_Boolean;

   procedure Check_Kind_For_Strong_Flag (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Next
           | N_Next_A
           | N_Next_E
           | N_Next_Event
           | N_Next_Event_A
           | N_Next_Event_E
           | N_Until
           | N_Before =>
            null;
         when others =>
            Failed ("Get/Set_Strong_Flag", N);
      end case;
   end Check_Kind_For_Strong_Flag;

   function Get_Strong_Flag (N : Node) return Boolean is
   begin
      Check_Kind_For_Strong_Flag (N);
      return Get_Flag1 (N);
   end Get_Strong_Flag;

   procedure Set_Strong_Flag (N : Node; B : Boolean) is
   begin
      Check_Kind_For_Strong_Flag (N);
      Set_Flag1 (N, B);
   end Set_Strong_Flag;

   procedure Check_Kind_For_Number (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Next
           | N_Next_Event =>
            null;
         when others =>
            Failed ("Get/Set_Number", N);
      end case;
   end Check_Kind_For_Number;

   function Get_Number (N : Node) return Node is
   begin
      Check_Kind_For_Number (N);
      return Node (Get_Field1 (N));
   end Get_Number;

   procedure Set_Number (N : Node; S : Node) is
   begin
      Check_Kind_For_Number (N);
      Set_Field1 (N, Int32 (S));
   end Set_Number;

   procedure Check_Kind_For_Decl (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Name =>
            null;
         when others =>
            Failed ("Get/Set_Decl", N);
      end case;
   end Check_Kind_For_Decl;

   function Get_Decl (N : Node) return Node is
   begin
      Check_Kind_For_Decl (N);
      return Node (Get_Field2 (N));
   end Get_Decl;

   procedure Set_Decl (N : Node; D : Node) is
   begin
      Check_Kind_For_Decl (N);
      Set_Field2 (N, Int32 (D));
   end Set_Decl;

   procedure Check_Kind_For_Value (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Number =>
            null;
         when others =>
            Failed ("Get/Set_Value", N);
      end case;
   end Check_Kind_For_Value;

   function Get_Value (N : Node) return Uns32 is
   begin
      Check_Kind_For_Value (N);
      return Int32_To_Uns32 (Get_Field1 (N));
   end Get_Value;

   procedure Set_Value (N : Node; Val : Uns32) is
   begin
      Check_Kind_For_Value (N);
      Set_Field1 (N, Uns32_To_Int32 (Val));
   end Set_Value;

   procedure Check_Kind_For_SERE (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Braced_SERE =>
            null;
         when others =>
            Failed ("Get/Set_SERE", N);
      end case;
   end Check_Kind_For_SERE;

   function Get_SERE (N : Node) return Node is
   begin
      Check_Kind_For_SERE (N);
      return Node (Get_Field1 (N));
   end Get_SERE;

   procedure Set_SERE (N : Node; S : Node) is
   begin
      Check_Kind_For_SERE (N);
      Set_Field1 (N, Int32 (S));
   end Set_SERE;

   procedure Check_Kind_For_Left (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Log_Imp_Prop
           | N_Until
           | N_Before
           | N_Or_Prop
           | N_And_Prop
           | N_Concat_SERE
           | N_Fusion_SERE
           | N_Within_SERE
           | N_Match_And_Seq
           | N_And_Seq
           | N_Or_Seq
           | N_And_Bool
           | N_Or_Bool
           | N_Imp_Bool =>
            null;
         when others =>
            Failed ("Get/Set_Left", N);
      end case;
   end Check_Kind_For_Left;

   function Get_Left (N : Node) return Node is
   begin
      Check_Kind_For_Left (N);
      return Node (Get_Field1 (N));
   end Get_Left;

   procedure Set_Left (N : Node; S : Node) is
   begin
      Check_Kind_For_Left (N);
      Set_Field1 (N, Int32 (S));
   end Set_Left;

   procedure Check_Kind_For_Right (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Log_Imp_Prop
           | N_Until
           | N_Before
           | N_Or_Prop
           | N_And_Prop
           | N_Concat_SERE
           | N_Fusion_SERE
           | N_Within_SERE
           | N_Match_And_Seq
           | N_And_Seq
           | N_Or_Seq
           | N_And_Bool
           | N_Or_Bool
           | N_Imp_Bool =>
            null;
         when others =>
            Failed ("Get/Set_Right", N);
      end case;
   end Check_Kind_For_Right;

   function Get_Right (N : Node) return Node is
   begin
      Check_Kind_For_Right (N);
      return Node (Get_Field2 (N));
   end Get_Right;

   procedure Set_Right (N : Node; S : Node) is
   begin
      Check_Kind_For_Right (N);
      Set_Field2 (N, Int32 (S));
   end Set_Right;

   procedure Check_Kind_For_Low_Bound (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Next_A
           | N_Next_E
           | N_Next_Event_A
           | N_Next_Event_E
           | N_Star_Repeat_Seq
           | N_Goto_Repeat_Seq
           | N_Equal_Repeat_Seq =>
            null;
         when others =>
            Failed ("Get/Set_Low_Bound", N);
      end case;
   end Check_Kind_For_Low_Bound;

   function Get_Low_Bound (N : Node) return Node is
   begin
      Check_Kind_For_Low_Bound (N);
      return Node (Get_Field1 (N));
   end Get_Low_Bound;

   procedure Set_Low_Bound (N : Node; S : Node) is
   begin
      Check_Kind_For_Low_Bound (N);
      Set_Field1 (N, Int32 (S));
   end Set_Low_Bound;

   procedure Check_Kind_For_High_Bound (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Next_A
           | N_Next_E
           | N_Next_Event_A
           | N_Next_Event_E
           | N_Star_Repeat_Seq
           | N_Goto_Repeat_Seq
           | N_Equal_Repeat_Seq =>
            null;
         when others =>
            Failed ("Get/Set_High_Bound", N);
      end case;
   end Check_Kind_For_High_Bound;

   function Get_High_Bound (N : Node) return Node is
   begin
      Check_Kind_For_High_Bound (N);
      return Node (Get_Field2 (N));
   end Get_High_Bound;

   procedure Set_High_Bound (N : Node; S : Node) is
   begin
      Check_Kind_For_High_Bound (N);
      Set_Field2 (N, Int32 (S));
   end Set_High_Bound;

   procedure Check_Kind_For_Inclusive_Flag (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Until
           | N_Before =>
            null;
         when others =>
            Failed ("Get/Set_Inclusive_Flag", N);
      end case;
   end Check_Kind_For_Inclusive_Flag;

   function Get_Inclusive_Flag (N : Node) return Boolean is
   begin
      Check_Kind_For_Inclusive_Flag (N);
      return Get_Flag2 (N);
   end Get_Inclusive_Flag;

   procedure Set_Inclusive_Flag (N : Node; B : Boolean) is
   begin
      Check_Kind_For_Inclusive_Flag (N);
      Set_Flag2 (N, B);
   end Set_Inclusive_Flag;

   procedure Check_Kind_For_Presence (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Not_Bool
           | N_And_Bool
           | N_Or_Bool
           | N_Imp_Bool
           | N_HDL_Expr =>
            null;
         when others =>
            Failed ("Get/Set_Presence", N);
      end case;
   end Check_Kind_For_Presence;

   function Get_Presence (N : Node) return PSL_Presence_Kind is
   begin
      Check_Kind_For_Presence (N);
      return PSL_Presence_Kind'Val(Get_State1 (N));
   end Get_Presence;

   procedure Set_Presence (N : Node; P : PSL_Presence_Kind) is
   begin
      Check_Kind_For_Presence (N);
      Set_State1 (N, PSL_Presence_Kind'pos (P));
   end Set_Presence;

   procedure Check_Kind_For_HDL_Node (N : Node) is
   begin
      case Get_Kind (N) is
         when N_HDL_Expr =>
            null;
         when others =>
            Failed ("Get/Set_HDL_Node", N);
      end case;
   end Check_Kind_For_HDL_Node;

   function Get_HDL_Node (N : Node) return HDL_Node is
   begin
      Check_Kind_For_HDL_Node (N);
      return Get_Field1 (N);
   end Get_HDL_Node;

   procedure Set_HDL_Node (N : Node; H : HDL_Node) is
   begin
      Check_Kind_For_HDL_Node (N);
      Set_Field1 (N, H);
   end Set_HDL_Node;

   procedure Check_Kind_For_HDL_Index (N : Node) is
   begin
      case Get_Kind (N) is
         when N_HDL_Expr
           | N_EOS =>
            null;
         when others =>
            Failed ("Get/Set_HDL_Index", N);
      end case;
   end Check_Kind_For_HDL_Index;

   function Get_HDL_Index (N : Node) return Int32 is
   begin
      Check_Kind_For_HDL_Index (N);
      return Get_Field2 (N);
   end Get_HDL_Index;

   procedure Set_HDL_Index (N : Node; Idx : Int32) is
   begin
      Check_Kind_For_HDL_Index (N);
      Set_Field2 (N, Idx);
   end Set_HDL_Index;

   procedure Check_Kind_For_Hash (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Not_Bool
           | N_And_Bool
           | N_Or_Bool
           | N_Imp_Bool
           | N_HDL_Expr
           | N_EOS =>
            null;
         when others =>
            Failed ("Get/Set_Hash", N);
      end case;
   end Check_Kind_For_Hash;

   function Get_Hash (N : Node) return Uns32 is
   begin
      Check_Kind_For_Hash (N);
      return Int32_To_Uns32 (Get_Field5 (N));
   end Get_Hash;

   procedure Set_Hash (N : Node; E : Uns32) is
   begin
      Check_Kind_For_Hash (N);
      Set_Field5 (N, Uns32_To_Int32 (E));
   end Set_Hash;

   procedure Check_Kind_For_Hash_Link (N : Node) is
   begin
      case Get_Kind (N) is
         when N_Not_Bool
           | N_And_Bool
           | N_Or_Bool
           | N_Imp_Bool
           | N_HDL_Expr
           | N_EOS =>
            null;
         when others =>
            Failed ("Get/Set_Hash_Link", N);
      end case;
   end Check_Kind_For_Hash_Link;

   function Get_Hash_Link (N : Node) return Node is
   begin
      Check_Kind_For_Hash_Link (N);
      return Node (Get_Field6 (N));
   end Get_Hash_Link;

   procedure Set_Hash_Link (N : Node; E : Node) is
   begin
      Check_Kind_For_Hash_Link (N);
      Set_Field6 (N, Int32 (E));
   end Set_Hash_Link;


end PSL.Nodes;

