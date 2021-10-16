--  Iir to ortho translator.
--  Copyright (C) 2002-2014 Tristan Gingold
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

with Name_Table; -- use Name_Table;
with Vhdl.Nodes_Priv;
with Tables;
with Trans_Decls; use Trans_Decls;

package body Trans is
   use Trans.Helpers;

   package body Subprgs is
      procedure Clear_Subprg_Instance (Prev : out Subprg_Instance_Stack) is
      begin
         Prev := Current_Subprg_Instance;
         Current_Subprg_Instance := Null_Subprg_Instance_Stack;
      end Clear_Subprg_Instance;

      procedure Push_Subprg_Instance (Scope    : Var_Scope_Acc;
                                      Ptr_Type : O_Tnode;
                                      Ident    : O_Ident;
                                      Prev     : out Subprg_Instance_Stack)
      is
      begin
         Prev := Current_Subprg_Instance;
         Current_Subprg_Instance := (Scope => Scope,
                                     Ptr_Type => Ptr_Type,
                                     Ident => Ident);
      end Push_Subprg_Instance;

      function Has_Current_Subprg_Instance return Boolean is
      begin
         return Current_Subprg_Instance.Ptr_Type /= O_Tnode_Null;
      end Has_Current_Subprg_Instance;

      procedure Pop_Subprg_Instance (Ident : O_Ident;
                                     Prev  : Subprg_Instance_Stack)
      is
      begin
         if Is_Equal (Current_Subprg_Instance.Ident, Ident) then
            Current_Subprg_Instance := Prev;
         else
            --  POP does not match with a push.
            raise Internal_Error;
         end if;
      end Pop_Subprg_Instance;

      procedure Add_Subprg_Instance_Interfaces
        (Interfaces : in out O_Inter_List; Vars : out Subprg_Instance_Type)
      is
      begin
         if Has_Current_Subprg_Instance then
            Vars.Scope := Current_Subprg_Instance.Scope;
            Vars.Inter_Type := Current_Subprg_Instance.Ptr_Type;
            New_Interface_Decl
              (Interfaces, Vars.Inter,
               Current_Subprg_Instance.Ident,
               Current_Subprg_Instance.Ptr_Type);
         else
            Vars := Null_Subprg_Instance;
         end if;
      end Add_Subprg_Instance_Interfaces;

      procedure Add_Subprg_Instance_Field
        (Field : out O_Fnode; Prev_Scope : out Var_Scope_Acc) is
      begin
         if Has_Current_Subprg_Instance then
            Field := Add_Instance_Factory_Field
              (Current_Subprg_Instance.Ident,
               Current_Subprg_Instance.Ptr_Type);
            Prev_Scope := Current_Subprg_Instance.Scope;
         else
            Field := O_Fnode_Null;
            Prev_Scope := null;
         end if;
      end Add_Subprg_Instance_Field;

      function Has_Subprg_Instance (Vars : Subprg_Instance_Type)
                                    return Boolean is
      begin
         return Vars.Inter /= O_Dnode_Null;
      end Has_Subprg_Instance;

      function Get_Subprg_Instance (Vars : Subprg_Instance_Type)
                                    return O_Enode is
      begin
         pragma Assert (Has_Subprg_Instance (Vars));
         return New_Address (Get_Instance_Ref (Vars.Scope.all),
                             Vars.Inter_Type);
      end Get_Subprg_Instance;

      procedure Add_Subprg_Instance_Assoc
        (Assocs : in out O_Assoc_List; Vars : Subprg_Instance_Type) is
      begin
         if Has_Subprg_Instance (Vars) then
            New_Association (Assocs, Get_Subprg_Instance (Vars));
         end if;
      end Add_Subprg_Instance_Assoc;

      procedure Set_Subprg_Instance_Field
        (Var : O_Dnode; Field : O_Fnode; Vars : Subprg_Instance_Type)
      is
      begin
         if Has_Subprg_Instance (Vars) and then Field /= O_Fnode_Null then
            New_Assign_Stmt (New_Selected_Acc_Value (New_Obj (Var), Field),
                             New_Obj_Value (Vars.Inter));
         end if;
      end Set_Subprg_Instance_Field;

      procedure Start_Subprg_Instance_Use (Vars : Subprg_Instance_Type) is
      begin
         if Has_Subprg_Instance (Vars) then
            Set_Scope_Via_Param_Ptr (Vars.Scope.all, Vars.Inter);
         end if;
      end Start_Subprg_Instance_Use;

      procedure Finish_Subprg_Instance_Use (Vars : Subprg_Instance_Type) is
      begin
         if Has_Subprg_Instance (Vars) then
            Clear_Scope (Vars.Scope.all);
         end if;
      end Finish_Subprg_Instance_Use;

      procedure Start_Prev_Subprg_Instance_Use_Via_Field
        (Prev_Scope : Var_Scope_Acc; Field : O_Fnode) is
      begin
         if Field /= O_Fnode_Null then
            Set_Scope_Via_Field_Ptr (Prev_Scope.all, Field,
                                     Current_Subprg_Instance.Scope);
         end if;
      end Start_Prev_Subprg_Instance_Use_Via_Field;

      procedure Finish_Prev_Subprg_Instance_Use_Via_Field
        (Prev_Scope : Var_Scope_Acc; Field : O_Fnode) is
      begin
         if Field /= O_Fnode_Null then
            Clear_Scope (Prev_Scope.all);
         end if;
      end Finish_Prev_Subprg_Instance_Use_Via_Field;

      procedure Create_Subprg_Instance (Interfaces : in out O_Inter_List;
                                        Subprg     : Iir)
      is
      begin
         Add_Subprg_Instance_Interfaces
           (Interfaces, Get_Info (Subprg).Subprg_Instance);
      end Create_Subprg_Instance;

      procedure Start_Subprg_Instance_Use (Subprg : Iir) is
      begin
         Start_Subprg_Instance_Use (Get_Info (Subprg).Subprg_Instance);
      end Start_Subprg_Instance_Use;

      procedure Finish_Subprg_Instance_Use (Subprg : Iir) is
      begin
         Finish_Subprg_Instance_Use (Get_Info (Subprg).Subprg_Instance);
      end Finish_Subprg_Instance_Use;

      function Instantiate_Subprg_Instance (Inst : Subprg_Instance_Type)
                                            return Subprg_Instance_Type is
      begin
         return Subprg_Instance_Type'
           (Inter => Inst.Inter,
            Inter_Type => Inst.Inter_Type,
            Scope => Instantiated_Var_Scope (Inst.Scope));
      end Instantiate_Subprg_Instance;
   end Subprgs;

   package body Chap10 is
      --  Identifiers.
      --  The following functions are helpers to create ortho identifiers.
      Identifier_Buffer : String (1 .. 4096);
      Identifier_Len    : Natural := 0;
      Identifier_Start  : Natural := 1;

      --  Per scope unique id.
      Identifier_Local  : Local_Identifier_Type := 0;


      Inst_Build : Inst_Build_Acc := null;
      procedure Unchecked_Deallocation is new Ada.Unchecked_Deallocation
        (Object => Inst_Build_Type, Name => Inst_Build_Acc);

      procedure Set_Global_Storage (Storage : O_Storage) is
      begin
         Global_Storage := Storage;
      end Set_Global_Storage;

      procedure Pop_Build_Instance
      is
         Old : Inst_Build_Acc;
      begin
         Old := Inst_Build;
         pragma Assert (Old.Prev_Id_Start <= Identifier_Start);
         Identifier_Start := Old.Prev_Id_Start;
         Inst_Build := Old.Prev;
         Unchecked_Deallocation (Old);
      end Pop_Build_Instance;

      function Get_Scope_Type (Scope : Var_Scope_Type) return O_Tnode is
      begin
         pragma Assert (Scope.Scope_Type /= O_Tnode_Null);
         return Scope.Scope_Type;
      end Get_Scope_Type;

      function Get_Scope_Size (Scope : Var_Scope_Type) return O_Cnode is
      begin
         pragma Assert (Scope.Scope_Type /= O_Tnode_Null);
         return New_Sizeof (Scope.Scope_Type, Ghdl_Index_Type);
      end Get_Scope_Size;

      function Has_Scope_Type (Scope : Var_Scope_Type) return Boolean is
      begin
         return Scope.Scope_Type /= O_Tnode_Null;
      end Has_Scope_Type;

      procedure Predeclare_Scope_Type
        (Scope : in out Var_Scope_Type; Name : O_Ident) is
      begin
         pragma Assert (Scope.Scope_Type = O_Tnode_Null);
         New_Uncomplete_Record_Type (Scope.Scope_Type);
         New_Type_Decl (Name, Scope.Scope_Type);
      end Predeclare_Scope_Type;

      procedure Declare_Scope_Acc
        (Scope : Var_Scope_Type; Name : O_Ident; Ptr_Type : out O_Tnode) is
      begin
         Ptr_Type := New_Access_Type (Get_Scope_Type (Scope));
         New_Type_Decl (Name, Ptr_Type);
      end Declare_Scope_Acc;

      --  Common routine for instance and frame.
      procedure Start_Instance_Factory (Inst : Inst_Build_Acc) is
      begin
         Inst.Prev := Inst_Build;
         Inst.Prev_Id_Start := Identifier_Start;

         Identifier_Start := Identifier_Len + 1;

         if Inst.Scope.Scope_Type /= O_Tnode_Null then
            Start_Uncomplete_Record_Type
              (Inst.Scope.Scope_Type, Inst.Elements);
         else
            Start_Record_Type (Inst.Elements);
         end if;
         Inst_Build := Inst;
      end Start_Instance_Factory;

      procedure Push_Instance_Factory (Scope : Var_Scope_Acc)
      is
         Inst : Inst_Build_Acc;
      begin
         Inst := new Inst_Build_Type (Instance);
         Inst.Scope := Scope;

         Start_Instance_Factory (Inst);
      end Push_Instance_Factory;

      procedure Push_Frame_Factory (Scope : Var_Scope_Acc;
                                    Persistant : Boolean)
      is
         Inst : Inst_Build_Acc;
      begin
         if Persistant then
            Inst := new Inst_Build_Type (Persistant_Frame);
         else
            Inst := new Inst_Build_Type (Stack_Frame);
         end if;
         Inst.Scope := Scope;

         Start_Instance_Factory (Inst);
      end Push_Frame_Factory;

      function Add_Instance_Factory_Field (Name : O_Ident; Ftype : O_Tnode)
                                           return O_Fnode
      is
         Res : O_Fnode;
      begin
         New_Record_Field (Inst_Build.Elements, Res, Name, Ftype);
         return Res;
      end Add_Instance_Factory_Field;

      procedure Add_Scope_Field
        (Name : O_Ident; Child : in out Var_Scope_Type)
      is
         Field : O_Fnode;
      begin
         Field := Add_Instance_Factory_Field (Name, Get_Scope_Type (Child));
         Set_Scope_Via_Field (Child, Field, Inst_Build.Scope);
      end Add_Scope_Field;

      function Get_Scope_Offset (Child : Var_Scope_Type; Otype : O_Tnode)
                                 return O_Cnode is
      begin
         return New_Offsetof (Get_Scope_Type (Child.Up_Link.all),
                              Child.Field, Otype);
      end Get_Scope_Offset;

      procedure Finish_Instance_Factory (Scope : in Var_Scope_Acc)
      is
         Res : O_Tnode;
      begin
         Finish_Record_Type (Inst_Build.Elements, Res);
         Pop_Build_Instance;
         Scope.Scope_Type := Res;
      end Finish_Instance_Factory;

      procedure Pop_Instance_Factory (Scope : in Var_Scope_Acc) is
      begin
         --  Not matching.
         pragma Assert (Inst_Build.Kind = Instance);

         Finish_Instance_Factory (Scope);
      end Pop_Instance_Factory;

      procedure Pop_Frame_Factory (Scope : in Var_Scope_Acc) is
      begin
         --  Not matching.
         pragma Assert (Inst_Build.Kind in Stack_Frame .. Persistant_Frame);

         Finish_Instance_Factory (Scope);
      end Pop_Frame_Factory;

      procedure Push_Local_Factory
      is
         Inst : Inst_Build_Acc;
      begin
         if Inst_Build /= null
           and then (Inst_Build.Kind /= Global and Inst_Build.Kind /= Local)
         then
            --  Cannot create a local factory on an instance.
            raise Internal_Error;
         end if;
         Inst := new Inst_Build_Type (Kind => Local);
         Inst.Prev := Inst_Build;
         Inst.Prev_Global_Storage := Global_Storage;

         Inst.Prev_Id_Start := Identifier_Start;
         Identifier_Start := Identifier_Len + 1;

         Inst_Build := Inst;
         case Global_Storage is
            when O_Storage_Public =>
               Global_Storage := O_Storage_Private;
            when O_Storage_Private
               | O_Storage_External =>
               null;
            when O_Storage_Local =>
               raise Internal_Error;
         end case;
      end Push_Local_Factory;

      --  Return TRUE is the current scope is local.
      function Is_Local_Scope return Boolean is
      begin
         if Inst_Build = null then
            return False;
         end if;
         case Inst_Build.Kind is
            when Local
              | Instance
              | Stack_Frame
              | Persistant_Frame =>
               return True;
            when Global =>
               return False;
         end case;
      end Is_Local_Scope;

      procedure Pop_Local_Factory is
      begin
         if Inst_Build.Kind /= Local then
            --  Not matching.
            raise Internal_Error;
         end if;
         Global_Storage := Inst_Build.Prev_Global_Storage;
         Pop_Build_Instance;
      end Pop_Local_Factory;

      procedure Create_Union_Scope
        (Scope : out Var_Scope_Type; Stype : O_Tnode) is
      begin
         pragma Assert (Scope.Scope_Type = O_Tnode_Null);
         pragma Assert (Scope.Kind = Var_Scope_None);
         Scope.Scope_Type := Stype;
      end Create_Union_Scope;

      procedure Set_Scope_Via_Field
        (Scope       : in out Var_Scope_Type;
         Scope_Field : O_Fnode; Scope_Parent : Var_Scope_Acc) is
      begin
         pragma Assert (Scope.Kind = Var_Scope_None);
         Scope := (Scope_Type => Scope.Scope_Type,
                   Kind => Var_Scope_Field,
                   Field => Scope_Field, Up_Link => Scope_Parent);
      end Set_Scope_Via_Field;

      procedure Set_Scope_Via_Field_Ptr
        (Scope       : in out Var_Scope_Type;
         Scope_Field : O_Fnode; Scope_Parent : Var_Scope_Acc) is
      begin
         pragma Assert (Scope.Kind = Var_Scope_None);
         Scope := (Scope_Type => Scope.Scope_Type,
                   Kind => Var_Scope_Field_Ptr,
                   Field => Scope_Field, Up_Link => Scope_Parent);
      end Set_Scope_Via_Field_Ptr;

      procedure Set_Scope_Via_Var_Ptr
        (Scope : in out Var_Scope_Type; Var : Var_Type) is
      begin
         pragma Assert (Scope.Kind = Var_Scope_None);
         pragma Assert (Var.Kind = Var_Scope);
         Scope := (Scope_Type => Scope.Scope_Type,
                   Kind => Var_Scope_Field_Ptr,
                   Field => Var.I_Field, Up_Link => Var.I_Scope);
      end Set_Scope_Via_Var_Ptr;

      procedure Set_Scope_Via_Param_Ptr
        (Scope : in out Var_Scope_Type; Scope_Param : O_Dnode) is
      begin
         pragma Assert (Scope.Kind = Var_Scope_None);
         Scope := (Scope_Type => Scope.Scope_Type,
                   Kind => Var_Scope_Ptr, D => Scope_Param);
      end Set_Scope_Via_Param_Ptr;

      procedure Set_Scope_Via_Decl
        (Scope : in out Var_Scope_Type; Decl : O_Dnode) is
      begin
         pragma Assert (Scope.Kind = Var_Scope_None);
         Scope := (Scope_Type => Scope.Scope_Type,
                   Kind => Var_Scope_Decl, D => Decl);
      end Set_Scope_Via_Decl;

      procedure Set_Scope_Via_Var
        (Scope : in out Var_Scope_Type; Var : Var_Type) is
      begin
         pragma Assert (Scope.Kind = Var_Scope_None);
         case Var.Kind is
            when Var_Scope =>
               Scope := (Scope_Type => Scope.Scope_Type,
                         Kind => Var_Scope_Field,
                         Field => Var.I_Field,
                         Up_Link => Var.I_Scope);
            when Var_Global
              | Var_Local =>
               Scope := (Scope_Type => Scope.Scope_Type,
                         Kind => Var_Scope_Decl,
                         D => Var.E);
            when Var_None =>
               raise Internal_Error;
         end case;
      end Set_Scope_Via_Var;

      procedure Clear_Scope (Scope : in out Var_Scope_Type) is
      begin
         pragma Assert (Scope.Kind /= Var_Scope_None);
         Scope := (Scope_Type => Scope.Scope_Type, Kind => Var_Scope_None);
      end Clear_Scope;

      function Is_Null (Scope : Var_Scope_Type) return Boolean is
      begin
         return Scope.Kind = Var_Scope_None;
      end Is_Null;

      function Create_Global_Var
        (Name : O_Ident; Vtype : O_Tnode; Storage : O_Storage)
         return Var_Type
      is
         Var : O_Dnode;
      begin
         New_Var_Decl (Var, Name, Storage, Vtype);
         return Var_Type'(Kind => Var_Global, E => Var);
      end Create_Global_Var;

      function Create_Global_Const
        (Name          : O_Ident;
         Vtype         : O_Tnode;
         Storage       : O_Storage;
         Initial_Value : O_Cnode)
         return Var_Type
      is
         Res : O_Dnode;
      begin
         New_Const_Decl (Res, Name, Storage, Vtype);
         if Storage /= O_Storage_External
           and then Initial_Value /= O_Cnode_Null
         then
            Start_Init_Value (Res);
            Finish_Init_Value (Res, Initial_Value);
         end if;
         return Var_Type'(Kind => Var_Global, E => Res);
      end Create_Global_Const;

      procedure Define_Global_Const (Const : in out Var_Type; Val : O_Cnode) is
      begin
         Start_Init_Value (Const.E);
         Finish_Init_Value (Const.E, Val);
      end Define_Global_Const;

      function Create_Var
        (Name    : Var_Ident_Type;
         Vtype   : O_Tnode;
         Storage : O_Storage := Global_Storage)
         return Var_Type
      is
         Res   : O_Dnode;
         Field : O_Fnode;
         K     : Inst_Build_Kind_Type;
      begin
         if Inst_Build = null then
            K := Global;
         else
            K := Inst_Build.Kind;
         end if;
         case K is
            when Global =>
               --  The global scope is in use...
               return Create_Global_Var (Name.Id, Vtype, Storage);
            when Local =>
               --  It is always possible to create a variable in a local scope.
               --  Create a var.
               New_Var_Decl (Res, Name.Id, O_Storage_Local, Vtype);
               return Var_Type'(Kind => Var_Local, E => Res);
            when Instance | Stack_Frame | Persistant_Frame =>
               --  Create a field.
               New_Record_Field (Inst_Build.Elements, Field, Name.Id, Vtype);
               return Var_Type'(Kind => Var_Scope, I_Build_Kind => K,
                                I_Field => Field, I_Scope => Inst_Build.Scope);
         end case;
      end Create_Var;

      --  Get a reference to scope STYPE. If IS_PTR is set, RES is an access
      --  to the scope, otherwise RES directly designates the scope.
      procedure Find_Scope (Scope  : Var_Scope_Type;
                            Res    : out O_Lnode;
                            Is_Ptr : out Boolean) is
      begin
         case Scope.Kind is
            when Var_Scope_None =>
               raise Internal_Error;
            when Var_Scope_Ptr
               | Var_Scope_Decl =>
               Res := New_Obj (Scope.D);
               Is_Ptr := Scope.Kind = Var_Scope_Ptr;
            when Var_Scope_Field
               | Var_Scope_Field_Ptr =>
               declare
                  Parent     : O_Lnode;
                  Parent_Ptr : Boolean;
               begin
                  Find_Scope (Scope.Up_Link.all, Parent, Parent_Ptr);
                  if Parent_Ptr then
                     Parent := New_Acc_Value (Parent);
                  end if;
                  Res := New_Selected_Element (Parent, Scope.Field);
                  Is_Ptr := Scope.Kind = Var_Scope_Field_Ptr;
               end;
         end case;
      end Find_Scope;

      procedure Check_Not_Building is
      begin
         --  Variables cannot be referenced if there is an instance being
         --  built.
         if Inst_Build /= null and then Inst_Build.Kind = Instance then
            raise Internal_Error;
         end if;
      end Check_Not_Building;

      function Get_Instance_Access (Block : Iir) return O_Enode
      is
         Info   : constant Block_Info_Acc := Get_Info (Block);
         Res    : O_Lnode;
         Is_Ptr : Boolean;
      begin
         Check_Not_Building;
         Find_Scope (Info.Block_Scope, Res, Is_Ptr);
         if Is_Ptr then
            return New_Value (Res);
         else
            return New_Address (Res, Info.Block_Decls_Ptr_Type);
         end if;
      end Get_Instance_Access;

      function Get_Instance_Ref (Scope : Var_Scope_Type) return O_Lnode
      is
         Res    : O_Lnode;
         Is_Ptr : Boolean;
      begin
         Check_Not_Building;
         Find_Scope (Scope, Res, Is_Ptr);
         if Is_Ptr then
            return New_Acc_Value (Res);
         else
            return Res;
         end if;
      end Get_Instance_Ref;

      function Get_Var (Var : Var_Type) return O_Lnode
      is
      begin
         case Var.Kind is
            when Var_None =>
               raise Internal_Error;
            when Var_Local
               | Var_Global =>
               return New_Obj (Var.E);
            when Var_Scope =>
               return New_Selected_Element
                 (Get_Instance_Ref (Var.I_Scope.all), Var.I_Field);
         end case;
      end Get_Var;

      function Get_Alloc_Kind_For_Var (Var : Var_Type)
                                       return Allocation_Kind is
      begin
         case Var.Kind is
            when Var_Local =>
               return Alloc_Stack;
            when Var_Global =>
               return Alloc_System;
            when Var_Scope =>
               case Var.I_Build_Kind is
                  when Stack_Frame =>
                     return Alloc_Stack;
                  when Persistant_Frame =>
                     return Alloc_Return;
                  when Instance =>
                     return Alloc_System;
                  when others =>
                     raise Internal_Error;
               end case;
            when Var_None =>
               raise Internal_Error;
         end case;
      end Get_Alloc_Kind_For_Var;

      function Is_Var_Stable (Var : Var_Type) return Boolean is
      begin
         case Var.Kind is
            when Var_Local
               | Var_Global =>
               return True;
            when Var_Scope =>
               return False;
            when Var_None =>
               raise Internal_Error;
         end case;
      end Is_Var_Stable;

      function Is_Var_Field (Var : Var_Type) return Boolean is
      begin
         case Var.Kind is
            when Var_Local
               | Var_Global =>
               return False;
            when Var_Scope =>
               return True;
            when Var_None =>
               raise Internal_Error;
         end case;
      end Is_Var_Field;

      function Get_Var_Offset (Var : Var_Type; Otype : O_Tnode) return O_Cnode
      is
      begin
         return New_Offsetof (Get_Scope_Type (Var.I_Scope.all),
                              Var.I_Field, Otype);
      end Get_Var_Offset;

      function Get_Var_Label (Var : Var_Type) return O_Dnode is
      begin
         case Var.Kind is
            when Var_Local
               | Var_Global =>
               return Var.E;
            when Var_Scope
               | Var_None =>
               raise Internal_Error;
         end case;
      end Get_Var_Label;

      procedure Save_Local_Identifier (Id : out Local_Identifier_Type) is
      begin
         Id := Identifier_Local;
      end Save_Local_Identifier;

      procedure Restore_Local_Identifier (Id : Local_Identifier_Type) is
      begin
         --  If the value is restored with a smaller value, some identifiers
         --  will be reused.  This is certainly an internal error.
         pragma Assert (Identifier_Local <= Id);
         Identifier_Local := Id;
      end Restore_Local_Identifier;

      --  Reset the identifier.
      procedure Reset_Identifier_Prefix is
      begin
         pragma Assert (Identifier_Len = 0 and Identifier_Local = 0);
         null;
      end Reset_Identifier_Prefix;

      procedure Pop_Identifier_Prefix (Mark : in Id_Mark_Type) is
      begin
         pragma Assert (Mark.Len <= Identifier_Len);
         Identifier_Len := Mark.Len;
         Identifier_Local := Mark.Local_Id;
      end Pop_Identifier_Prefix;

      procedure Add_String (Len : in out Natural; Str : String) is
      begin
         Identifier_Buffer (Len + 1 .. Len + Str'Length) := Str;
         Len := Len + Str'Length;
      end Add_String;

      procedure Add_Nat (Len : in out Natural; Val : Natural)
      is
         Num : String (1 .. 10);
         V   : Natural;
         P   : Natural;
      begin
         P := Num'Last;
         V := Val;
         loop
            Num (P) := Character'Val (Character'Pos ('0') + V mod 10);
            V := V / 10;
            exit when V = 0;
            P := P - 1;
         end loop;
         Add_String (Len, Num (P .. Num'Last));
      end Add_Nat;

      type Bool_Array_Type is array (Character) of Boolean;
      pragma Pack (Bool_Array_Type);
      Is_Extended_Char : constant Bool_Array_Type :=
        ('0' .. '9' | 'A' .. 'Z' | 'a' .. 'z' => False,
         others => True);

      --  Convert name_id NAME to a string stored to
      --  NAME_BUFFER (1 .. NAME_LENGTH).
      --
      --  This encodes extended identifiers.
      --
      --  Extended identifier encoding:
      --  They start with 'X'.
      --  Non extended character [0-9a-zA-Z] are left as is,
      --  others are encoded to _XX, where XX is the character position in hex.
      --  They finish with "__".
      function Name_Id_To_String (Name : Name_Id) return String  is
      begin
         if Name_Table.Is_Character (Name) then
            declare
               P : constant Natural :=
                 Character'Pos (Name_Table.Get_Character (Name));
               Res : String (1 .. 3);
            begin
               Res (1) := 'C';
               Res (2) := N2hex (P / 16);
               Res (3) := N2hex (P mod 16);
               return Res;
            end;
         else
            declare
               Img   : constant String := Name_Table.Image (Name);
               N_Len : Natural;
            begin
               if Img (Img'First) /= '\' then
                  return Img;
               end if;

               --  Extended identifier.

               --  Count number of characters in the extended string.
               N_Len := 3;
               for I in Img'First + 1 .. Img'Last - 1 loop
                  if Is_Extended_Char (Img (I)) then
                     N_Len := N_Len + 3;
                  else
                     N_Len := N_Len + 1;
                  end if;
               end loop;

               declare
                  Img2 : String (1 .. N_Len);
                  P     : Natural;
                  C     : Character;
               begin
                  --  Convert (without the trailing backslash).
                  Img2 (1) := 'X';
                  P := 1;
                  for I in Img'First + 1 .. Img'Last - 1 loop
                     C := Img (I);
                     if Is_Extended_Char (C) then
                        Img2 (P + 1) := '_';
                        Img2 (P + 2) := N2hex (Character'Pos (C) / 16);
                        Img2 (P + 3) := N2hex (Character'Pos (C) mod 16);
                        P := P + 3;
                     else
                        Img2 (P + 1) := C;
                        P := P + 1;
                     end if;
                  end loop;
                  Img2 (P + 1) := '_';
                  Img2 (P + 2) := '_';
                  pragma Assert (N_Len = P + 2);
                  return Img2;
               end;
            end;
         end if;
      end Name_Id_To_String;

      function Identifier_To_String (N : Iir) return String is
      begin
         return Name_Id_To_String (Get_Identifier (N));
      end Identifier_To_String;

      procedure Add_Name (Len : in out Natural; Name : Name_Id) is
      begin
         Add_String (Len, Name_Id_To_String (Name));
      end Add_Name;

      procedure Push_Identifier_Prefix (Mark : out Id_Mark_Type;
                                        Name : String;
                                        Val  : Iir_Int32 := 0)
      is
         P : Natural;
      begin
         Mark.Len := Identifier_Len;
         Mark.Local_Id := Identifier_Local;
         Identifier_Local := 0;
         P := Identifier_Len;
         Add_String (P, Name);
         if Val > 0 then
            Add_String (P, "O");
            Add_Nat (P, Natural (Val));
         end if;
         Add_String (P, "__");
         Identifier_Len := P;
      end Push_Identifier_Prefix;

      --  Add a suffix to the prefix (!!!).
      procedure Push_Identifier_Prefix
        (Mark : out Id_Mark_Type; Name : Name_Id; Val : Iir_Int32 := 0) is
      begin
         if Name = Null_Identifier then
            Push_Identifier_Prefix (Mark, "", Val);
         else
            Push_Identifier_Prefix (Mark, Name_Id_To_String (Name), Val);
         end if;
      end Push_Identifier_Prefix;

      procedure Push_Identifier_Prefix_Uniq (Mark : out Id_Mark_Type)
      is
         Str : String := Local_Identifier_Type'Image (Identifier_Local);
      begin
         --  Increment local identifier, as the value has been used.
         Identifier_Local := Identifier_Local + 1;
         Str (1) := 'U';
         Push_Identifier_Prefix (Mark, Str, 0);
      end Push_Identifier_Prefix_Uniq;

      procedure Add_Identifier (Len : in out Natural; Id : Name_Id) is
      begin
         if Id /= Null_Identifier then
            Add_Name (Len, Id);
         end if;
      end Add_Identifier;

      --  Create an identifier from IIR node ID without the prefix.
      function Create_Identifier_Without_Prefix (Id : Iir) return O_Ident is
      begin
         return Get_Identifier (Name_Id_To_String (Get_Identifier (Id)));
      end Create_Identifier_Without_Prefix;

      function Create_Identifier_Without_Prefix (Id : Name_Id; Str : String)
                                                 return O_Ident is
      begin
         if Str'Length = 0 then
            return Get_Identifier (Name_Id_To_String (Id));
         else
            return Get_Identifier (Name_Id_To_String (Id) & Str);
         end if;
      end Create_Identifier_Without_Prefix;

      function Create_Identifier_Without_Prefix
        (Id : Iir; Str : String) return O_Ident is
      begin
         return Create_Identifier_Without_Prefix (Get_Identifier (Id), Str);
      end Create_Identifier_Without_Prefix;

      --  Create an identifier from IIR node ID with prefix.
      function Create_Id (Id : Name_Id; Str : String; Is_Local : Boolean)
                          return O_Ident
      is
         L : Natural;
      begin
         L := Identifier_Len;
         Add_Identifier (L, Id);
         Add_String (L, Str);
         --Identifier_Buffer (L + Str'Length + 1) := Nul;
         if Is_Local then
            return Get_Identifier
              (Identifier_Buffer (Identifier_Start .. L));
         else
            return Get_Identifier (Identifier_Buffer (1 .. L));
         end if;
      end Create_Id;

      function Create_Identifier (Id : Name_Id; Str : String := "")
                                  return O_Ident
      is
      begin
         return Create_Id (Id, Str, False);
      end Create_Identifier;

      function Create_Identifier (Id : Iir; Str : String := "")
                                  return O_Ident
      is
      begin
         return Create_Id (Get_Identifier (Id), Str, False);
      end Create_Identifier;

      function Create_Identifier
        (Id : Iir; Val : Iir_Int32; Str : String := "")
         return O_Ident
      is
         Len : Natural;
      begin
         Len := Identifier_Len;
         Add_Identifier (Len, Get_Identifier (Id));

         if Val > 0 then
            Add_String (Len, "O");
            Add_Nat (Len, Natural (Val));
         end if;
         Add_String (Len, Str);
         return Get_Identifier (Identifier_Buffer (1 .. Len));
      end Create_Identifier;

      function Create_Identifier (Str : String)
                                  return O_Ident
      is
         Len : Natural;
      begin
         Len := Identifier_Len;
         Add_String (Len, Str);
         return Get_Identifier (Identifier_Buffer (1 .. Len));
      end Create_Identifier;

      function Create_Identifier return O_Ident
      is
      begin
         return Get_Identifier (Identifier_Buffer (1 .. Identifier_Len - 2));
      end Create_Identifier;

      function Create_Elab_Identifier (Kind : Elab_Kind) return O_Ident is
      begin
         case Kind is
            when Elab_Decls =>
               return Create_Identifier ("DECL_ELAB");
            when Elab_Stmts =>
               return Create_Identifier ("STMT_ELAB");
         end case;
      end Create_Elab_Identifier;

      function Create_Var_Identifier_From_Buffer (L : Natural)
                                                  return Var_Ident_Type
      is
         Start : Natural;
      begin
         if Is_Local_Scope then
            Start := Identifier_Start;
         else
            Start := 1;
         end if;
         return (Id => Get_Identifier (Identifier_Buffer (Start .. L)));
      end Create_Var_Identifier_From_Buffer;

      function Create_Var_Identifier (Id : Iir)
                                      return Var_Ident_Type
      is
         L : Natural := Identifier_Len;
      begin
         Add_Identifier (L, Get_Identifier (Id));
         return Create_Var_Identifier_From_Buffer (L);
      end Create_Var_Identifier;

      function Create_Var_Identifier (Id : String)
                                      return Var_Ident_Type
      is
         L : Natural := Identifier_Len;
      begin
         Add_String (L, Id);
         return Create_Var_Identifier_From_Buffer (L);
      end Create_Var_Identifier;

      function Create_Var_Identifier (Id : Iir; Str : String; Val : Natural)
                                      return Var_Ident_Type
      is
         L : Natural := Identifier_Len;
      begin
         Add_Identifier (L, Get_Identifier (Id));
         Add_String (L, Str);
         if Val > 0 then
            Add_String (L, "O");
            Add_Nat (L, Val);
         end if;
         return Create_Var_Identifier_From_Buffer (L);
      end Create_Var_Identifier;

      function Create_Uniq_Identifier return Var_Ident_Type
      is
         Res : Var_Ident_Type;
      begin
         Res.Id := Create_Uniq_Identifier;
         return Res;
      end Create_Uniq_Identifier;

      type Instantiate_Var_Stack;
      type Instantiate_Var_Stack_Acc is access Instantiate_Var_Stack;

      type Instantiate_Var_Stack is record
         Orig_Scope : Var_Scope_Acc;
         Inst_Scope : Var_Scope_Acc;
         Prev       : Instantiate_Var_Stack_Acc;
      end record;

      Top_Instantiate_Var_Stack  : Instantiate_Var_Stack_Acc := null;
      Free_Instantiate_Var_Stack : Instantiate_Var_Stack_Acc := null;

      procedure Push_Instantiate_Var_Scope
        (Inst_Scope : Var_Scope_Acc; Orig_Scope : Var_Scope_Acc)
      is
         Inst : Instantiate_Var_Stack_Acc;
      begin
         if Free_Instantiate_Var_Stack = null then
            Inst := new Instantiate_Var_Stack;
         else
            Inst := Free_Instantiate_Var_Stack;
            Free_Instantiate_Var_Stack := Inst.Prev;
         end if;
         Inst.all := (Orig_Scope => Orig_Scope,
                      Inst_Scope => Inst_Scope,
                      Prev => Top_Instantiate_Var_Stack);
         Top_Instantiate_Var_Stack := Inst;
      end Push_Instantiate_Var_Scope;

      procedure Pop_Instantiate_Var_Scope (Inst_Scope : Var_Scope_Acc)
      is
         Item : constant Instantiate_Var_Stack_Acc :=
           Top_Instantiate_Var_Stack;
      begin
         pragma Assert (Item /= null);
         pragma Assert (Item.Inst_Scope = Inst_Scope);
         Top_Instantiate_Var_Stack := Item.Prev;
         Item.all := (Orig_Scope => null,
                      Inst_Scope => null,
                      Prev => Free_Instantiate_Var_Stack);
         Free_Instantiate_Var_Stack := Item;
      end Pop_Instantiate_Var_Scope;

      function Instantiated_Var_Scope (Scope : Var_Scope_Acc)
                                       return Var_Scope_Acc
      is
         Item : Instantiate_Var_Stack_Acc;
      begin
         if Scope = null then
            return null;
         end if;

         Item := Top_Instantiate_Var_Stack;
         loop
            pragma Assert (Item /= null);
            if Item.Orig_Scope = Scope then
               return Item.Inst_Scope;
            end if;
            Item := Item.Prev;
         end loop;
      end Instantiated_Var_Scope;

      function Instantiate_Var (Var : Var_Type) return Var_Type is
      begin
         case Var.Kind is
            when Var_None
               | Var_Global
               | Var_Local =>
               return Var;
            when Var_Scope =>
               return Var_Type'
                 (Kind => Var_Scope,
                  I_Build_Kind => Var.I_Build_Kind,
                  I_Field => Var.I_Field,
                  I_Scope => Instantiated_Var_Scope (Var.I_Scope));
         end case;
      end Instantiate_Var;

      function Instantiate_Var_Scope (Scope : Var_Scope_Type)
                                      return Var_Scope_Type is
      begin
         case Scope.Kind is
            when Var_Scope_None
               | Var_Scope_Ptr
               | Var_Scope_Decl =>
               return Scope;
            when Var_Scope_Field =>
               return Var_Scope_Type'
                 (Kind => Var_Scope_Field,
                  Scope_Type => Scope.Scope_Type,
                  Field => Scope.Field,
                  Up_Link => Instantiated_Var_Scope (Scope.Up_Link));
            when Var_Scope_Field_Ptr =>
               return Var_Scope_Type'
                 (Kind => Var_Scope_Field_Ptr,
                  Scope_Type => Scope.Scope_Type,
                  Field => Scope.Field,
                  Up_Link => Instantiated_Var_Scope (Scope.Up_Link));
         end case;
      end Instantiate_Var_Scope;
   end Chap10;

   function Align_Val (Algn : Alignment_Type) return O_Cnode is
   begin
      case Algn is
         when Align_Undef =>
            raise Internal_Error;
         when Align_8 =>
            return Ghdl_Index_1;
         when Align_16 =>
            return Ghdl_Index_2;
         when Align_32 =>
            return Ghdl_Index_4;
         when Align_Ptr =>
            return Ghdl_Index_Ptr_Align;
         when Align_64 =>
            return Ghdl_Index_8;
      end case;
   end Align_Val;

   function Get_Object_Kind (M : Mnode) return Object_Kind_Type is
   begin
      return M.M1.K;
   end Get_Object_Kind;

   function Get_Var
     (Var : Var_Type; Vtype : Type_Info_Acc; Mode : Object_Kind_Type)
      return Mnode
   is
      L      : O_Lnode;
      D      : O_Dnode;
      Stable : Boolean;
   begin
      --  FIXME: there may be Vv2M and Vp2M.
      Stable := Is_Var_Stable (Var);
      if Stable then
         D := Get_Var_Label (Var);
      else
         L := Get_Var (Var);
      end if;
      case Vtype.Type_Mode is
         when Type_Mode_Scalar
           | Type_Mode_Acc
           | Type_Mode_File
           | Type_Mode_Unbounded_Array
           | Type_Mode_Unbounded_Record
           | Type_Mode_Bounds_Acc =>
            if Stable then
               return Dv2M (D, Vtype, Mode);
            else
               return Lv2M (L, Vtype, Mode);
            end if;
         when Type_Mode_Complex_Array
            | Type_Mode_Complex_Record
            | Type_Mode_Protected =>
            if Stable then
               return Dp2M (D, Vtype, Mode);
            else
               return Lp2M (L, Vtype, Mode);
            end if;
         when Type_Mode_Static_Array
            | Type_Mode_Static_Record =>
            if Stable then
               return Dv2M (D, Vtype, Mode);
            else
               return Lv2M (L, Vtype, Mode);
            end if;
         when Type_Mode_Unknown =>
            raise Internal_Error;
      end case;
   end Get_Var;

   function Get_Varp
     (Var : Var_Type; Vtype : Type_Info_Acc; Mode : Object_Kind_Type)
      return Mnode
   is
      Stable : Boolean;
   begin
      --  FIXME: there may be Vv2M and Vp2M.
      Stable := Is_Var_Stable (Var);
      if Stable then
         return Dp2M (Get_Var_Label (Var), Vtype, Mode);
      else
         return Lp2M (Get_Var (Var), Vtype, Mode);
      end if;
   end Get_Varp;

   function Stabilize (M : Mnode; Can_Copy : Boolean := False) return Mnode
   is
      K : constant Object_Kind_Type := M.M1.K;
      D : O_Dnode;
   begin
      case M.M1.State is
         when Mstate_Ep =>
            --  Create a pointer variable.
            D := Create_Temp_Init (M.M1.Ptype, M.M1.Ep);
            return Mnode'(M1 => (State => Mstate_Dp,
                                 K => K, T => M.M1.T, Dp => D,
                                 Vtype => M.M1.Vtype, Ptype => M.M1.Ptype));
         when Mstate_Ev =>
            if not Can_Copy then
               raise Internal_Error;
            end if;
            --  Create a scalar variable.
            D := Create_Temp_Init (M.M1.Vtype, M.M1.Ev);
            return Mnode'(M1 => (State => Mstate_Dv,
                                 K => K, T => M.M1.T, Dv => D,
                                 Vtype => M.M1.Vtype, Ptype => M.M1.Ptype));
         when Mstate_Lp =>
            D := Create_Temp_Init (M.M1.Ptype, New_Value (M.M1.Lp));
            return Mnode'(M1 => (State => Mstate_Dp,
                                 K => K, T => M.M1.T, Dp => D,
                                 Vtype => M.M1.Vtype, Ptype => M.M1.Ptype));
         when Mstate_Lv =>
            if M.M1.Ptype = O_Tnode_Null then
               if not Can_Copy then
                  raise Internal_Error;
               end if;
               D := Create_Temp_Init (M.M1.Vtype, New_Value (M.M1.Lv));
               return Mnode'(M1 => (State => Mstate_Dv,
                                    K => K, T => M.M1.T, Dv => D,
                                    Vtype => M.M1.Vtype, Ptype => M.M1.Ptype));

            else
               D := Create_Temp_Ptr (M.M1.Ptype, M.M1.Lv);
               return Mnode'(M1 => (State => Mstate_Dp,
                                    K => K, T => M.M1.T, Dp => D,
                                    Vtype => M.M1.Vtype, Ptype => M.M1.Ptype));
            end if;
         when Mstate_Dp
            | Mstate_Dv =>
            return M;
         when Mstate_Bad
            | Mstate_Null =>
            raise Internal_Error;
      end case;
   end Stabilize;

   procedure Stabilize (M : in out Mnode) is
   begin
      M := Stabilize (M);
   end Stabilize;

   function Stabilize_Value (M : Mnode) return Mnode
   is
      D : O_Dnode;
      E : O_Enode;
   begin
      --  M must be scalar or access.
      pragma Assert (not Is_Composite (M.M1.T));
      case M.M1.State is
         when Mstate_Ev =>
            E := M.M1.Ev;
         when Mstate_Ep =>
            raise Internal_Error;
         when Mstate_Lp =>
            E := New_Value (New_Acc_Value (M.M1.Lp));
         when Mstate_Lv =>
            E := New_Value (M.M1.Lv);
         when Mstate_Dp
            | Mstate_Dv =>
            return M;
         when Mstate_Bad
            | Mstate_Null =>
            raise Internal_Error;
      end case;

      D := Create_Temp_Init (M.M1.Vtype, E);
      return Mnode'(M1 => (State => Mstate_Dv,
                           K => M.M1.K, T => M.M1.T, Dv => D,
                           Vtype => M.M1.Vtype, Ptype => M.M1.Ptype));
   end Stabilize_Value;

   function Create_Temp (Info : Type_Info_Acc;
                         Kind : Object_Kind_Type := Mode_Value)
                         return Mnode is
   begin
      if Is_Complex_Type (Info)
        and then Info.Type_Mode not in Type_Mode_Unbounded
      then
         --  For a complex and constrained object, we just allocate
         --  a pointer to the object.
         return Dp2M (Create_Temp (Info.Ortho_Ptr_Type (Kind)), Info, Kind);
      else
         return Dv2M (Create_Temp (Info.Ortho_Type (Kind)), Info, Kind);
      end if;
   end Create_Temp;

   function New_Value_Selected_Acc_Value (L : O_Lnode; Field : O_Fnode)
                                          return O_Enode is
   begin
      return New_Value
        (New_Selected_Element (New_Access_Element (New_Value (L)), Field));
   end New_Value_Selected_Acc_Value;

   function New_Selected_Acc_Value (L : O_Lnode; Field : O_Fnode)
                                    return O_Lnode is
   begin
      return New_Selected_Element
        (New_Access_Element (New_Value (L)), Field);
   end New_Selected_Acc_Value;

   function New_Indexed_Acc_Value (L : O_Lnode; I : O_Enode) return O_Lnode
   is
   begin
      return New_Indexed_Element (New_Access_Element (New_Value (L)), I);
   end New_Indexed_Acc_Value;

   function New_Acc_Value (L : O_Lnode) return O_Lnode is
   begin
      return New_Access_Element (New_Value (L));
   end New_Acc_Value;

   function Add_Pointer
     (Ptr : O_Enode; Offset : O_Enode; Res_Ptr : O_Tnode) return O_Enode is
   begin
      return New_Unchecked_Address
        (New_Slice
           (New_Access_Element (New_Convert_Ov (Ptr, Char_Ptr_Type)),
            Chararray_Type,
            Offset),
         Res_Ptr);
   end Add_Pointer;

   package Node_Infos is new Tables
     (Table_Component_Type => Ortho_Info_Acc,
      Table_Index_Type => Iir,
      Table_Low_Bound => 0,
      Table_Initial => 1024);

   procedure Init_Node_Infos is
   begin
      --  Create the node extension for translate.
      Node_Infos.Init;
      Node_Infos.Set_Last (4);
      Node_Infos.Table (0 .. 4) := (others => null);
   end Init_Node_Infos;

   procedure Update_Node_Infos
   is
      use Vhdl.Nodes_Priv;
      F, L : Iir;
   begin
      F := Node_Infos.Last;
      L := Get_Last_Node;
      Node_Infos.Set_Last (L);
      Node_Infos.Table (F + 1 .. L) := (others => null);
   end Update_Node_Infos;

   procedure Set_Info (Target : Iir; Info : Ortho_Info_Acc) is
   begin
      pragma Assert (Node_Infos.Table (Target) = null);
      Node_Infos.Table (Target) := Info;
   end Set_Info;

   procedure Clear_Info (Target : Iir) is
   begin
      Node_Infos.Table (Target) := null;
   end Clear_Info;

   function Get_Info (Target : Iir) return Ortho_Info_Acc is
   begin
      return Node_Infos.Table (Target);
   end Get_Info;

   --  Create an ortho_info field of kind KIND for iir node TARGET, and
   --  return it.
   function Add_Info (Target : Iir; Kind : Ortho_Info_Kind)
                      return Ortho_Info_Acc
   is
      Res : Ortho_Info_Acc;
   begin
      pragma Assert (Target /= Null_Iir);
      Res := new Ortho_Info_Type (Kind);
      Set_Info (Target, Res);
      return Res;
   end Add_Info;

   procedure Free_Info (Target : Iir)
   is
      Info : Ortho_Info_Acc;
   begin
      Info := Get_Info (Target);
      if Info /= null then
         Unchecked_Deallocation (Info);
         Clear_Info (Target);
      end if;
   end Free_Info;

   procedure Free_Type_Info (Info : in out Type_Info_Acc) is
   begin
      Unchecked_Deallocation (Info);
   end Free_Type_Info;

   function Get_Ortho_Literal (Target : Iir) return O_Cnode is
   begin
      return Get_Info (Target).Lit_Node;
   end Get_Ortho_Literal;

   function Get_Ortho_Type (Target : Iir; Is_Sig : Object_Kind_Type)
                            return O_Tnode is
   begin
      return Get_Info (Target).Ortho_Type (Is_Sig);
   end Get_Ortho_Type;

   function Is_Composite (Info : Type_Info_Acc) return Boolean is
   begin
      return Info.Type_Mode in Type_Mode_Composite;
   end Is_Composite;

   function Is_Complex_Type (Tinfo : Type_Info_Acc) return Boolean is
   begin
      case Tinfo.Type_Mode is
         when Type_Mode_Non_Composite =>
            return False;
         when Type_Mode_Static_Record
           | Type_Mode_Static_Array =>
            return False;
         when Type_Mode_Complex_Record
           | Type_Mode_Complex_Array =>
            return True;
         when Type_Mode_Unbounded_Record
           | Type_Mode_Unbounded_Array =>
            return False;
         when Type_Mode_Protected =>
            --  Considered as a complex type, as its size is known only in
            --  the body.
            --  Shouldn't be used.
            raise Internal_Error;
         when Type_Mode_Unknown =>
            return False;
      end case;
   end Is_Complex_Type;

   function Is_Static_Type (Tinfo : Type_Info_Acc) return Boolean is
   begin
      case Tinfo.Type_Mode is
         when Type_Mode_Non_Composite =>
            return True;
         when Type_Mode_Static_Record
           | Type_Mode_Static_Array =>
            return True;
         when Type_Mode_Complex_Record
           | Type_Mode_Complex_Array
           | Type_Mode_Unbounded_Record
           | Type_Mode_Unbounded_Array
           | Type_Mode_Protected =>
            return False;
         when Type_Mode_Unknown =>
            return False;
      end case;
   end Is_Static_Type;

   function Is_Unbounded_Type (Tinfo : Type_Info_Acc) return Boolean is
   begin
      return Tinfo.Type_Mode in Type_Mode_Unbounded;
   end Is_Unbounded_Type;

   procedure Free_Node_Infos
   is
      Info : Ortho_Info_Acc;
   begin
      --  Check each node is not marked.
      for I in Node_Infos.First .. Node_Infos.Last loop
         Info := Get_Info (I);
         pragma Assert (Info = null or else not Info.Mark);
      end loop;

      --  Clear duplicated nodes
      for I in Node_Infos.First .. Node_Infos.Last loop
         Info := Get_Info (I);
         if Info /= null then
            if Info.Mark then
               --  This info is shared by another node and was already seen.
               --  Unreference it.
               Clear_Info (I);
            else
               Info.Mark := True;
            end if;
         end if;
      end loop;

      --  Free infos
      for I in Node_Infos.First .. Node_Infos.Last loop
         Info := Get_Info (I);
         if Info /= null then
            if Info.Kind = Kind_Type then
               Free_Type_Info (Info);
            else
               Free_Info (I);
            end if;
         end if;
      end loop;

      Node_Infos.Free;
   end Free_Node_Infos;

   function Get_Type_Info (M : Mnode) return Type_Info_Acc is
   begin
      return M.M1.T;
   end Get_Type_Info;

   function E2M (E : O_Enode; T : Type_Info_Acc; Kind : Object_Kind_Type)
                 return Mnode is
   begin
      if Is_Composite (T) then
         return Mnode'(M1 => (State => Mstate_Ep,
                              K => Kind, T => T, Ep => E,
                              Vtype => T.Ortho_Type (Kind),
                              Ptype => T.Ortho_Ptr_Type (Kind)));
      else
         return Mnode'(M1 => (State => Mstate_Ev,
                              K => Kind, T => T, Ev => E,
                              Vtype => T.Ortho_Type (Kind),
                              Ptype => T.Ortho_Ptr_Type (Kind)));
      end if;
   end E2M;

   function E2M (E : O_Enode;
                 T : Type_Info_Acc;
                 Kind  : Object_Kind_Type;
                 Vtype : O_Tnode;
                 Ptype : O_Tnode)
                return Mnode is
   begin
      pragma Assert (Is_Composite (T));
      return Mnode'(M1 => (State => Mstate_Ep,
                           K => Kind, T => T, Ep => E,
                           Vtype => Vtype, Ptype => Ptype));
   end E2M;

   function Lv2M (L : O_Lnode; T : Type_Info_Acc; Kind : Object_Kind_Type)
                  return Mnode is
   begin
      return Mnode'(M1 => (State => Mstate_Lv,
                           K => Kind, T => T, Lv => L,
                           Vtype => T.Ortho_Type (Kind),
                           Ptype => T.Ortho_Ptr_Type (Kind)));
   end Lv2M;

   function Lv2M (L     : O_Lnode;
                  T     : Type_Info_Acc;
                  Kind  : Object_Kind_Type;
                  Vtype : O_Tnode;
                  Ptype : O_Tnode)
                  return Mnode is
   begin
      return Mnode'(M1 => (State => Mstate_Lv,
                           K => Kind, T => T, Lv => L,
                           Vtype => Vtype, Ptype => Ptype));
   end Lv2M;

   function Lp2M (L : O_Lnode; T : Type_Info_Acc; Kind : Object_Kind_Type)
                  return Mnode is
   begin
      return Mnode'(M1 => (State => Mstate_Lp,
                           K => Kind, T => T, Lp => L,
                           Vtype => T.Ortho_Type (Kind),
                           Ptype => T.Ortho_Ptr_Type (Kind)));
   end Lp2M;

   function Lp2M (L     : O_Lnode;
                  T     : Type_Info_Acc;
                  Kind  : Object_Kind_Type;
                  Vtype : O_Tnode;
                  Ptype : O_Tnode)
                  return Mnode is
   begin
      return Mnode'(M1 => (State => Mstate_Lp,
                           K => Kind, T => T, Lp => L,
                           Vtype => Vtype, Ptype => Ptype));
   end Lp2M;

   function Dv2M (D    : O_Dnode;
                  T    : Type_Info_Acc;
                  Kind : Object_Kind_Type)
                  return Mnode is
   begin
      return Mnode'(M1 => (State => Mstate_Dv,
                           K => Kind, T => T, Dv => D,
                           Vtype => T.Ortho_Type (Kind),
                           Ptype => T.Ortho_Ptr_Type (Kind)));
   end Dv2M;

   function Dv2M (D     : O_Dnode;
                  T     : Type_Info_Acc;
                  Kind  : Object_Kind_Type;
                  Vtype : O_Tnode;
                  Ptype : O_Tnode)
                  return Mnode is
   begin
      return Mnode'(M1 => (State => Mstate_Dv,
                           K => Kind, T => T, Dv => D,
                           Vtype => Vtype,
                           Ptype => Ptype));
   end Dv2M;

   function Dp2M (D     : O_Dnode;
                  T     : Type_Info_Acc;
                  Kind  : Object_Kind_Type;
                  Vtype : O_Tnode;
                  Ptype : O_Tnode)
                  return Mnode is
   begin
      return Mnode'(M1 => (State => Mstate_Dp,
                           K => Kind, T => T, Dp => D,
                           Vtype => Vtype, Ptype => Ptype));
   end Dp2M;

   function Dp2M (D    : O_Dnode;
                  T    : Type_Info_Acc;
                  Kind : Object_Kind_Type)
                  return Mnode is
   begin
      return Mnode'(M1 => (State => Mstate_Dp,
                           K => Kind, T => T, Dp => D,
                           Vtype => T.Ortho_Type (Kind),
                           Ptype => T.Ortho_Ptr_Type (Kind)));
   end Dp2M;

   function M2Lv (M : Mnode) return O_Lnode is
   begin
      case M.M1.State is
         when Mstate_Ev =>
            --  Scalar to var is not possible.
            --  FIXME: This is not coherent with the fact that this
            --  conversion is possible when M is stabilized.
            raise Internal_Error;
         when Mstate_Ep =>
            return New_Access_Element (M.M1.Ep);
         when Mstate_Lp =>
            return New_Acc_Value (M.M1.Lp);
         when Mstate_Lv =>
            return M.M1.Lv;
         when Mstate_Dp =>
            return New_Acc_Value (New_Obj (M.M1.Dp));
         when Mstate_Dv =>
            return New_Obj (M.M1.Dv);
         when Mstate_Null
            | Mstate_Bad =>
            raise Internal_Error;
      end case;
   end M2Lv;

   function M2Lp (M : Mnode) return O_Lnode is
   begin
      case M.M1.State is
         when Mstate_Ev
            | Mstate_Ep =>
            raise Internal_Error;
         when Mstate_Lp =>
            return M.M1.Lp;
         when Mstate_Dp =>
            return New_Obj (M.M1.Dp);
         when Mstate_Lv =>
            if Get_Type_Info (M).Type_Mode in Type_Mode_Fat then
               return New_Obj
                 (Create_Temp_Init (M.M1.Ptype,
                  New_Address (M.M1.Lv, M.M1.Ptype)));
            else
               raise Internal_Error;
            end if;
         when Mstate_Dv
            | Mstate_Null
            | Mstate_Bad =>
            raise Internal_Error;
      end case;
   end M2Lp;

   function M2Dp (M : Mnode) return O_Dnode is
   begin
      case M.M1.State is
         when Mstate_Dp =>
            return M.M1.Dp;
         when Mstate_Dv =>
            return Create_Temp_Init
              (M.M1.Ptype, New_Address (New_Obj (M.M1.Dv), M.M1.Ptype));

         when others =>
            raise Internal_Error;
      end case;
   end M2Dp;

   function M2Dv (M : Mnode) return O_Dnode is
   begin
      case M.M1.State is
         when Mstate_Dv =>
            return M.M1.Dv;
         when others =>
            raise Internal_Error;
      end case;
   end M2Dv;

   function T2M (Atype : Iir; Kind : Object_Kind_Type) return Mnode
   is
      T : Type_Info_Acc;
   begin
      T := Get_Info (Atype);
      return Mnode'(M1 => (State => Mstate_Null,
                           K => Kind, T => T,
                           Vtype => T.Ortho_Type (Kind),
                           Ptype => T.Ortho_Ptr_Type (Kind)));
   end T2M;

   function M2E (M : Mnode) return O_Enode is
   begin
      case M.M1.State is
         when Mstate_Ev =>
            return M.M1.Ev;
         when Mstate_Ep =>
            return M.M1.Ep;
         when Mstate_Lp =>
            case M.M1.T.Type_Mode is
               when Type_Mode_Unknown =>
                  raise Internal_Error;
               when Type_Mode_Thin =>
                  return New_Value (New_Acc_Value (M.M1.Lp));
               when Type_Mode_Fat =>
                  return New_Value (M.M1.Lp);
            end case;
         when Mstate_Dp =>
            case M.M1.T.Type_Mode is
               when Type_Mode_Unknown =>
                  raise Internal_Error;
               when Type_Mode_Thin =>
                  return New_Value (New_Acc_Value (New_Obj (M.M1.Dp)));
               when Type_Mode_Fat =>
                  return New_Value (New_Obj (M.M1.Dp));
            end case;
         when Mstate_Lv =>
            case M.M1.T.Type_Mode is
               when Type_Mode_Unknown =>
                  raise Internal_Error;
               when Type_Mode_Thin =>
                  return New_Value (M.M1.Lv);
               when Type_Mode_Fat =>
                  return New_Address (M.M1.Lv, M.M1.Ptype);
            end case;
         when Mstate_Dv =>
            case M.M1.T.Type_Mode is
               when Type_Mode_Unknown =>
                  raise Internal_Error;
               when Type_Mode_Thin =>
                  return New_Value (New_Obj (M.M1.Dv));
               when Type_Mode_Fat =>
                  return New_Address (New_Obj (M.M1.Dv), M.M1.Ptype);
            end case;
         when Mstate_Bad
            | Mstate_Null =>
            raise Internal_Error;
      end case;
   end M2E;

   function M2Addr (M : Mnode) return O_Enode is
   begin
      case M.M1.State is
         when Mstate_Lp =>
            return New_Value (M.M1.Lp);
         when Mstate_Dp =>
            return New_Value (New_Obj (M.M1.Dp));
         when Mstate_Lv =>
            return New_Address (M.M1.Lv, M.M1.Ptype);
         when Mstate_Dv =>
            return New_Address (New_Obj (M.M1.Dv), M.M1.Ptype);
         when Mstate_Ep =>
            return M.M1.Ep;
         when Mstate_Ev =>
            --  For scalar, M contains the value so there is no lvalue from
            --  which the address can be taken.
            raise Internal_Error;
         when Mstate_Bad
            | Mstate_Null =>
            raise Internal_Error;
      end case;
   end M2Addr;

   --    function Is_Null (M : Mnode) return Boolean is
   --    begin
   --       return M.M1.State = Mstate_Null;
   --    end Is_Null;

   function Is_Stable (M : Mnode) return Boolean is
   begin
      case M.M1.State is
         when Mstate_Dp
            | Mstate_Dv =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Stable;

   --    function Varv2M
   --      (Var : Var_Type; Vtype : Type_Info_Acc; Mode : Object_Kind_Type)
   --      return Mnode is
   --    begin
   --       return Lv2M (Get_Var (Var), Vtype, Mode);
   --    end Varv2M;

   function Varv2M (Var      : Var_Type;
                    Var_Type : Type_Info_Acc;
                    Mode     : Object_Kind_Type;
                    Vtype    : O_Tnode;
                    Ptype    : O_Tnode)
                    return Mnode is
   begin
      return Lv2M (Get_Var (Var), Var_Type, Mode, Vtype, Ptype);
   end Varv2M;

   --  Convert a Lnode for a sub object to an MNODE.
   function Lo2M (L : O_Lnode; Vtype : Type_Info_Acc; Mode : Object_Kind_Type)
                  return Mnode is
   begin
      case Vtype.Type_Mode is
         when Type_Mode_Scalar
           | Type_Mode_Acc
           | Type_Mode_File
           | Type_Mode_Unbounded_Array
           | Type_Mode_Unbounded_Record
           | Type_Mode_Bounds_Acc =>
            return Lv2M (L, Vtype, Mode);
         when Type_Mode_Complex_Array
           | Type_Mode_Complex_Record
           | Type_Mode_Protected =>
            return Lp2M (L, Vtype, Mode);
         when Type_Mode_Static_Array
           | Type_Mode_Static_Record =>
            return Lv2M (L, Vtype, Mode);
         when Type_Mode_Unknown =>
            raise Internal_Error;
      end case;
   end Lo2M;

   function Lo2M (D : O_Dnode; Vtype : Type_Info_Acc; Mode : Object_Kind_Type)
                  return Mnode is
   begin
      case Vtype.Type_Mode is
         when Type_Mode_Scalar
           | Type_Mode_Acc
           | Type_Mode_File
           | Type_Mode_Unbounded_Array
           | Type_Mode_Unbounded_Record
           | Type_Mode_Bounds_Acc =>
            return Dv2M (D, Vtype, Mode);
         when Type_Mode_Complex_Array
           | Type_Mode_Complex_Record
           | Type_Mode_Protected =>
            return Dp2M (D, Vtype, Mode);
         when Type_Mode_Static_Array
           | Type_Mode_Static_Record =>
            return Dv2M (D, Vtype, Mode);
         when Type_Mode_Unknown =>
            raise Internal_Error;
      end case;
   end Lo2M;

   package body Helpers is
      procedure Inc_Var (V : O_Dnode) is
      begin
         New_Assign_Stmt (New_Obj (V),
                          New_Dyadic_Op (ON_Add_Ov,
                                         New_Obj_Value (V),
                                         New_Lit (Ghdl_Index_1)));
      end Inc_Var;

      procedure Dec_Var (V : O_Dnode) is
      begin
         New_Assign_Stmt (New_Obj (V),
                          New_Dyadic_Op (ON_Sub_Ov,
                                         New_Obj_Value (V),
                                         New_Lit (Ghdl_Index_1)));
      end Dec_Var;

      procedure Init_Var (V : O_Dnode) is
      begin
         New_Assign_Stmt (New_Obj (V), New_Lit (Ghdl_Index_0));
      end Init_Var;

      procedure Gen_Exit_When (Label : O_Snode; Cond : O_Enode)
      is
         If_Blk : O_If_Block;
      begin
         Start_If_Stmt (If_Blk, Cond);
         New_Exit_Stmt (Label);
         Finish_If_Stmt (If_Blk);
      end Gen_Exit_When;

      procedure Set_Stack2_Mark (Var : O_Lnode)
      is
         Constr : O_Assoc_List;
      begin
         Start_Association (Constr, Ghdl_Stack2_Mark);
         New_Assign_Stmt (Var, New_Function_Call (Constr));
      end Set_Stack2_Mark;

      procedure Release_Stack2 (Var : O_Lnode)
      is
         Constr : O_Assoc_List;
      begin
         Start_Association (Constr, Ghdl_Stack2_Release);
         New_Association (Constr, New_Value (Var));
         New_Procedure_Call (Constr);
      end Release_Stack2;

      --  Create a temporary variable.
      type Temp_Level_Type;
      type Temp_Level_Acc is access Temp_Level_Type;
      type Temp_Level_Type is record
         --  Link to the outer record.
         Prev            : Temp_Level_Acc;

         --  Nested level.  'Top' level is 0.
         Level           : Natural;

         --  Generated variable id, starts from 0.
         Id              : Natural;

         --  True if a scope was created, as it is created dynamically at the
         --  first use.
         Emitted         : Boolean;

         --  If true, do not mark/release stack2.
         No_Stack2_Mark : Boolean;

         --  Declaration of the variable for the stack2 mark.  The stack2 will
         --  be released at the end of the scope (if used).
         Stack2_Mark     : O_Dnode;
      end record;
      --  Current level.
      Temp_Level : Temp_Level_Acc := null;

      --  List of unused temp_level_type structures.  To be faster, they are
      --  never deallocated.
      Old_Level : Temp_Level_Acc := null;

      --  If set, emit comments for open_temp/close_temp.
      Flag_Debug_Temp : constant Boolean := False;

      procedure Open_Temp
      is
         L : Temp_Level_Acc;
      begin
         --  Allocate a new record.
         if Old_Level /= null then
            --  From unused ones.
            L := Old_Level;
            Old_Level := L.Prev;
         else
            --  No unused, create a new one.
            L := new Temp_Level_Type;
         end if;

         L.all := (Prev => Temp_Level,
                   Level => 0,
                   Id => 0,
                   Emitted => False,
                   No_Stack2_Mark => False,
                   Stack2_Mark => O_Dnode_Null);
         if Temp_Level /= null then
            L.Level := Temp_Level.Level + 1;
         end if;
         Temp_Level := L;

         if Flag_Debug_Temp then
            New_Debug_Comment_Stmt
              ("Open_Temp level " & Natural'Image (L.Level));
         end if;
      end Open_Temp;

      procedure Disable_Stack2_Release is
      begin
         Temp_Level.No_Stack2_Mark := True;
      end Disable_Stack2_Release;

      procedure Open_Local_Temp is
      begin
         Open_Temp;
         Temp_Level.Emitted := True;
      end Open_Local_Temp;

      function Has_Stack2_Mark return Boolean is
      begin
         return Temp_Level.Stack2_Mark /= O_Dnode_Null;
      end Has_Stack2_Mark;

      procedure Stack2_Release is
      begin
         if Temp_Level.Stack2_Mark /= O_Dnode_Null then
            Release_Stack2 (New_Obj (Temp_Level.Stack2_Mark));
            Temp_Level.Stack2_Mark := O_Dnode_Null;
         end if;
      end Stack2_Release;

      procedure Close_Temp
      is
         L : Temp_Level_Acc;
      begin
         --  Check that OPEN_TEMP was called.
         pragma Assert (Temp_Level /= null);

         if Flag_Debug_Temp then
            New_Debug_Comment_Stmt
              ("Close_Temp level " & Natural'Image (Temp_Level.Level));
         end if;

         if Temp_Level.Stack2_Mark /= O_Dnode_Null then
            Stack2_Release;
         end if;
         if Temp_Level.Emitted then
            Finish_Declare_Stmt;
         end if;

         --  Unlink temp_level.
         L := Temp_Level;
         Temp_Level := L.Prev;
         L.Prev := Old_Level;
         Old_Level := L;
      end Close_Temp;

      procedure Close_Local_Temp is
      begin
         Temp_Level.Emitted := False;
         Close_Temp;
      end Close_Local_Temp;

      procedure Free_Old_Temp
      is
         procedure Free is new Ada.Unchecked_Deallocation
           (Temp_Level_Type, Temp_Level_Acc);
         T : Temp_Level_Acc;
      begin
         if Temp_Level /= null then
            --  Missing Close_Temp.
            raise Internal_Error;
         end if;
         loop
            T := Old_Level;
            exit when T = null;
            Old_Level := Old_Level.Prev;
            Free (T);
         end loop;
      end Free_Old_Temp;

      procedure Create_Temp_Stack2_Mark is
      begin
         if Temp_Level.Stack2_Mark /= O_Dnode_Null then
            --  Only the first mark in a region is registred.
            --  The release operation frees the memory allocated after the
            --  first mark.
            return;
         end if;

         if Temp_Level.No_Stack2_Mark then
            --  Stack2 mark and release was explicitly disabled.
            return;
         end if;

         Temp_Level.Stack2_Mark := Create_Temp (Ghdl_Ptr_Type);
         Set_Stack2_Mark (New_Obj (Temp_Level.Stack2_Mark));
      end Create_Temp_Stack2_Mark;

      function Create_Temp (Atype : O_Tnode) return O_Dnode
      is
         Str : String (1 .. 12);
         Val : Natural;
         Res : O_Dnode;
         P   : Natural;
      begin
         if Temp_Level = null then
            --  OPEN_TEMP was never called.
            raise Internal_Error;
            --  This is an hack, just to allow array subtype to array type
            --  conversion.
            --New_Var_Decl
            --  (Res, Create_Uniq_Identifier, O_Storage_Private, Atype);
            --return Res;
         else
            if not Temp_Level.Emitted then
               Temp_Level.Emitted := True;
               Start_Declare_Stmt;
            end if;
         end if;
         Val := Temp_Level.Id;
         Temp_Level.Id := Temp_Level.Id + 1;
         P := Str'Last;
         loop
            Str (P) := Character'Val (Val mod 10 + Character'Pos ('0'));
            Val := Val / 10;
            P := P - 1;
            exit when Val = 0;
         end loop;
         Str (P) := '_';
         P := P - 1;
         Val := Temp_Level.Level;
         loop
            Str (P) := Character'Val (Val mod 10 + Character'Pos ('0'));
            Val := Val / 10;
            P := P - 1;
            exit when Val = 0;
         end loop;
         Str (P) := 'T';
         --Str (12) := Nul;
         New_Var_Decl
           (Res, Get_Identifier (Str (P .. Str'Last)), O_Storage_Local, Atype);
         return Res;
      end Create_Temp;

      function Create_Temp_Init (Atype : O_Tnode; Value : O_Enode)
                                 return O_Dnode
      is
         Res : O_Dnode;
      begin
         Res := Create_Temp (Atype);
         New_Assign_Stmt (New_Obj (Res), Value);
         return Res;
      end Create_Temp_Init;

      function Create_Temp_Ptr (Atype : O_Tnode; Name : O_Lnode)
                                return O_Dnode is
      begin
         return Create_Temp_Init (Atype, New_Address (Name, Atype));
      end Create_Temp_Ptr;

      function Create_Temp_Bounds (Tinfo : Type_Info_Acc) return Mnode is
      begin
         return Dv2M (Create_Temp (Tinfo.B.Bounds_Type),
                      Tinfo, Mode_Value,
                      Tinfo.B.Bounds_Type, Tinfo.B.Bounds_Ptr_Type);
      end Create_Temp_Bounds;

      --  Return a ghdl_index_type literal for NUM.
      function New_Index_Lit (Num : Unsigned_64) return O_Cnode is
      begin
         return New_Unsigned_Literal (Ghdl_Index_Type, Num);
      end New_Index_Lit;

      Uniq_Id : Natural := 0;

      function Create_Uniq_Identifier return Uniq_Identifier_String
      is
         Str : Uniq_Identifier_String;
         Val : Natural;
      begin
         Str (1 .. 3) := "_UI";
         Val := Uniq_Id;
         Uniq_Id := Uniq_Id + 1;
         for I in reverse 4 .. 11 loop
            Str (I) := N2hex (Val mod 16);
            Val := Val / 16;
         end loop;
         return Str;
      end Create_Uniq_Identifier;

      function Create_Uniq_Identifier return O_Ident is
      begin
         return Get_Identifier (Create_Uniq_Identifier);
      end Create_Uniq_Identifier;

   end Helpers;

end Trans;
