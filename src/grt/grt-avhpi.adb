--  GHDL Run Time (GRT) - VHPI implementation for Ada.
--  Copyright (C) 2002 - 2014 Tristan Gingold
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
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
with Grt.Errors; use Grt.Errors;
with Grt.Vstrings; use Grt.Vstrings;
with Grt.Rtis_Utils; use Grt.Rtis_Utils;
with Grt.To_Strings;

package body Grt.Avhpi is
   procedure Get_Root_Inst (Res : out VhpiHandleT) is
   begin
      Res := (Kind => VhpiRootInstK,
              Ctxt => Get_Top_Context);
   end Get_Root_Inst;

   procedure Get_Root_Scope (Res : out VhpiHandleT) is
   begin
      Res := (Kind => AvhpiRootScopeK,
              Ctxt => Null_Context);
   end Get_Root_Scope;

   procedure Get_Package_Inst (Res : out VhpiHandleT) is
   begin
      --  Ctxt is the list of instantiated packages.
      Res := (Kind => VhpiIteratorK,
              Ctxt => (Base => Null_Address,
                       Block => To_Ghdl_Rti_Access (Ghdl_Rti_Top'Address)),
              Rel => VhpiPackInsts,
              It_Cur => 0,
              It2 => 0,
              Max2 => 0);
   end Get_Package_Inst;

   --  Number of elements in an array.
   function Ranges_To_Length (Rngs : Ghdl_Range_Array;
                              Indexes : Ghdl_Rti_Arr_Acc)
                             return Ghdl_Index_Type
   is
      Res : Ghdl_Index_Type;
   begin
      Res := 1;
      for I in Rngs'Range loop
         Res := Res * Range_To_Length
           (Rngs (I), Get_Base_Type (Indexes (I - Rngs'First)));
      end loop;
      return Res;
   end Ranges_To_Length;

   procedure Vhpi_Iterator (Rel : VhpiOneToManyT;
                            Ref : VhpiHandleT;
                            Res : out VhpiHandleT;
                            Error : out AvhpiErrorT) is
   begin
      --  Default value in case of success.
      Res := (Kind => VhpiIteratorK,
              Ctxt => Ref.Ctxt,
              Rel => Rel,
              It_Cur => 0,
              It2 => 0,
              Max2 => 0);
      Error := AvhpiErrorOk;

      case Rel is
         when VhpiInternalRegions =>
            case Ref.Kind is
               when VhpiRootInstK
                 | VhpiArchBodyK
                 | VhpiBlockStmtK
                 | VhpiIfGenerateK =>
                  return;
               when VhpiForGenerateK =>
                  Res.It2 := 1;
                  return;
               when VhpiCompInstStmtK =>
                  Get_Instance_Context (Ref.Inst, Ref.Ctxt, Res.Ctxt);
                  return;
               when AvhpiRootScopeK =>
                  Res := (Kind => AvhpiRootScopeIteratorK,
                          Ctxt => Ref.Ctxt,
                          Rel => Rel,
                          It_Cur => 0,
                          It2 => 0,
                          Max2 => 0);
                  return;
               when others =>
                  null;
            end case;
         when VhpiDecls =>
            case Ref.Kind is
               when VhpiArchBodyK
                 | VhpiBlockStmtK
                 | VhpiIfGenerateK
                 | VhpiForGenerateK =>
                  return;
               when VhpiRootInstK
                 | VhpiPackInstK =>
                  Res.It2 := 1;
                  return;
               when VhpiCompInstStmtK =>
                  Get_Instance_Context (Ref.Inst, Ref.Ctxt, Res.Ctxt);
                  Res.It2 := 1;
                  return;
               when others =>
                  null;
            end case;
         when VhpiIndexedNames =>
            case Ref.Kind is
               when VhpiGenericDeclK
                  | VhpiConstDeclK=>
                  Res := (Kind => AvhpiNameIteratorK,
                          Ctxt => Ref.Ctxt,
                          N_Addr => Avhpi_Get_Address (Ref),
                          N_Type => Ref.Obj.Obj_Type,
                          N_Idx => 0,
                          N_Obj => Ref.Obj);
               when VhpiIndexedNameK =>
                  Res := (Kind => AvhpiNameIteratorK,
                          Ctxt => Ref.Ctxt,
                          N_Addr => Ref.N_Addr,
                          N_Type => Ref.N_Type,
                          N_Idx => 0,
                          N_Obj => Ref.N_Obj);
               when others =>
                  Error := AvhpiErrorNotImplemented;
                  return;
            end case;
            case Res.N_Type.Kind is
               when Ghdl_Rtik_Subtype_Array =>
                  declare
                     St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                       To_Ghdl_Rtin_Subtype_Composite_Acc (Res.N_Type);
                     Bt : constant Ghdl_Rtin_Type_Array_Acc :=
                       To_Ghdl_Rtin_Type_Array_Acc (St.Basetype);
                     Rngs : Ghdl_Range_Array (0 .. Bt.Nbr_Dim - 1);
                     Layout : Address;
                  begin
                     Layout :=
                       Loc_To_Addr (St.Common.Depth, St.Layout, Res.Ctxt);
                     Bound_To_Range
                       (Array_Layout_To_Bounds (Layout), Bt, Rngs);
                     Res.N_Idx := Ranges_To_Length (Rngs, Bt.Indexes);
                  end;
               when others =>
                  Error := AvhpiErrorBadRel;
            end case;
            return;
         when others =>
            null;
      end case;
      --  Failure.
      Res := Null_Handle;
      Error := AvhpiErrorNotImplemented;
   end Vhpi_Iterator;

   --  OBJ_RTI is the RTI for the base name.
   function Add_Index (Ctxt : Rti_Context;
                       Obj_Base : Address;
                       Obj_Rti : Ghdl_Rtin_Object_Acc;
                       El_Type : Ghdl_Rti_Access;
                       Off : Ghdl_Index_Type) return Address
   is
      Is_Sig : Boolean;
      El_Size : Ghdl_Index_Type;
      El_Type1 : Ghdl_Rti_Access;
   begin
      case Obj_Rti.Common.Kind is
         when Ghdl_Rtik_Generic
            | Ghdl_Rtik_Constant =>
            Is_Sig := False;
         when Ghdl_Rtik_Signal =>
            Is_Sig := True;
         when others =>
            Internal_Error ("add_index(1)");
      end case;

      if El_Type.Kind = Ghdl_Rtik_Subtype_Scalar then
         El_Type1 := Get_Base_Type (El_Type);
      else
         El_Type1 := El_Type;
      end if;

      case El_Type1.Kind is
         when Ghdl_Rtik_Type_P64 =>
            if Is_Sig then
               El_Size := Address'Size / Storage_Unit;
            else
               El_Size := Ghdl_I64'Size / Storage_Unit;
            end if;
         when Ghdl_Rtik_Subtype_Array =>
            declare
               Sizes : Ghdl_Indexes_Ptr;
            begin
               Sizes := To_Ghdl_Indexes_Ptr
                 (Loc_To_Addr
                    (El_Type1.Depth,
                     To_Ghdl_Rtin_Subtype_Composite_Acc (El_Type1).Layout,
                     Ctxt));
               if Is_Sig then
                  El_Size := Sizes.Signal;
               else
                  El_Size := Sizes.Value;
               end if;
            end;
         when others =>
            Internal_Error ("add_index(2)");
      end case;
      return Obj_Base + Off * El_Size;
   end Add_Index;

   procedure Vhpi_Scan_Indexed_Name (Iterator : in out VhpiHandleT;
                                     Res : out VhpiHandleT;
                                     Error : out AvhpiErrorT)
   is
      El_Type : Ghdl_Rti_Access;
   begin
      if Iterator.N_Idx = 0 then
         Error := AvhpiErrorIteratorEnd;
         return;
      end if;

      El_Type := To_Ghdl_Rtin_Type_Array_Acc
        (Get_Base_Type (Iterator.N_Type)).Element;

      Res := (Kind => VhpiIndexedNameK,
              Ctxt => Iterator.Ctxt,
              N_Addr => Iterator.N_Addr,
              N_Type => El_Type,
              N_Idx => 0,
              N_Obj => Iterator.N_Obj);

      --  Increment Address.
      Iterator.N_Addr := Add_Index
        (Iterator.Ctxt, Iterator.N_Addr, Iterator.N_Obj, El_Type, 1);

      Iterator.N_Idx := Iterator.N_Idx - 1;
      Error := AvhpiErrorOk;
   end Vhpi_Scan_Indexed_Name;

   procedure Vhpi_Scan_Internal_Regions (Iterator : in out VhpiHandleT;
                                         Res : out VhpiHandleT;
                                         Error : out AvhpiErrorT)
   is
      Blk : Ghdl_Rtin_Block_Acc;
      Ch : Ghdl_Rti_Access;
      Nblk : Ghdl_Rtin_Block_Acc;
   begin
      Blk := To_Ghdl_Rtin_Block_Acc (Iterator.Ctxt.Block);
      if Blk = null then
         Error := AvhpiErrorIteratorEnd;
         return;
      end if;

      loop
         << Again >> null;
         if Iterator.It_Cur >= Blk.Nbr_Child then
            Error := AvhpiErrorIteratorEnd;
            return;
         end if;

         Ch := Blk.Children (Iterator.It_Cur);
         Nblk := To_Ghdl_Rtin_Block_Acc (Ch);

         if Iterator.Max2 /= 0 then
            --  A for generate.
            Iterator.It2 := Iterator.It2 + 1;
            if Iterator.It2 >= Iterator.Max2 then
               --  End of loop.
               Iterator.Max2 := 0;
               Iterator.It_Cur := Iterator.It_Cur + 1;
               goto Again;
            else
               pragma Assert (Ch.Kind = Ghdl_Rtik_For_Generate);
               declare
                  Gen : constant Ghdl_Rtin_Generate_Acc :=
                    To_Ghdl_Rtin_Generate_Acc (Ch);
                  Base : Address;
               begin
                  Base := To_Addr_Acc (Iterator.Ctxt.Base + Gen.Loc).all;
                  Base := Base + Iterator.It2 * Gen.Size;
                  Res := (Kind => VhpiForGenerateK,
                          Ctxt => (Base => Base,
                                   Block => Gen.Child));

                  Error := AvhpiErrorOk;
                  return;
               end;
            end if;
         end if;


         Iterator.It_Cur := Iterator.It_Cur + 1;

         case Ch.Kind is
            when Ghdl_Rtik_Process =>
               Res := (Kind => VhpiProcessStmtK,
                       Ctxt => (Base => Iterator.Ctxt.Base + Nblk.Loc,
                                Block => Ch));
               Error := AvhpiErrorOk;
               return;
            when Ghdl_Rtik_Block =>
               Res := (Kind => VhpiBlockStmtK,
                       Ctxt => (Base => Iterator.Ctxt.Base + Nblk.Loc,
                                Block => Ch));
               Error := AvhpiErrorOk;
               return;
            when Ghdl_Rtik_If_Generate =>
               Res := (Kind => VhpiIfGenerateK,
                       Ctxt => Get_If_Case_Generate_Child (Iterator.Ctxt, Ch));
               --  Return only if the condition is true.
               if Res.Ctxt.Base /= Null_Address then
                  Error := AvhpiErrorOk;
                  return;
               end if;
            when Ghdl_Rtik_For_Generate =>
               declare
                  Gen : constant Ghdl_Rtin_Generate_Acc :=
                    To_Ghdl_Rtin_Generate_Acc (Ch);
               begin
                  Res := (Kind => VhpiForGenerateK,
                          Ctxt => (Base => To_Addr_Acc (Iterator.Ctxt.Base
                                                          + Gen.Loc).all,
                                   Block => Gen.Child));
                  Iterator.Max2 :=
                    Get_For_Generate_Length (Gen, Iterator.Ctxt);
                  Iterator.It2 := 0;
                  if Iterator.Max2 > 0 then
                     Iterator.It_Cur := Iterator.It_Cur - 1;
                     Error := AvhpiErrorOk;
                     return;
                  end if;
                  --  If the iterator range is nul, then continue to scan.
               end;
            when Ghdl_Rtik_Instance =>
               Res := (Kind => VhpiCompInstStmtK,
                       Ctxt => Iterator.Ctxt,
                       Inst => To_Ghdl_Rtin_Instance_Acc (Ch));
               Error := AvhpiErrorOk;
               return;
            when others =>
               --  Next one.
               null;
         end case;
      end loop;
   end Vhpi_Scan_Internal_Regions;

   procedure Vhpi_Scan_Root_Design (Iterator : in out VhpiHandleT;
                                    Res : out VhpiHandleT;
                                    Error : out AvhpiErrorT) is
   begin
      if Iterator.It_Cur = 0 then
         Get_Root_Inst (Res);
         Iterator.It_Cur := 1;
         Error := AvhpiErrorOk;
      else
         Error := AvhpiErrorIteratorEnd;
      end if;
   end Vhpi_Scan_Root_Design;

   procedure Rti_To_Handle (Rti : Ghdl_Rti_Access;
                            Ctxt : Rti_Context;
                            Res : out VhpiHandleT)
   is
   begin
      case Rti.Kind is
         when Ghdl_Rtik_Signal =>
            Res := (Kind => VhpiSigDeclK,
                    Ctxt => Ctxt,
                    Obj => To_Ghdl_Rtin_Object_Acc (Rti));
         when Ghdl_Rtik_Port =>
            Res := (Kind => VhpiPortDeclK,
                    Ctxt => Ctxt,
                    Obj => To_Ghdl_Rtin_Object_Acc (Rti));
         when Ghdl_Rtik_Generic =>
            Res := (Kind => VhpiGenericDeclK,
                    Ctxt => Ctxt,
                    Obj => To_Ghdl_Rtin_Object_Acc (Rti));
         when Ghdl_Rtik_Constant =>
            Res := (Kind => VhpiConstDeclK,
                    Ctxt => Ctxt,
                    Obj => To_Ghdl_Rtin_Object_Acc (Rti));
         when Ghdl_Rtik_Subtype_Array =>
            declare
               Atype : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
               Bt : constant Ghdl_Rtin_Type_Array_Acc :=
                 To_Ghdl_Rtin_Type_Array_Acc (Atype.Basetype);
            begin
               if Atype.Name = Bt.Name then
                  Res := (Kind => VhpiArrayTypeDeclK,
                          Ctxt => Ctxt,
                          Atype => Rti);
               else
                  Res := (Kind => VhpiSubtypeDeclK,
                          Ctxt => Ctxt,
                          Atype => Rti);
               end if;
            end;
         when Ghdl_Rtik_Type_Array =>
            Res := (Kind => VhpiArrayTypeDeclK,
                    Ctxt => Ctxt,
                    Atype => Rti);
         when Ghdl_Rtik_Type_B1
           | Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32 =>
            Res := (Kind => VhpiEnumTypeDeclK,
                    Ctxt => Ctxt,
                    Atype => Rti);
         when Ghdl_Rtik_Type_P32
           | Ghdl_Rtik_Type_P64 =>
            Res := (Kind => VhpiPhysTypeDeclK,
                    Ctxt => Ctxt,
                    Atype => Rti);
         when Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_I64 =>
            Res := (Kind => VhpiIntTypeDeclK,
                    Ctxt => Ctxt,
                    Atype => Rti);
         when Ghdl_Rtik_Subtype_Scalar =>
            Res := (Kind => VhpiSubtypeDeclK,
                    Ctxt => Ctxt,
                    Atype => Rti);
         when others =>
            Res := (Kind => VhpiUndefined,
                    Ctxt => Ctxt);
      end case;
   end Rti_To_Handle;

   procedure Vhpi_Scan_Decls (Iterator : in out VhpiHandleT;
                              Res : out VhpiHandleT;
                              Error : out AvhpiErrorT)
   is
      Blk : Ghdl_Rtin_Block_Acc;
      Ch : Ghdl_Rti_Access;
   begin
      Blk := To_Ghdl_Rtin_Block_Acc (Iterator.Ctxt.Block);

      --  If there is no context, returns now.
      --  This may happen for a unbound compinststmt.
      if Blk = null then
         Error := AvhpiErrorIteratorEnd;
         return;
      end if;

      if Iterator.It2 = 1 then
         case Blk.Common.Kind is
            when Ghdl_Rtik_Architecture =>
               --  Iterate on the entity.
               Blk := To_Ghdl_Rtin_Block_Acc (Blk.Parent);
            when Ghdl_Rtik_Package_Body =>
               --  Iterate on the package.
               Blk := To_Ghdl_Rtin_Block_Acc (Blk.Parent);
            when Ghdl_Rtik_Package =>
               --  Only for std.standard.
               Iterator.It2 := 0;
            when others =>
               Internal_Error ("vhpi_scan_decls");
         end case;
      end if;
      loop
         loop
            exit when Iterator.It_Cur >= Blk.Nbr_Child;

            Ch := Blk.Children (Iterator.It_Cur);

            Iterator.It_Cur := Iterator.It_Cur + 1;

            case Ch.Kind is
               when Ghdl_Rtik_Port
                 | Ghdl_Rtik_Generic
                 | Ghdl_Rtik_Constant
                 | Ghdl_Rtik_Signal
                 | Ghdl_Rtik_Type_Array
                 | Ghdl_Rtik_Subtype_Array
                 | Ghdl_Rtik_Type_E8
                 | Ghdl_Rtik_Type_E32
                 | Ghdl_Rtik_Type_B1
                 | Ghdl_Rtik_Subtype_Scalar =>
                  Rti_To_Handle (Ch, Iterator.Ctxt, Res);
                  if Res.Kind /= VhpiUndefined then
                     Error := AvhpiErrorOk;
                     return;
                  else
                     Internal_Error ("vhpi_scan_decls");
                  end if;
               when others =>
                  null;
            end case;
         end loop;
         case Iterator.It2 is
            when 1 =>
               --  Iterate on the architecture/package decl.
               Iterator.It2 := 0;
               Blk := To_Ghdl_Rtin_Block_Acc (Iterator.Ctxt.Block);
               Iterator.It_Cur := 0;
            when others =>
               exit;
         end case;
      end loop;
      Error := AvhpiErrorIteratorEnd;
   end Vhpi_Scan_Decls;

   procedure Vhpi_Scan_Pack_Insts (Iterator : in out VhpiHandleT;
                                   Res : out VhpiHandleT;
                                   Error : out AvhpiErrorT)
   is
      Blk : Ghdl_Rtin_Block_Acc;
   begin
      Blk := To_Ghdl_Rtin_Block_Acc (Iterator.Ctxt.Block);
      if Iterator.It_Cur >= Blk.Nbr_Child then
         Error := AvhpiErrorIteratorEnd;
         return;
      end if;
      Res := (Kind => VhpiPackInstK,
              Ctxt => (Base => Null_Address,
                       Block => Blk.Children (Iterator.It_Cur)));
      Iterator.It_Cur := Iterator.It_Cur + 1;
      Error := AvhpiErrorOk;
   end Vhpi_Scan_Pack_Insts;

   procedure Vhpi_Scan (Iterator : in out VhpiHandleT;
                        Res : out VhpiHandleT;
                        Error : out AvhpiErrorT)
   is
   begin
      case Iterator.Kind is
         when AvhpiNameIteratorK =>
            case Iterator.N_Type.Kind is
               when Ghdl_Rtik_Subtype_Array =>
                  Vhpi_Scan_Indexed_Name (Iterator, Res, Error);
               when others =>
                  Error := AvhpiErrorHandle;
                  Res := Null_Handle;
            end case;
         when VhpiIteratorK =>
            case Iterator.Rel is
               when VhpiPackInsts =>
                  Vhpi_Scan_Pack_Insts (Iterator, Res, Error);
               when VhpiInternalRegions =>
                  Vhpi_Scan_Internal_Regions (Iterator, Res, Error);
               when VhpiDecls =>
                  Vhpi_Scan_Decls (Iterator, Res, Error);
               when others =>
                  Res := Null_Handle;
                  Error := AvhpiErrorNotImplemented;
            end case;
         when AvhpiRootScopeIteratorK =>
            Vhpi_Scan_Root_Design (Iterator, Res, Error);
         when others =>
            Error := AvhpiErrorHandle;
            Res := Null_Handle;
      end case;
   end Vhpi_Scan;

   function Avhpi_Get_Base_Name (Obj : VhpiHandleT) return Ghdl_C_String
   is
   begin
      case Obj.Kind is
         when VhpiEnumTypeDeclK =>
            return To_Ghdl_Rtin_Type_Enum_Acc (Obj.Atype).Name;
         when VhpiPackInstK
           | VhpiArchBodyK
           | VhpiEntityDeclK
           | VhpiProcessStmtK
           | VhpiBlockStmtK =>
            return To_Ghdl_Rtin_Block_Acc (Obj.Ctxt.Block).Name;
         when VhpiIfGenerateK
           | VhpiForGenerateK =>
            declare
               --  The context is a generate body.
               Gen : constant Ghdl_Rtin_Block_Acc :=
                 To_Ghdl_Rtin_Block_Acc (Obj.Ctxt.Block);
            begin
               --  Get the name of the if/for/case generate.
               return To_Ghdl_Rtin_Generate_Acc (Gen.Parent).Name;
            end;
         when VhpiRootInstK =>
            declare
               Blk : Ghdl_Rtin_Block_Acc;
            begin
               --  Get top architecture.
               Blk := To_Ghdl_Rtin_Block_Acc (Obj.Ctxt.Block);
               --  From architecture to entity.
               Blk := To_Ghdl_Rtin_Block_Acc (Blk.Parent);
               return Blk.Name;
            end;
         when VhpiCompInstStmtK =>
            return Obj.Inst.Name;
         when VhpiSigDeclK
           | VhpiPortDeclK
           | VhpiGenericDeclK
           | VhpiConstDeclK =>
            return Obj.Obj.Name;
         when VhpiSubtypeDeclK =>
            return To_Ghdl_Rtin_Subtype_Scalar_Acc (Obj.Atype).Name;
         when others =>
            return null;
      end case;
   end Avhpi_Get_Base_Name;

   procedure Vhpi_Get_Str (Property : VhpiStrPropertyT;
                           Obj : VhpiHandleT;
                           Res : out Ghdl_C_String) is
   begin
      Res := null;

      case Property is
         when VhpiFileNameP =>
            declare
               Parent : Ghdl_Rti_Access;
            begin
               Parent := Obj.Ctxt.Block;
               while Parent /= null loop
                  case Parent.Kind is
                     when Ghdl_Rtik_Package
                       | Ghdl_Rtik_Package_Body
                       | Ghdl_Rtik_Entity
                       | Ghdl_Rtik_Architecture =>
                        Res :=
                          To_Ghdl_Rtin_Block_Filename_Acc (Parent).Filename;
                        return;
                     when Ghdl_Rtik_Block
                       | Ghdl_Rtik_Process =>
                        Parent :=
                          To_Ghdl_Rtin_Block_Acc (Parent).Parent;
                     when others =>
                        return;
                  end case;
               end loop;
            end;
         when others =>
            null;
      end case;
   end Vhpi_Get_Str;

   procedure Vhpi_Get_Str (Property : VhpiStrPropertyT;
                           Obj : VhpiHandleT;
                           Res : out String;
                           Len : out Natural)
   is
      subtype R_Type is String (1 .. Res'Length);
      R : R_Type renames Res;

      procedure Add (C : Character) is
      begin
         Len := Len + 1;
         if Len <= R_Type'Last then
            R (Len) := C;
         end if;
      end Add;

      procedure Add (Str : String) is
      begin
         for I in Str'Range loop
            Add (Str (I));
         end loop;
      end Add;

      procedure Add (Str : Ghdl_C_String) is
      begin
         if Str = null then
            return;
         end if;
         for I in Str'Range loop
            exit when Str (I) = NUL;
            Add (Str (I));
         end loop;
      end Add;
   begin
      Len := 0;

      case Property is
         when VhpiFileNameP =>
            declare
               Str : Ghdl_C_String;
            begin
               Vhpi_Get_Str (Property, Obj, Str);
               Add (Str);
            end;
         when VhpiNameP =>
            case Obj.Kind is
               when VhpiEnumTypeDeclK =>
                  Add (To_Ghdl_Rtin_Type_Enum_Acc (Obj.Atype).Name);
               when VhpiIntTypeDeclK =>
                  Add (To_Ghdl_Rtin_Type_Scalar_Acc (Obj.Atype).Name);
               when VhpiSubtypeDeclK =>
                  Add (To_Ghdl_Rtin_Subtype_Scalar_Acc (Obj.Atype).Name);
               when VhpiArrayTypeDeclK =>
                  Add (To_Ghdl_Rtin_Type_Array_Acc (Obj.Atype).Name);
               when VhpiPackInstK
                 | VhpiArchBodyK
                 | VhpiEntityDeclK
                 | VhpiProcessStmtK
                 | VhpiBlockStmtK =>
                  Add (To_Ghdl_Rtin_Block_Acc (Obj.Ctxt.Block).Name);
               when VhpiRootInstK =>
                  declare
                     Blk : Ghdl_Rtin_Block_Acc;
                  begin
                     Blk := To_Ghdl_Rtin_Block_Acc (Obj.Ctxt.Block);
                     Blk := To_Ghdl_Rtin_Block_Acc (Blk.Parent);
                     Add (Blk.Name);
                  end;
               when VhpiCompInstStmtK =>
                  Add (Obj.Inst.Name);
               when VhpiSigDeclK
                 | VhpiPortDeclK
                 | VhpiGenericDeclK
                 | VhpiConstDeclK =>
                  Add (Obj.Obj.Name);
               when VhpiIfGenerateK =>
                  Add (To_Ghdl_Rtin_Generate_Acc
                         (To_Ghdl_Rtin_Block_Acc
                            (Obj.Ctxt.Block).Parent).Name);
               when VhpiForGenerateK =>
                  declare
                     Blk : constant Ghdl_Rtin_Block_Acc :=
                       To_Ghdl_Rtin_Block_Acc (Obj.Ctxt.Block);
                     Iter : constant Ghdl_Rtin_Object_Acc :=
                       To_Ghdl_Rtin_Object_Acc (Blk.Children (0));
                     Vptr : constant Ghdl_Value_Ptr := To_Ghdl_Value_Ptr
                       (Loc_To_Addr (Iter.Common.Depth, Iter.Loc, Obj.Ctxt));
                     Iter_Type : Ghdl_Rti_Access;
                     Buf : String (1 .. 12);
                     Buf_Len : Natural;
                  begin
                     --  Add the name of the generate (need to skip the
                     --  generate body).
                     Add (To_Ghdl_Rtin_Generate_Acc (Blk.Parent).Name);
                     Add ('(');
                     Iter_Type := Iter.Obj_Type;
                     if Iter_Type.Kind = Ghdl_Rtik_Subtype_Scalar then
                        Iter_Type := To_Ghdl_Rtin_Subtype_Scalar_Acc
                          (Iter_Type).Basetype;
                     end if;
                     case Iter_Type.Kind is
                        when Ghdl_Rtik_Type_I32 =>
                           Grt.To_Strings.To_String (Buf, Buf_Len, Vptr.I32);
                           Add (Buf (Buf_Len .. Buf'Last));
--                         when Ghdl_Rtik_Type_E8 =>
--                            Disp_Enum_Value
--                              (Stream, Rti, Ghdl_Index_Type (Vptr.E8));
--                         when Ghdl_Rtik_Type_E32 =>
--                            Disp_Enum_Value
--                              (Stream, Rti, Ghdl_Index_Type (Vptr.E32));
--                         when Ghdl_Rtik_Type_B1 =>
--                            Disp_Enum_Value
--                              (Stream, Rti,
--                               Ghdl_Index_Type (Ghdl_B1'Pos (Vptr.B1)));
                        when others =>
                           Add ('?');
                     end case;
                     --Disp_Value (stdout, Iter.Obj_Type, Ctxt, Addr, False);
                     Add (')');
                  end;
               when others =>
                  null;
            end case;
         when VhpiCompNameP =>
            case Obj.Kind is
               when VhpiCompInstStmtK =>
                  declare
                     Comp : Ghdl_Rtin_Component_Acc;
                  begin
                     Comp := To_Ghdl_Rtin_Component_Acc (Obj.Inst.Instance);
                     if Comp.Common.Kind = Ghdl_Rtik_Component then
                        Add (Comp.Name);
                     end if;
                  end;
               when others =>
                  null;
            end case;
         when VhpiLibLogicalNameP =>
            case Obj.Kind is
               when VhpiPackInstK
                 | VhpiArchBodyK
                 | VhpiEntityDeclK =>
                  declare
                     Blk : Ghdl_Rtin_Block_Acc;
                     Lib : Ghdl_Rtin_Type_Scalar_Acc;
                  begin
                     Blk := To_Ghdl_Rtin_Block_Acc (Obj.Ctxt.Block);
                     if Blk.Common.Kind = Ghdl_Rtik_Package_Body then
                        Blk := To_Ghdl_Rtin_Block_Acc (Blk.Parent);
                     end if;
                     Lib := To_Ghdl_Rtin_Type_Scalar_Acc (Blk.Parent);
                     if Lib.Common.Kind /= Ghdl_Rtik_Library then
                        Internal_Error ("VhpiLibLogicalNameP");
                     end if;
                     Add (Lib.Name);
                  end;
               when others =>
                  null;
            end case;
         when VhpiFullNameP =>
            declare
               Rstr : Rstring;
               Nctxt : Rti_Context;
            begin
               if Obj.Kind = VhpiCompInstStmtK then
                  Get_Instance_Context (Obj.Inst, Obj.Ctxt, Nctxt);
                  Get_Path_Name (Rstr, Nctxt, ':', False);
               else
                  Get_Path_Name (Rstr, Obj.Ctxt, ':', False);
               end if;
               Copy (Rstr, R, Len);
               Free (Rstr);
               case Obj.Kind is
                  when VhpiCompInstStmtK =>
                     null;
                  when VhpiPortDeclK
                    | VhpiSigDeclK =>
                     Add (':');
                     Add (Obj.Obj.Name);
                  when others =>
                     null;
               end case;
            end;
         when others =>
            null;
      end case;
   end Vhpi_Get_Str;

   procedure Vhpi_Handle (Rel : VhpiOneToOneT;
                          Ref : VhpiHandleT;
                          Res : out VhpiHandleT;
                          Error : out AvhpiErrorT)
   is
   begin
      --  Default error.
      Error := AvhpiErrorNotImplemented;
      Res := Null_Handle;

      case Rel is
         when VhpiDesignUnit =>
            case Ref.Kind is
               when VhpiRootInstK =>
                  case Ref.Ctxt.Block.Kind is
                     when Ghdl_Rtik_Architecture =>
                        Res := (Kind => VhpiArchBodyK,
                                Ctxt => Ref.Ctxt);
                        Error := AvhpiErrorOk;
                        return;
                     when others =>
                        return;
                  end case;
               when VhpiCompInstStmtK =>
                  Res := (Kind => VhpiArchBodyK,
                          Ctxt => Null_Context);
                  Get_Instance_Context (Ref.Inst, Ref.Ctxt, Res.Ctxt);
                  if Res.Ctxt = Null_Context then
                     --  Component is not bound.
                     Res := Null_Handle;
                  else
                     pragma Assert
                       (Ref.Ctxt.Block.Kind = Ghdl_Rtik_Architecture);
                     null;
                  end if;
                  Error := AvhpiErrorOk;
                  return;
               when others =>
                  return;
            end case;

         when VhpiPrimaryUnit =>
            case Ref.Kind is
               when VhpiArchBodyK =>
                  declare
                     Rti : Ghdl_Rti_Access;
                     Ent : Ghdl_Rtin_Block_Acc;
                  begin
                     Rti := To_Ghdl_Rtin_Block_Acc (Ref.Ctxt.Block).Parent;
                     Ent := To_Ghdl_Rtin_Block_Acc (Rti);
                     Res := (Kind => VhpiEntityDeclK,
                             Ctxt => (Base => Ref.Ctxt.Base + Ent.Loc,
                                      Block => Rti));
                     Error := AvhpiErrorOk;
                  end;
               when others =>
                  return;
            end case;

         when VhpiIterScheme =>
            case Ref.Kind is
               when VhpiForGenerateK =>
                  declare
                     Blk : constant Ghdl_Rtin_Block_Acc :=
                       To_Ghdl_Rtin_Block_Acc (Ref.Ctxt.Block);
                     Iter : Ghdl_Rtin_Object_Acc;
                  begin
                     Iter := To_Ghdl_Rtin_Object_Acc (Blk.Children (0));
                     Res := (Kind => VhpiConstDeclK,
                             Ctxt => Ref.Ctxt,
                             Obj => Iter);
                     Error := AvhpiErrorOk;
                  end;
               when others =>
                  return;
            end case;

         when VhpiSubtype =>
            case Ref.Kind is
               when VhpiPortDeclK
                  | VhpiSigDeclK
                  | VhpiGenericDeclK
                  | VhpiConstDeclK =>
                  Res := (Kind => VhpiSubtypeIndicK,
                          Ctxt => Ref.Ctxt,
                          Atype => Ref.Obj.Obj_Type);
                  Error := AvhpiErrorOk;
               when VhpiIndexedNameK =>
                  Res := (Kind => VhpiSubtypeIndicK,
                          Ctxt => Ref.Ctxt,
                          Atype => Ref.N_Type);
                  Error := AvhpiErrorOk;
               when others =>
                  return;
            end case;

         when VhpiTypeMark =>
            case Ref.Kind is
               when VhpiSubtypeIndicK =>
                  --  FIXME: if the subtype is anonymous, return the base type.
                  Rti_To_Handle (Ref.Atype, Ref.Ctxt, Res);
                  if Res.Kind /= VhpiUndefined then
                     Error := AvhpiErrorOk;
                  end if;
                  return;
               when others =>
                  return;
            end case;

         when VhpiBaseType =>
            declare
               Atype : Ghdl_Rti_Access;
            begin
               case Ref.Kind is
                  when VhpiSubtypeIndicK
                     | VhpiSubtypeDeclK
                     | VhpiArrayTypeDeclK =>
                     Atype := Ref.Atype;
                  when VhpiGenericDeclK
                     | VhpiConstDeclK
                     | VhpiSigDeclK =>
                     Atype := Ref.Obj.Obj_Type;
                  when VhpiIndexedNameK =>
                     Atype := Ref.N_Type;
                  when others =>
                     return;
               end case;
               case Atype.Kind is
                  when Ghdl_Rtik_Subtype_Array =>
                     Rti_To_Handle
                       (To_Ghdl_Rtin_Subtype_Composite_Acc (Atype).Basetype,
                        Ref.Ctxt, Res);
                     if Res.Kind /= VhpiUndefined then
                        Error := AvhpiErrorOk;
                     end if;
                  when Ghdl_Rtik_Subtype_Scalar =>
                     Rti_To_Handle
                       (To_Ghdl_Rtin_Subtype_Scalar_Acc (Atype).Basetype,
                        Ref.Ctxt, Res);
                     if Res.Kind /= VhpiUndefined then
                        Error := AvhpiErrorOk;
                     end if;
                  when Ghdl_Rtik_Type_Array =>
                     Res := Ref;
                     Error := AvhpiErrorOk;
                  when others =>
                     return;
               end case;
            end;

         when VhpiElemSubtype =>
            declare
               Base_Type : Ghdl_Rti_Access;
            begin
               case Ref.Atype.Kind is
                  when Ghdl_Rtik_Subtype_Array =>
                     Base_Type :=
                       To_Ghdl_Rtin_Subtype_Composite_Acc (Ref.Atype).Basetype;
                  when Ghdl_Rtik_Type_Array =>
                     Base_Type := Ref.Atype;
                  when others =>
                     return;
               end case;
               Rti_To_Handle (To_Ghdl_Rtin_Type_Array_Acc (Base_Type).Element,
                              Ref.Ctxt, Res);
               if Res.Kind /= VhpiUndefined then
                  Error := AvhpiErrorOk;
               end if;
            end;

         when VhpiBaseName =>
            case Ref.Kind is
               when VhpiIndexedNameK =>
                  Rti_To_Handle
                    (To_Ghdl_Rti_Access (Ref.N_Obj), Ref.Ctxt, Res);
                  if Res.Kind /= VhpiUndefined then
                     Error := AvhpiErrorOk;
                  end if;
               when others =>
                  null;
            end case;

         when others =>
            null;
      end case;
   end Vhpi_Handle;

   procedure Constraints_By_Index (Ref : VhpiHandleT;
                                   Index : Natural;
                                   Res : out VhpiHandleT;
                                   Error : out AvhpiErrorT) is
   begin
      --  Default error.
      Error := AvhpiErrorNotImplemented;

      case Ref.Kind is
         when VhpiSubtypeIndicK =>
            if Ref.Atype.Kind = Ghdl_Rtik_Subtype_Array then
               declare
                  Arr_Subtype : constant Ghdl_Rtin_Subtype_Composite_Acc
                    := To_Ghdl_Rtin_Subtype_Composite_Acc (Ref.Atype);
                  Basetype : constant Ghdl_Rtin_Type_Array_Acc :=
                    To_Ghdl_Rtin_Type_Array_Acc (Arr_Subtype.Basetype);
                  Idx : constant Ghdl_Index_Type := Ghdl_Index_Type (Index);
                  Layout : Address;
                  Bounds : Ghdl_Range_Array (0 .. Basetype.Nbr_Dim - 1);
                  Range_Basetype : Ghdl_Rti_Access;
               begin
                  if Idx not in 1 .. Basetype.Nbr_Dim then
                     Res := Null_Handle;
                     Error := AvhpiErrorBadIndex;
                     return;
                  end if;
                  --  constraint type is basetype.indexes (idx - 1)
                  Layout := Loc_To_Addr (Arr_Subtype.Common.Depth,
                                         Arr_Subtype.Layout, Ref.Ctxt);
                  Bound_To_Range
                    (Array_Layout_To_Bounds (Layout), Basetype, Bounds);
                  Res := (Kind => VhpiIntRangeK,
                          Ctxt => Ref.Ctxt,
                          Rng_Type => Basetype.Indexes (Idx - 1),
                          Rng_Addr => Bounds (Idx - 1));
                  Range_Basetype := Get_Base_Type (Res.Rng_Type);
                  case Range_Basetype.Kind is
                     when Ghdl_Rtik_Type_I32 =>
                        null;
                     when Ghdl_Rtik_Type_E8
                       | Ghdl_Rtik_Type_E32 =>
                        Res := (Kind => VhpiEnumRangeK,
                                Ctxt => Ref.Ctxt,
                                Rng_Type => Res.Rng_Type,
                                Rng_Addr => Res.Rng_Addr);
                     when others =>
                        Internal_Error ("vhpi_handle_by_index/constraint");
                  end case;
                  Error := AvhpiErrorOk;
               end;
            end if;
         when others =>
            return;
      end case;
   end Constraints_By_Index;

   procedure Indexed_Names_By_Index (Ref : VhpiHandleT;
                                     Index : Natural;
                                     Res : out VhpiHandleT;
                                     Error : out AvhpiErrorT)
   is
      Base_Type, El_Type : VhpiHandleT;
   begin
      --  Default error.
      Error := AvhpiErrorNotImplemented;

      Vhpi_Handle (VhpiBaseType, Ref, Base_Type, Error);
      if Error /= AvhpiErrorOk then
         return;
      end if;
      if Vhpi_Get_Kind (Base_Type) /= VhpiArrayTypeDeclK then
         Error := AvhpiErrorBadRel;
         return;
      end if;
      Vhpi_Handle (VhpiElemSubtype, Base_Type, El_Type, Error);
      if Error /= AvhpiErrorOk then
         return;
      end if;
      Res := (Kind => VhpiIndexedNameK,
              Ctxt => Ref.Ctxt,
              N_Addr => Avhpi_Get_Address (Ref),
              N_Type => El_Type.Atype,
              N_Idx => Ghdl_Index_Type (Index),
              N_Obj => Ref.Obj);
      if Res.N_Addr = Null_Address then
         Error := AvhpiErrorBadRel;
         return;
      end if;
      --  Note: the index is a flat index (ie an offset).
      --  TODO: check with length ?
      Res.N_Addr := Add_Index (Res.Ctxt, Res.N_Addr, Res.N_Obj, Res.N_Type,
                               Ghdl_Index_Type (Index));
   end Indexed_Names_By_Index;

   procedure Vhpi_Handle_By_Index (Rel : VhpiOneToManyT;
                                   Ref : VhpiHandleT;
                                   Index : Natural;
                                   Res : out VhpiHandleT;
                                   Error : out AvhpiErrorT) is
   begin
      case Rel is
         when VhpiConstraints =>
            Constraints_By_Index (Ref, Index, Res, Error);

         when VhpiIndexedNames =>
            Indexed_Names_By_Index (Ref, Index, Res, Error);

         when others =>
            Res := Null_Handle;
            Error := AvhpiErrorNotImplemented;
      end case;
   end Vhpi_Handle_By_Index;

   procedure Vhpi_Handle_By_Array_Index (Ref : VhpiHandleT;
                                         Index : VhpiIntT;
                                         Res : out VhpiHandleT;
                                         Error : out AvhpiErrorT)
   is
      Typ : VhpiHandleT;
      Rng : VhpiHandleT;
      Is_Up : VhpiIntT;
      Left, Right : VhpiIntT;
      Off : Natural;
   begin
      --  Get object subtype.
      Vhpi_Handle (VhpiSubtype, Ref, Typ, Error);
      if Error /= AvhpiErrorOk then
         return;
      end if;

      --  Get the range
      Vhpi_Handle_By_Index (VhpiConstraints, Typ, 1, Rng, Error);
      if Error /= AvhpiErrorOk then
         return;
      end if;

      --  Get the range bounds
      Vhpi_Get (VhpiIsUpP, Rng, Is_Up, Error);
      if Error /= AvhpiErrorOk then
         return;
      end if;
      Vhpi_Get (VhpiLeftBoundP, Rng, Left, Error);
      if Error /= AvhpiErrorOk then
         return;
      end if;
      Vhpi_Get (VhpiRightBoundP, Rng, Right, Error);
      if Error /= AvhpiErrorOk then
         return;
      end if;

      --  Compute the offset
      if Is_Up /= 0 then
         if Index < Left or Index > Right then
            Error := AvhpiErrorBadIndex;
            return;
         end if;
         Off := Natural (Index - Left);
      else
         if Index > Left or Index < Right then
            Error := AvhpiErrorBadIndex;
            return;
         end if;
         Off := Natural (Left - Index);
      end if;

      Indexed_Names_By_Index (Ref, Off, Res, Error);
   end Vhpi_Handle_By_Array_Index;

   procedure Vhpi_Get (Property : VhpiIntPropertyT;
                       Obj : VhpiHandleT;
                       Res : out VhpiIntT;
                       Error : out AvhpiErrorT)
   is
   begin
      --  Default error.
      Error := AvhpiErrorNotImplemented;

      case Property is
         when VhpiIsUpP =>
            if Obj.Kind /= VhpiIntRangeK then
               Res := 0;
               Error := AvhpiErrorBadRel;
               return;
            end if;
            Error := AvhpiErrorOk;
            case Get_Base_Type (Obj.Rng_Type).Kind is
               when Ghdl_Rtik_Type_I32 =>
                  Res := Boolean'Pos (Obj.Rng_Addr.I32.Dir = Dir_To);
               when others =>
                  null;
            end case;

         when VhpiLeftBoundP =>
            if Obj.Kind /= VhpiIntRangeK then
               Res := 0;
               Error := AvhpiErrorBadRel;
               return;
            end if;
            Error := AvhpiErrorOk;
            case Get_Base_Type (Obj.Rng_Type).Kind is
               when Ghdl_Rtik_Type_I32 =>
                  Res := Obj.Rng_Addr.I32.Left;
               when others =>
                  null;
            end case;

         when VhpiRightBoundP =>
            if Obj.Kind /= VhpiIntRangeK then
               Error := AvhpiErrorBadRel;
               return;
            end if;
            Error := AvhpiErrorOk;
            case Get_Base_Type (Obj.Rng_Type).Kind is
               when Ghdl_Rtik_Type_I32 =>
                  Res := Obj.Rng_Addr.I32.Right;
               when others =>
                  null;
            end case;

         when VhpiLineNoP =>
            declare
               Linecol : Ghdl_Index_Type;
            begin
               case Obj.Kind is
                  when VhpiSigDeclK
                    | VhpiPortDeclK
                    | VhpiGenericDeclK
                    | VhpiConstDeclK =>
                     --  Objects.
                     Linecol := Obj.Obj.Linecol;
                  when VhpiPackInstK
                    | VhpiArchBodyK
                    | VhpiEntityDeclK
                    | VhpiProcessStmtK
                    | VhpiBlockStmtK
                    | VhpiIfGenerateK =>
                     --  Blocks.
                     Linecol :=
                       To_Ghdl_Rtin_Block_Acc (Obj.Ctxt.Block).Linecol;
                  when VhpiCompInstStmtK =>
                     Linecol := Obj.Inst.Linecol;
                  when others =>
                     return;
               end case;
               Res := VhpiIntT (Linecol / 256);
               Error := AvhpiErrorOk;
            end;

         when others =>
            null;
      end case;
   end Vhpi_Get;

   procedure Vhpi_Get (Property : VhpiIntPropertyT;
                       Obj : VhpiHandleT;
                       Res : out Boolean;
                       Error : out AvhpiErrorT)
   is
   begin
      case Property is
         when VhpiIsUpP =>
            if Obj.Kind /= VhpiIntRangeK then
               Res := False;
               Error := AvhpiErrorBadRel;
               return;
            end if;
            Error := AvhpiErrorOk;
            case Get_Base_Type (Obj.Rng_Type).Kind is
               when Ghdl_Rtik_Type_I32 =>
                  Res := Obj.Rng_Addr.I32.Dir = Dir_To;
               when others =>
                  Error := AvhpiErrorNotImplemented;
            end case;
            return;
         when others =>
            Error := AvhpiErrorNotImplemented;
      end case;
   end Vhpi_Get;

   function Vhpi_Get_EntityClass (Obj : VhpiHandleT)
                                 return VhpiEntityClassT
   is
   begin
      case Obj.Kind is
         when VhpiArchBodyK =>
            return VhpiArchitectureEC;
         when others =>
            return VhpiErrorEC;
      end case;
   end Vhpi_Get_EntityClass;

   function Vhpi_Get_Kind (Obj : VhpiHandleT) return VhpiClassKindT is
   begin
      return Obj.Kind;
   end Vhpi_Get_Kind;

   function Vhpi_Get_Mode (Obj : VhpiHandleT) return VhpiModeT is
   begin
      case Obj.Kind is
         when VhpiPortDeclK =>
            case Obj.Obj.Common.Mode and Ghdl_Rti_Signal_Mode_Mask is
               when Ghdl_Rti_Signal_Mode_In =>
                  return VhpiInMode;
               when Ghdl_Rti_Signal_Mode_Out =>
                  return VhpiOutMode;
               when Ghdl_Rti_Signal_Mode_Inout =>
                  return VhpiInoutMode;
               when Ghdl_Rti_Signal_Mode_Buffer =>
                  return VhpiBufferMode;
               when Ghdl_Rti_Signal_Mode_Linkage =>
                  return VhpiLinkageMode;
               when others =>
                  return VhpiErrorMode;
            end case;
         when others =>
            return VhpiErrorMode;
      end case;
   end Vhpi_Get_Mode;

   function Avhpi_Get_Rti (Obj : VhpiHandleT) return Ghdl_Rti_Access is
   begin
      case Obj.Kind is
         when VhpiSubtypeIndicK
           | VhpiEnumTypeDeclK =>
            return Obj.Atype;
         when VhpiSigDeclK
           | VhpiPortDeclK
           | VhpiGenericDeclK
           | VhpiConstDeclK =>
            return To_Ghdl_Rti_Access (Obj.Obj);
         when others =>
            return null;
      end case;
   end Avhpi_Get_Rti;

   function Avhpi_Get_Address (Obj : VhpiHandleT) return Address is
   begin
      case Obj.Kind is
         when VhpiPortDeclK
           | VhpiSigDeclK
           | VhpiGenericDeclK
           | VhpiConstDeclK =>
            return Loc_To_Addr (Obj.Ctxt.Block.Depth,
                                Obj.Obj.Loc,
                                Obj.Ctxt);
         when VhpiIndexedNameK =>
            return Obj.N_Addr;
         when others =>
            return Null_Address;
      end case;
   end Avhpi_Get_Address;

   function Avhpi_Get_Context (Obj : VhpiHandleT) return Rti_Context is
   begin
      return Obj.Ctxt;
   end Avhpi_Get_Context;

   function Vhpi_Compare_Handles (Hdl1, Hdl2 : VhpiHandleT)
                                 return Boolean
   is
   begin
      if Hdl1.Kind /= Hdl2.Kind then
         return False;
      end if;
      case Hdl1.Kind is
         when VhpiSubtypeIndicK
           | VhpiSubtypeDeclK
           | VhpiArrayTypeDeclK
           | VhpiPhysTypeDeclK
           | VhpiIntTypeDeclK =>
            return Hdl1.Atype = Hdl2.Atype;
         when others =>
            -- FIXME: todo
            Internal_Error ("vhpi_compare_handles");
      end case;
   end Vhpi_Compare_Handles;

   function Vhpi_Put_Value (Obj : VhpiHandleT; Val : Ghdl_I64)
                           return AvhpiErrorT
   is
      Vptr : Ghdl_Value_Ptr;
      Atype : Ghdl_Rti_Access;
   begin
      case Obj.Kind is
         when VhpiIndexedNameK =>
            Vptr := To_Ghdl_Value_Ptr (Obj.N_Addr);
            Atype := Obj.N_Type;
         when VhpiGenericDeclK =>
            --  Putting values for generics is necessary to support SDF
            --  annotations.
            Vptr := To_Ghdl_Value_Ptr (Avhpi_Get_Address (Obj));
            Atype := Obj.Obj.Obj_Type;
         when VhpiConstDeclK =>
            -- Don't support changing values of constants.
            return AvhpiErrorNotImplemented;
         when others =>
            return AvhpiErrorNotImplemented;
      end case;
      case Get_Base_Type (Atype).Kind is
         when Ghdl_Rtik_Type_P64 =>
            null;
         when others =>
            return AvhpiErrorHandle;
      end case;
      Vptr.I64 := Val;
      return AvhpiErrorOk;
   end Vhpi_Put_Value;
end Grt.Avhpi;
