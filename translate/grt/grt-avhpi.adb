--  GHDL Run Time (GRT) - VHPI implementation for Ada.
--  Copyright (C) 2002, 2003, 2004, 2005 Tristan Gingold
--
--  GHDL is free software; you can redistribute it and/or modify it under
--  the terms of the GNU General Public License as published by the Free
--  Software Foundation; either version 2, or (at your option) any later
--  version.
--
--  GHDL is distributed in the hope that it will be useful, but WITHOUT ANY
--  WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
--  for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with GCC; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.
with Grt.Errors; use Grt.Errors;
with Grt.Vstrings; use Grt.Vstrings;
with Grt.Rtis_Utils; use Grt.Rtis_Utils;

package body Grt.Avhpi is
   procedure Get_Root_Inst (Res : out VhpiHandleT)
   is
   begin
      Res := (Kind => VhpiRootInstK,
              Ctxt => Get_Top_Context);
   end Get_Root_Inst;

   procedure Get_Package_Inst (Res : out VhpiHandleT) is
   begin
      Res := (Kind => VhpiIteratorK,
              Ctxt => (Base => Null_Address,
                       Block => To_Ghdl_Rti_Access (Ghdl_Rti_Top_Ptr)),
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
                            Error : out AvhpiErrorT)
   is
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
               when VhpiGenericDeclK =>
                  Res := (Kind => AvhpiNameIteratorK,
                          Ctxt => Ref.Ctxt,
                          N_Addr => Loc_To_Addr (Ref.Obj.Common.Depth,
                                                 Ref.Obj.Loc,
                                                 Ref.Ctxt),
                          N_Type => Ref.Obj.Obj_Type,
                          N_Idx => 0,
                          N_Obj => Ref.Obj);
               when others =>
                  Error := AvhpiErrorNotImplemented;
                  return;
            end case;
            case Res.N_Type.Kind is
               when Ghdl_Rtik_Subtype_Array =>
                  declare
                     St : Ghdl_Rtin_Subtype_Array_Acc :=
                       To_Ghdl_Rtin_Subtype_Array_Acc (Res.N_Type);
                     Bt : Ghdl_Rtin_Type_Array_Acc := St.Basetype;
                     Rngs : Ghdl_Range_Array (0 .. Bt.Nbr_Dim - 1);
                  begin
                     Bound_To_Range
                       (Loc_To_Addr (St.Common.Depth, St.Bounds, Res.Ctxt),
                        Bt, Rngs);
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

   procedure Vhpi_Scan_Indexed_Name (Iterator : in out VhpiHandleT;
                                     Res : out VhpiHandleT;
                                     Error : out AvhpiErrorT)
   is
      procedure Update (S : Ghdl_Index_Type) is
      begin
         Iterator.N_Addr := Iterator.N_Addr + (S / Storage_Unit);
      end Update;

      Is_Sig : Boolean;
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
      case Iterator.N_Obj.Common.Kind is
         when Ghdl_Rtik_Generic =>
            Is_Sig := False;
         when others =>
            Internal_Error ("vhpi_scan_indexed_name(1)");
      end case;

      case Get_Base_Type (El_Type).Kind is
         when Ghdl_Rtik_Type_P64 =>
            if Is_Sig then
               Update (Address'Size);
            else
               Update (Ghdl_I64'Size);
            end if;
         when others =>
            Internal_Error ("vhpi_scan_indexed_name");
      end case;
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
               declare
                  Base : Address;
               begin
                  Base := To_Addr_Acc (Iterator.Ctxt.Base + Nblk.Loc.Off).all;
                  Base := Base + Iterator.It2 * Nblk.Size;
                  Res := (Kind => VhpiForGenerateK,
                          Ctxt => (Base => Base,
                                   Block => Ch));

                  Error := AvhpiErrorOk;
                  return;
               end;
            end if;
         end if;


         Iterator.It_Cur := Iterator.It_Cur + 1;

         case Ch.Kind is
            when Ghdl_Rtik_Process =>
               Res := (Kind => VhpiProcessStmtK,
                       Ctxt => (Base => Iterator.Ctxt.Base + Nblk.Loc.Off,
                                Block => Ch));
               Error := AvhpiErrorOk;
               return;
            when Ghdl_Rtik_Block =>
               Res := (Kind => VhpiBlockStmtK,
                       Ctxt => (Base => Iterator.Ctxt.Base + Nblk.Loc.Off,
                                Block => Ch));
               Error := AvhpiErrorOk;
               return;
            when Ghdl_Rtik_If_Generate =>
               Res := (Kind => VhpiIfGenerateK,
                       Ctxt => (Base => To_Addr_Acc (Iterator.Ctxt.Base
                                                     + Nblk.Loc.Off).all,
                                Block => Ch));
               --  Return only if the condition is true.
               if Res.Ctxt.Base /= Null_Address then
                  Error := AvhpiErrorOk;
                  return;
               end if;
            when Ghdl_Rtik_For_Generate =>
               Res := (Kind => VhpiForGenerateK,
                       Ctxt => (Base => To_Addr_Acc (Iterator.Ctxt.Base
                                                     + Nblk.Loc.Off).all,
                                Block => Ch));
               Iterator.Max2 := Get_For_Generate_Length (Nblk, Iterator.Ctxt);
               Iterator.It2 := 0;
               if Iterator.Max2 > 0 then
                  Iterator.It_Cur := Iterator.It_Cur - 1;
                  Error := AvhpiErrorOk;
                  return;
               end if;
               --  If the iterator range is nul, then continue to scan.
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
         when Ghdl_Rtik_Subtype_Array
           | Ghdl_Rtik_Subtype_Array_Ptr =>
            declare
               Atype : Ghdl_Rtin_Subtype_Array_Acc;
               Bt : Ghdl_Rtin_Type_Array_Acc;
            begin
               Atype := To_Ghdl_Rtin_Subtype_Array_Acc (Rti);
               Bt := Atype.Basetype;
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
         when Ghdl_Rtik_Type_B2
           | Ghdl_Rtik_Type_E8
	   | Ghdl_Rtik_Type_E32 =>
            Res := (Kind => VhpiEnumTypeDeclK,
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
      Obj : Ghdl_Rtin_Object_Acc;
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
            Obj := To_Ghdl_Rtin_Object_Acc (Ch);

            Iterator.It_Cur := Iterator.It_Cur + 1;

            case Ch.Kind is
               when Ghdl_Rtik_Port
                 | Ghdl_Rtik_Generic
                 | Ghdl_Rtik_Signal
                 | Ghdl_Rtik_Subtype_Array
                 | Ghdl_Rtik_Subtype_Array_Ptr
                 | Ghdl_Rtik_Type_E8
                 | Ghdl_Rtik_Type_E32
                 | Ghdl_Rtik_Type_B2 =>
                  Rti_To_Handle (Ch, Iterator.Ctxt, Res);
                  if Res.Kind /= VhpiUndefined then
                     Error := AvhpiErrorOk;
                     return;
                  else
                     Internal_Error ("vhpi_handle");
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

   procedure Vhpi_Scan (Iterator : in out VhpiHandleT;
                        Res : out VhpiHandleT;
                        Error : out AvhpiErrorT)
   is
   begin
      if Iterator.Kind = AvhpiNameIteratorK then
         case Iterator.N_Type.Kind is
            when Ghdl_Rtik_Subtype_Array =>
               Vhpi_Scan_Indexed_Name (Iterator, Res, Error);
            when others =>
               Error := AvhpiErrorHandle;
               Res := Null_Handle;
         end case;
         return;
      elsif Iterator.Kind /= VhpiIteratorK then
         Error := AvhpiErrorHandle;
         Res := Null_Handle;
         return;
      end if;

      case Iterator.Rel is
         when VhpiPackInsts =>
            declare
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
            end;
         when VhpiInternalRegions =>
            Vhpi_Scan_Internal_Regions (Iterator, Res, Error);
         when VhpiDecls =>
            Vhpi_Scan_Decls (Iterator, Res, Error);
         when others =>
            Res := Null_Handle;
            Error := AvhpiErrorNotImplemented;
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
           | VhpiBlockStmtK
           | VhpiIfGenerateK
           | VhpiForGenerateK =>
            return To_Ghdl_Rtin_Block_Acc (Obj.Ctxt.Block).Name;
         when VhpiRootInstK =>
            declare
               Blk : Ghdl_Rtin_Block_Acc;
            begin
               Blk := To_Ghdl_Rtin_Block_Acc (Obj.Ctxt.Block);
               Blk := To_Ghdl_Rtin_Block_Acc (Blk.Parent);
               return Blk.Name;
            end;
         when VhpiCompInstStmtK =>
            return Obj.Inst.Name;
         when VhpiSigDeclK
           | VhpiPortDeclK
           | VhpiGenericDeclK =>
            return Obj.Obj.Name;
         when VhpiSubtypeDeclK =>
            return To_Ghdl_Rtin_Subtype_Scalar_Acc (Obj.Atype).Name;
         when others =>
            return null;
      end case;
   end Avhpi_Get_Base_Name;

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
         for I in Str'Range loop
            exit when Str (I) = NUL;
            Add (Str (I));
         end loop;
      end Add;
   begin
      Len := 0;

      case Property is
         when VhpiNameP =>
            case Obj.Kind is
               when VhpiEnumTypeDeclK =>
                  Add (To_Ghdl_Rtin_Type_Enum_Acc (Obj.Atype).Name);
               when VhpiPackInstK
                 | VhpiArchBodyK
                 | VhpiEntityDeclK
                 | VhpiProcessStmtK
                 | VhpiBlockStmtK
                 | VhpiIfGenerateK =>
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
                 | VhpiGenericDeclK =>
                  Add (Obj.Obj.Name);
               when VhpiSubtypeDeclK =>
                  Add (To_Ghdl_Rtin_Subtype_Scalar_Acc (Obj.Atype).Name);
               when VhpiForGenerateK =>
                  declare
                     Blk : Ghdl_Rtin_Block_Acc;
                     Iter : Ghdl_Rtin_Object_Acc;
                     Iter_Type : Ghdl_Rti_Access;
                     Vptr : Ghdl_Value_Ptr;
                     Buf : String (1 .. 12);
                     Buf_Len : Natural;
                  begin
                     Blk := To_Ghdl_Rtin_Block_Acc (Obj.Ctxt.Block);
                     Iter := To_Ghdl_Rtin_Object_Acc (Blk.Children (0));
                     Vptr := To_Ghdl_Value_Ptr
                       (Loc_To_Addr (Iter.Common.Depth, Iter.Loc, Obj.Ctxt));
                     Add (Blk.Name);
                     Add ('(');
                     Iter_Type := Iter.Obj_Type;
                     if Iter_Type.Kind = Ghdl_Rtik_Subtype_Scalar then
                        Iter_Type := To_Ghdl_Rtin_Subtype_Scalar_Acc
                          (Iter_Type).Basetype;
                     end if;
                     case Iter_Type.Kind is
                        when Ghdl_Rtik_Type_I32 =>
                           To_String (Buf, Buf_Len, Vptr.I32);
                           Add (Buf (Buf_Len .. Buf'Last));
--                         when Ghdl_Rtik_Type_E8 =>
--                            Disp_Enum_Value
--                              (Stream, Rti, Ghdl_Index_Type (Vptr.E8));
--                         when Ghdl_Rtik_Type_E32 =>
--                            Disp_Enum_Value
--                              (Stream, Rti, Ghdl_Index_Type (Vptr.E32));
--                         when Ghdl_Rtik_Type_B2 =>
--                            Disp_Enum_Value
--                              (Stream, Rti,
--                               Ghdl_Index_Type (Ghdl_B2'Pos (Vptr.B2)));
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
                     Comp := To_Ghdl_Rtin_Component_Acc (Obj.Obj.Obj_Type);
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
                             Ctxt => (Base => Ref.Ctxt.Base + Ent.Loc.Off,
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
                     Blk : Ghdl_Rtin_Block_Acc;
                     Iter : Ghdl_Rtin_Object_Acc;
                  begin
                     Blk := To_Ghdl_Rtin_Block_Acc (Ref.Ctxt.Block);
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
         when others =>
            Res := Null_Handle;
            Error := AvhpiErrorNotImplemented;
      end case;
   end Vhpi_Handle;

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

   function Vhpi_Get_Mode (Obj : VhpiHandleT) return VhpiModeP is
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
           | VhpiPortDeclK =>
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
           | VhpiArrayTypeDeclK =>
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


