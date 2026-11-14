--  GHDL Run Time (GRT) - External names.
--  Copyright (C) 2026 Tristan Gingold
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
with Grt.Vhdl_Types; use Grt.Vhdl_Types;
with Grt.Rtis_Addr; use Grt.Rtis_Addr;
with Grt.Rtis_Utils; use Grt.Rtis_Utils;
with Grt.Signals; use Grt.Signals;

package body Grt.External_Names is
   --  Values for Step.
   --  The next pathname element is the root entity (absolute pathname).
   Step_Root : constant Ghdl_I32 := 0;
   --  Base/Block is a declarative region.
   Step_Region : constant Ghdl_I32 := 1;
   --  The next pathname element is the library (package pathname).
   Step_Library : constant Ghdl_I32 := 2;
   --  The next pathname element is the package (package pathname).
   Step_Package : constant Ghdl_I32 := 3;
   --  The resolution of a soft external name has failed.
   Step_Failed : constant Ghdl_I32 := 4;

   --  Set when the resolution of a soft external name has failed.
   Soft_Failed : Boolean := False;

   function Ghdl_Extname_Failed return Ghdl_Index_Type is
   begin
      if Soft_Failed then
         return 1;
      else
         return 0;
      end if;
   end Ghdl_Extname_Failed;

   --  Called when a pathname element or the object is not yet elaborated.
   --  Return True if the resolution is abandoned (because of soft mode),
   --  False if this is an error.
   function Soft_Fail (St : Ghdl_Extname_State_Acc; Is_Component : Boolean)
                      return Boolean is
   begin
      if St.Soft = 2 or else (St.Soft = 1 and then Is_Component) then
         St.Step := Step_Failed;
         Soft_Failed := True;
         return True;
      else
         return False;
      end if;
   end Soft_Fail;

   procedure Ghdl_Extname_Defer_Failed (Proc : Proc_Acc; This : Instance_Acc)
   is
   begin
      if Soft_Failed then
         Ghdl_Elab_Defer (Proc, This);
         Soft_Failed := False;
      end if;
   end Ghdl_Extname_Defer_Failed;

   procedure Error_Start (St : Ghdl_Extname_State_Acc) is
   begin
      Error_S ("external name at ");
      if St.Filename = null then
         Diag_C ("*unknown*");
      else
         Diag_C (St.Filename);
      end if;
      Diag_C (':');
      Diag_C (St.Line);
      Diag_C (": ");
   end Error_Start;

   procedure Error_Name (St : Ghdl_Extname_State_Acc;
                         Msg1 : String;
                         Name : Ghdl_C_String;
                         Msg2 : String);
   pragma No_Return (Error_Name);

   procedure Error_Name (St : Ghdl_Extname_State_Acc;
                         Msg1 : String;
                         Name : Ghdl_C_String;
                         Msg2 : String) is
   begin
      Error_Start (St);
      Diag_C (Msg1);
      Diag_C ('"');
      Diag_C (Name);
      Diag_C ('"');
      Error_E (Msg2);
   end Error_Name;

   procedure Error_Msg (St : Ghdl_Extname_State_Acc; Msg : String);
   pragma No_Return (Error_Msg);

   procedure Error_Msg (St : Ghdl_Extname_State_Acc; Msg : String) is
   begin
      Error_Start (St);
      Error_E (Msg);
   end Error_Msg;

   function Is_Name (L, R : Ghdl_C_String) return Boolean is
   begin
      return L /= null and then R /= null and then strcmp (L, R) = 0;
   end Is_Name;

   function Get_Block_Name (Rti : Ghdl_Rti_Access) return Ghdl_C_String is
   begin
      return To_Ghdl_Rtin_Block_Acc (Rti).Name;
   end Get_Block_Name;

   --  Move CTXT to the innermost concurrent region that contains it.
   function Get_Concurrent_Region (Ctxt : Rti_Context) return Rti_Context
   is
      Res : Rti_Context;
   begin
      Res := Ctxt;
      loop
         case Res.Block.Kind is
            when Ghdl_Rtik_Architecture
              | Ghdl_Rtik_Block
              | Ghdl_Rtik_Generate_Body =>
               return Res;
            when Ghdl_Rtik_Process
              | Ghdl_Rtiks_Psl =>
               Res := Get_Parent_Context (Res);
            when others =>
               return Null_Context;
         end case;
      end loop;
   end Get_Concurrent_Region;

   procedure Ghdl_Extname_Start (St : Ghdl_Extname_State_Acc;
                                 Kind : Ghdl_Index_Type;
                                 Ctxt_Rti : Ghdl_Rti_Access;
                                 Ctxt_Base : Address;
                                 Filename : Ghdl_C_String;
                                 Line : Ghdl_I32;
                                 Soft : Ghdl_Index_Type) is
   begin
      St.all := (Base => Null_Address,
                 Block => null,
                 Lib => null,
                 Filename => Filename,
                 Line => Line,
                 Step => Step_Region,
                 Soft => Ghdl_I32 (Soft));
      case Kind is
         when Extname_Absolute =>
            St.Step := Step_Root;
         when Extname_Relative =>
            declare
               Ctxt : Rti_Context;
            begin
               if Ctxt_Rti = null then
                  Ctxt := Null_Context;
               else
                  Ctxt := Get_Concurrent_Region ((Base => Ctxt_Base,
                                                  Block => Ctxt_Rti));
               end if;
               if Ctxt.Block = null then
                  Error_Msg (St, "not within a concurrent region");
               end if;
               St.Base := Ctxt.Base;
               St.Block := Ctxt.Block;
            end;
         when Extname_Package =>
            St.Step := Step_Library;
         when others =>
            Error_Msg (St, "bad pathname kind");
      end case;
   end Ghdl_Extname_Start;

   procedure Ghdl_Extname_Up (St : Ghdl_Extname_State_Acc)
   is
      Ctxt : Rti_Context;
   begin
      if St.Step = Step_Failed then
         return;
      end if;
      pragma Assert (St.Step = Step_Region);
      Ctxt := (Base => St.Base, Block => St.Block);
      case Ctxt.Block.Kind is
         when Ghdl_Rtik_Architecture =>
            --  Go to the region of the instantiation statement.
            declare
               Entity_Ctxt : Rti_Context;
               Link : Ghdl_Entity_Link_Acc;
               Stmt : Ghdl_Rti_Access;
            begin
               Entity_Ctxt := Get_Parent_Context (Ctxt);
               Link := To_Ghdl_Entity_Link_Acc (Entity_Ctxt.Base);
               Get_Instance_Link (Link, Ctxt, Stmt);
               if Stmt = null then
                  Error_Msg (St, "path already at top of hierarchy");
               end if;
            end;
         when Ghdl_Rtik_Block
           | Ghdl_Rtik_Generate_Body =>
            Ctxt := Get_Parent_Context (Ctxt);
         when others =>
            Error_Msg (St, "path already at top of hierarchy");
      end case;
      Ctxt := Get_Concurrent_Region (Ctxt);
      if Ctxt.Block = null then
         Error_Msg (St, "path already at top of hierarchy");
      end if;
      St.Base := Ctxt.Base;
      St.Block := Ctxt.Block;
   end Ghdl_Extname_Up;

   --  Read the value of the generate parameter of generate body CTXT.
   function Read_Generate_Parameter (Ctxt : Rti_Context; Ok : out Boolean)
                                    return Ghdl_I64
   is
      Bod : constant Ghdl_Rtin_Block_Acc :=
        To_Ghdl_Rtin_Block_Acc (Ctxt.Block);
      Iter : constant Ghdl_Rtin_Object_Acc :=
        To_Ghdl_Rtin_Object_Acc (Bod.Children (0));
      Addr : constant Address :=
        Loc_To_Addr (Iter.Common.Depth, Iter.Loc, Ctxt);
   begin
      Ok := True;
      case Get_Base_Type (Iter.Obj_Type).Kind is
         when Ghdl_Rtik_Type_B1 =>
            declare
               V : Ghdl_B1;
               for V'Address use Addr;
               pragma Import (Ada, V);
            begin
               return Ghdl_B1'Pos (V);
            end;
         when Ghdl_Rtik_Type_E8 =>
            declare
               V : Ghdl_E8;
               for V'Address use Addr;
               pragma Import (Ada, V);
            begin
               return Ghdl_I64 (V);
            end;
         when Ghdl_Rtik_Type_E32 =>
            declare
               V : Ghdl_E32;
               for V'Address use Addr;
               pragma Import (Ada, V);
            begin
               return Ghdl_I64 (V);
            end;
         when Ghdl_Rtik_Type_I32 =>
            declare
               V : Ghdl_I32;
               for V'Address use Addr;
               pragma Import (Ada, V);
            begin
               return Ghdl_I64 (V);
            end;
         when Ghdl_Rtik_Type_I64 =>
            declare
               V : Ghdl_I64;
               for V'Address use Addr;
               pragma Import (Ada, V);
            begin
               return V;
            end;
         when others =>
            Ok := False;
            return 0;
      end case;
   end Read_Generate_Parameter;

   procedure Ghdl_Extname_Elem (St : Ghdl_Extname_State_Acc;
                                Name : Ghdl_C_String;
                                Has_Index : Ghdl_Index_Type;
                                Index : Ghdl_I64)
   is
      Blk : Ghdl_Rtin_Block_Acc;
      Ctxt : Rti_Context;
      Child : Ghdl_Rti_Access;
   begin
      case St.Step is
         when Step_Failed =>
            return;
         when Step_Root =>
            --  LRM08 8.7 External names
            --  For an absolute pathname, the root declarative region
            --  encloses the root design entity.
            Ctxt := Get_Top_Context;
            if not Is_Name
              (Get_Block_Name (To_Ghdl_Rtin_Block_Acc (Ctxt.Block).Parent),
               Name)
            then
               Error_Name (St, "", Name, " is not the root of the design");
            end if;
            St.Base := Ctxt.Base;
            St.Block := Ctxt.Block;
            St.Step := Step_Region;
            return;
         when Step_Library =>
            St.Lib := Name;
            St.Step := Step_Package;
            return;
         when Step_Package =>
            for I in 1 .. Ghdl_Rti_Top.Nbr_Child loop
               Child := Ghdl_Rti_Top.Children (I - 1);
               if Child.Kind = Ghdl_Rtik_Package
                 and then Is_Name (Get_Block_Name (Child), Name)
                 and then Is_Name
                   (To_Ghdl_Rtin_Type_Scalar_Acc
                      (To_Ghdl_Rtin_Block_Acc (Child).Parent).Name,
                    St.Lib)
               then
                  St.Base := Null_Address;
                  St.Block := Child;
                  St.Step := Step_Region;
                  return;
               end if;
            end loop;
            Error_Start (St);
            Diag_C ("cannot find package """);
            Diag_C (St.Lib);
            Diag_C ('.');
            Diag_C (Name);
            Error_E (""" in the design");
            return;
         when others =>
            null;
      end case;

      Ctxt := (Base => St.Base, Block => St.Block);
      case Ctxt.Block.Kind is
         when Ghdl_Rtik_Architecture
           | Ghdl_Rtik_Block
           | Ghdl_Rtik_Generate_Body =>
            null;
         when others =>
            Error_Name
              (St, "cannot find ", Name, " (not a concurrent region)");
      end case;

      Blk := To_Ghdl_Rtin_Block_Acc (Ctxt.Block);
      for I in 1 .. Blk.Nbr_Child loop
         Child := Blk.Children (I - 1);
         case Child.Kind is
            when Ghdl_Rtik_Block
              | Ghdl_Rtik_Instance
              | Ghdl_Rtik_For_Generate
              | Ghdl_Rtik_If_Generate
              | Ghdl_Rtik_Case_Generate =>
               if Is_Name (Get_Block_Name (Child), Name) then
                  if Has_Index /= 0
                    and then Child.Kind /= Ghdl_Rtik_For_Generate
                  then
                     Error_Name
                       (St, "index expression valid only for a for-generate"
                          & " statement (", Name, ")");
                  end if;

                  case Child.Kind is
                     when Ghdl_Rtik_Block =>
                        St.Base :=
                          Ctxt.Base + To_Ghdl_Rtin_Block_Acc (Child).Loc;
                        St.Block := Child;
                     when Ghdl_Rtik_Instance =>
                        declare
                           Sub : Rti_Context;
                        begin
                           Get_Instance_Context
                             (To_Ghdl_Rtin_Instance_Acc (Child), Ctxt, Sub);
                           if Sub.Base = Null_Address
                             or else Sub.Block = null
                           then
                              if Soft_Fail
                                (St, To_Ghdl_Rtin_Instance_Acc
                                   (Child).Instance.Kind = Ghdl_Rtik_Component)
                              then
                                 --  Retry at the end of the elaboration.
                                 return;
                              end if;
                              Error_Name
                                (St, "", Name, " is not yet elaborated");
                           end if;
                           if Sub.Block.Kind /= Ghdl_Rtik_Architecture then
                              Error_Name (St, "", Name,
                                          " does not denote a VHDL entity");
                           end if;
                           St.Base := Sub.Base;
                           St.Block := Sub.Block;
                        end;
                     when Ghdl_Rtik_For_Generate =>
                        declare
                           Gen : constant Ghdl_Rtin_Generate_Acc :=
                             To_Ghdl_Rtin_Generate_Acc (Child);
                           Bod : Rti_Context;
                           Len : Ghdl_Index_Type;
                           V : Ghdl_I64;
                           Ok : Boolean;
                        begin
                           if Has_Index = 0 then
                              Error_Name
                                (St, "expression required for for-generate"
                                   & " statement ", Name, "");
                           end if;
                           Bod :=
                             (Base => To_Addr_Acc (Ctxt.Base + Gen.Loc).all,
                              Block => Gen.Child);
                           if Bod.Base = Null_Address then
                              if Soft_Fail (St, False) then
                                 return;
                              end if;
                              Error_Name
                                (St, "", Name, " is not yet elaborated");
                           end if;
                           Len := Get_For_Generate_Length (Gen, Ctxt);
                           for J in 1 .. Len loop
                              V := Read_Generate_Parameter (Bod, Ok);
                              if not Ok then
                                 Error_Name
                                   (St, "unhandled type of parameter for ",
                                    Name, "");
                              end if;
                              if V = Index then
                                 St.Base := Bod.Base;
                                 St.Block := Bod.Block;
                                 return;
                              end if;
                              Bod.Base := Bod.Base + Gen.Size;
                           end loop;
                           Error_Name (St, "expression not in the range of"
                                         & " the parameter of ", Name, "");
                        end;
                     when Ghdl_Rtik_If_Generate
                       | Ghdl_Rtik_Case_Generate =>
                        declare
                           Sub : Rti_Context;
                        begin
                           Sub := Get_If_Case_Generate_Child (Ctxt, Child);
                           if Sub.Base = Null_Address then
                              if Soft_Fail (St, False) then
                                 return;
                              end if;
                              Error_Name (St, "no block generated for ",
                                          Name, " (or not yet elaborated)");
                           end if;
                           St.Base := Sub.Base;
                           St.Block := Sub.Block;
                        end;
                     when others =>
                        raise Program_Error;
                  end case;
                  return;
               end if;
            when others =>
               null;
         end case;
      end loop;

      Error_Name (St, "cannot find ", Name, "");
   end Ghdl_Extname_Elem;

   --  Return the base type of RTI, or null if not handled.
   function Get_Base_Type_Or_Null (Rti : Ghdl_Rti_Access)
                                  return Ghdl_Rti_Access is
   begin
      case Rti.Kind is
         when Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32
           | Ghdl_Rtik_Type_B1
           | Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_I64
           | Ghdl_Rtik_Type_P32
           | Ghdl_Rtik_Type_P64
           | Ghdl_Rtik_Type_F64
           | Ghdl_Rtik_Subtype_Scalar
           | Ghdl_Rtik_Type_Array
           | Ghdl_Rtik_Type_Record
           | Ghdl_Rtik_Type_Unbounded_Record
           | Ghdl_Rtik_Subtype_Array
           | Ghdl_Rtik_Subtype_Unbounded_Array
           | Ghdl_Rtik_Subtype_Record
           | Ghdl_Rtik_Subtype_Unbounded_Record =>
            return Get_Base_Type (Rti);
         when Ghdl_Rtik_Type_Access
           | Ghdl_Rtik_Type_File
           | Ghdl_Rtik_Type_Protected =>
            return Rti;
         when Ghdl_Rtik_Subtype_Access =>
            return To_Ghdl_Rtin_Type_Fileacc_Acc (Rti).Base;
         when others =>
            return null;
      end case;
   end Get_Base_Type_Or_Null;

   --  Return the bounds (for an array) or the layout (for a record) of
   --  subtype RTI, if they are not known from the object.
   function Get_Subtype_Bounds (Rti : Ghdl_Rti_Access; Ctxt : Rti_Context)
                               return Address is
   begin
      case Rti.Kind is
         when Ghdl_Rtik_Subtype_Array =>
            declare
               St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
            begin
               return Array_Layout_To_Bounds
                 (Loc_To_Addr (St.Common.Depth, St.Layout, Ctxt));
            end;
         when Ghdl_Rtik_Subtype_Record =>
            declare
               St : constant Ghdl_Rtin_Subtype_Composite_Acc :=
                 To_Ghdl_Rtin_Subtype_Composite_Acc (Rti);
            begin
               return Loc_To_Addr (St.Common.Depth, St.Layout, Ctxt);
            end;
         when Ghdl_Rtik_Type_Record =>
            declare
               Rec : constant Ghdl_Rtin_Type_Record_Acc :=
                 To_Ghdl_Rtin_Type_Record_Acc (Rti);
            begin
               if Rti_Complex_Type (Rti) then
                  return Loc_To_Addr (Rec.Common.Depth, Rec.Layout, Ctxt);
               else
                  return Null_Address;
               end if;
            end;
         when others =>
            return Null_Address;
      end case;
   end Get_Subtype_Bounds;

   type Scalar_Status is
     (
      --  No scalar subelement (null array or empty record).
      Scalar_None,
      --  Found.
      Scalar_Found,
      --  Layout not handled.
      Scalar_Unhandled
     );

   --  Find the first scalar subelement of a signal whose type is RTI, whose
   --  signals are at SIG_ADDR and bounds (or layout) at BOUNDS.
   --  If STATUS is Scalar_Found, SIG is that signal (or null if not yet
   --  created) and VAL_OFF is the offset of its value.
   procedure Get_First_Scalar_Signal (Rti : Ghdl_Rti_Access;
                                      Ctxt : Rti_Context;
                                      Sig_Addr : Address;
                                      Bounds : Address;
                                      Status : out Scalar_Status;
                                      Sig : out Ghdl_Signal_Ptr;
                                      Val_Off : out Ghdl_Rti_Loc) is
   begin
      Status := Scalar_Unhandled;
      Sig := null;
      Val_Off := 0;
      case Rti.Kind is
         when Ghdl_Rtik_Type_E8
           | Ghdl_Rtik_Type_E32
           | Ghdl_Rtik_Type_B1
           | Ghdl_Rtik_Type_I32
           | Ghdl_Rtik_Type_I64
           | Ghdl_Rtik_Type_P32
           | Ghdl_Rtik_Type_P64
           | Ghdl_Rtik_Type_F64
           | Ghdl_Rtik_Subtype_Scalar =>
            Status := Scalar_Found;
            Sig := To_Ghdl_Signal_Ptr (To_Addr_Acc (Sig_Addr).all);
         when Ghdl_Rtik_Type_Array
           | Ghdl_Rtik_Subtype_Array
           | Ghdl_Rtik_Subtype_Unbounded_Array =>
            declare
               Arr : constant Ghdl_Rtin_Type_Array_Acc :=
                 Get_Base_Array_Type (Rti);
               Rngs : Ghdl_Range_Array (0 .. Arr.Nbr_Dim - 1);
               El_Rti : Ghdl_Rti_Access;
            begin
               if Bounds = Null_Address then
                  return;
               end if;
               Bound_To_Range (Bounds, Arr, Rngs);
               for I in Rngs'Range loop
                  if Rngs (I) = null then
                     return;
                  end if;
                  if Range_To_Length
                    (Rngs (I), Get_Base_Type (Arr.Indexes (I))) = 0
                  then
                     Status := Scalar_None;
                     return;
                  end if;
               end loop;
               --  The first element is at offset 0.
               El_Rti := Arr.Element;
               case El_Rti.Kind is
                  when Ghdl_Rtik_Type_Array
                    | Ghdl_Rtik_Subtype_Unbounded_Array
                    | Ghdl_Rtik_Type_Unbounded_Record
                    | Ghdl_Rtik_Subtype_Unbounded_Record =>
                     --  Not handled.
                     return;
                  when others =>
                     null;
               end case;
               Get_First_Scalar_Signal
                 (El_Rti, Ctxt, Sig_Addr, Get_Subtype_Bounds (El_Rti, Ctxt),
                  Status, Sig, Val_Off);
            end;
         when Ghdl_Rtik_Type_Record
           | Ghdl_Rtik_Type_Unbounded_Record
           | Ghdl_Rtik_Subtype_Record
           | Ghdl_Rtik_Subtype_Unbounded_Record =>
            declare
               Rec : constant Ghdl_Rtin_Type_Record_Acc :=
                 To_Ghdl_Rtin_Type_Record_Acc (Get_Base_Type (Rti));
               El : Ghdl_Rtin_Element_Acc;
               El_Sig : Address;
               El_Val : Address;
               El_Bounds : Address;
               Off : Ghdl_Rti_Loc;
            begin
               if Rec.Nbrel = 0 then
                  Status := Scalar_None;
                  return;
               end if;
               El := To_Ghdl_Rtin_Element_Acc (Rec.Elements (0));
               if El.Common.Mode /= Ghdl_Rti_Element_Static
                 and then Bounds = Null_Address
               then
                  return;
               end if;
               Record_To_Element
                 (Sig_Addr, El, True, Bounds, El_Sig, El_Bounds);
               Record_To_Element
                 (Null_Address, El, False, Bounds, El_Val, El_Bounds);
               if El_Bounds = Null_Address then
                  El_Bounds := Get_Subtype_Bounds (El.Eltype, Ctxt);
               end if;
               Get_First_Scalar_Signal
                 (El.Eltype, Ctxt, El_Sig, El_Bounds, Status, Sig, Off);
               Val_Off := Off + Ghdl_Rti_Loc (To_Integer (El_Val));
            end;
         when others =>
            null;
      end case;
   end Get_First_Scalar_Signal;

   procedure Check_Lengths (St : Ghdl_Extname_State_Acc;
                            Rti : Ghdl_Rti_Access;
                            Bounds : Address;
                            Exp_Bounds : Address)
   is
      Arr : constant Ghdl_Rtin_Type_Array_Acc := Get_Base_Array_Type (Rti);
      Rngs, Exp_Rngs : Ghdl_Range_Array (0 .. Arr.Nbr_Dim - 1);
      Idx : Ghdl_Rti_Access;
   begin
      Bound_To_Range (Bounds, Arr, Rngs);
      Bound_To_Range (Exp_Bounds, Arr, Exp_Rngs);
      for I in Rngs'Range loop
         if Rngs (I) /= null and then Exp_Rngs (I) /= null then
            Idx := Get_Base_Type (Arr.Indexes (I));
            if Range_To_Length (Rngs (I), Idx)
              /= Range_To_Length (Exp_Rngs (I), Idx)
            then
               Error_Msg (St, "length of the object does not match the"
                            & " subtype of the external name");
            end if;
         end if;
      end loop;
   end Check_Lengths;

   --  Check that the range of scalar subtype RTI is the same as EXP_RNG.
   procedure Check_Range (St : Ghdl_Extname_State_Acc;
                          Rti : Ghdl_Rti_Access;
                          Ctxt : Rti_Context;
                          Exp_Rng : Address)
   is
      Exp : constant Ghdl_Range_Ptr := To_Ghdl_Range_Ptr (Exp_Rng);
      Rng : Ghdl_Range_Ptr;
      Ok : Boolean;
   begin
      case Rti.Kind is
         when Ghdl_Rtik_Subtype_Scalar =>
            declare
               Sub : constant Ghdl_Rtin_Subtype_Scalar_Acc :=
                 To_Ghdl_Rtin_Subtype_Scalar_Acc (Rti);
            begin
               Rng := To_Ghdl_Range_Ptr
                 (Loc_To_Addr (Sub.Common.Depth, Sub.Range_Loc, Ctxt));
               case Sub.Basetype.Kind is
                  when Ghdl_Rtik_Type_B1 =>
                     Ok := Rng.B1.Left = Exp.B1.Left
                       and Rng.B1.Right = Exp.B1.Right
                       and Rng.B1.Dir = Exp.B1.Dir;
                  when Ghdl_Rtik_Type_E8 =>
                     Ok := Rng.E8.Left = Exp.E8.Left
                       and Rng.E8.Right = Exp.E8.Right
                       and Rng.E8.Dir = Exp.E8.Dir;
                  when Ghdl_Rtik_Type_E32 =>
                     Ok := Rng.E32.Left = Exp.E32.Left
                       and Rng.E32.Right = Exp.E32.Right
                       and Rng.E32.Dir = Exp.E32.Dir;
                  when Ghdl_Rtik_Type_I32
                    | Ghdl_Rtik_Type_P32 =>
                     Ok := Rng.I32.Left = Exp.I32.Left
                       and Rng.I32.Right = Exp.I32.Right
                       and Rng.I32.Dir = Exp.I32.Dir;
                  when Ghdl_Rtik_Type_I64
                    | Ghdl_Rtik_Type_P64 =>
                     Ok := Rng.P64.Left = Exp.P64.Left
                       and Rng.P64.Right = Exp.P64.Right
                       and Rng.P64.Dir = Exp.P64.Dir;
                  when Ghdl_Rtik_Type_F64 =>
                     Ok := Rng.F64.Left = Exp.F64.Left
                       and Rng.F64.Right = Exp.F64.Right
                       and Rng.F64.Dir = Exp.F64.Dir;
                  when others =>
                     Ok := True;
               end case;
            end;
         when Ghdl_Rtik_Type_B1 =>
            --  The whole range of the type.
            Ok := Exp.B1.Left = False and Exp.B1.Right = True
              and Exp.B1.Dir = Dir_To;
         when Ghdl_Rtik_Type_E8 =>
            Ok := Exp.E8.Left = 0
              and Ghdl_Index_Type (Exp.E8.Right)
                = To_Ghdl_Rtin_Type_Enum_Acc (Rti).Nbr - 1
              and Exp.E8.Dir = Dir_To;
         when Ghdl_Rtik_Type_E32 =>
            Ok := Exp.E32.Left = 0
              and Ghdl_Index_Type (Exp.E32.Right)
                = To_Ghdl_Rtin_Type_Enum_Acc (Rti).Nbr - 1
              and Exp.E32.Dir = Dir_To;
         when others =>
            Ok := True;
      end case;

      if not Ok then
         Error_Msg (St, "bounds of the object do not match the subtype of"
                      & " the external name");
      end if;
   end Check_Range;

   procedure Ghdl_Extname_Object (St : Ghdl_Extname_State_Acc;
                                  Name : Ghdl_C_String;
                                  Class : Ghdl_Index_Type;
                                  Base_Type : Ghdl_Rti_Access;
                                  Exp_Bounds : Address;
                                  Res : Ghdl_Extname_Result_Acc)
   is
      --  Search NAME in the children of CTXT.
      function Find (Ctxt : Rti_Context) return Ghdl_Rtin_Object_Acc
      is
         Blk : constant Ghdl_Rtin_Block_Acc :=
           To_Ghdl_Rtin_Block_Acc (Ctxt.Block);
         Child : Ghdl_Rti_Access;
      begin
         for I in 1 .. Blk.Nbr_Child loop
            Child := Blk.Children (I - 1);
            case Child.Kind is
               when Ghdl_Rtik_Constant
                 | Ghdl_Rtik_Generic
                 | Ghdl_Rtik_Signal
                 | Ghdl_Rtik_Port
                 | Ghdl_Rtik_Guard
                 | Ghdl_Rtik_Variable =>
                  if Is_Name (To_Ghdl_Rtin_Object_Acc (Child).Name, Name)
                  then
                     return To_Ghdl_Rtin_Object_Acc (Child);
                  end if;
               when others =>
                  null;
            end case;
         end loop;
         return null;
      end Find;

      Ctxt : Rti_Context;
      Obj : Ghdl_Rtin_Object_Acc;
      Obj_Type : Ghdl_Rti_Access;
      Obj_Loc : Address;
      Addr : Address;
      Bounds : Address;
      Ok : Boolean;
   begin
      if St.Step = Step_Failed then
         Res.all := (Sig => Null_Address,
                     Val => Null_Address,
                     Bounds => Null_Address);
         return;
      end if;
      if St.Step /= Step_Region then
         Error_Msg (St, "incomplete pathname");
      end if;

      Ctxt := (Base => St.Base, Block => St.Block);
      Obj := Find (Ctxt);
      if Obj = null and then Ctxt.Block.Kind = Ghdl_Rtik_Architecture then
         --  Look in the entity.
         Ctxt := Get_Parent_Context (Ctxt);
         Obj := Find (Ctxt);
      end if;
      if Obj = null then
         Error_Name (St, "cannot find object ", Name, "");
      end if;

      case Class is
         when Extname_Constant =>
            Ok := Obj.Common.Kind in Ghdl_Rtik_Constant | Ghdl_Rtik_Generic;
         when Extname_Signal =>
            Ok := Obj.Common.Kind in Ghdl_Rtik_Signal | Ghdl_Rtik_Port
              | Ghdl_Rtik_Guard;
         when Extname_Variable =>
            Ok := Obj.Common.Kind = Ghdl_Rtik_Variable;
         when others =>
            Ok := False;
      end case;
      if not Ok then
         case Class is
            when Extname_Constant =>
               Error_Name (St, "", Name, " is not a constant");
            when Extname_Signal =>
               Error_Name (St, "", Name, " is not a signal");
            when others =>
               Error_Name (St, "", Name, " is not a variable");
         end case;
      end if;

      Obj_Type := Obj.Obj_Type;
      if Base_Type /= null
        and then Get_Base_Type_Or_Null (Obj_Type) /= Base_Type
      then
         Error_Name (St, "type of ", Name,
                     " is not the type of the external name");
      end if;

      Obj_Loc := Loc_To_Addr (Obj.Common.Depth, Obj.Loc, Ctxt);
      Object_To_Base_Bounds (Obj_Type, Obj_Loc, Addr, Bounds);
      if Obj_Type.Kind = Ghdl_Rtik_Type_Protected then
         --  The location contains a pointer to the protected object.
         Addr := To_Addr_Acc (Obj_Loc).all;
      end if;
      if Bounds = Null_Address then
         Bounds := Get_Subtype_Bounds (Obj_Type, Ctxt);
      end if;

      if Exp_Bounds /= Null_Address then
         case Obj_Type.Kind is
            when Ghdl_Rtik_Type_Array
              | Ghdl_Rtik_Subtype_Array
              | Ghdl_Rtik_Subtype_Unbounded_Array =>
               if Bounds /= Null_Address then
                  Check_Lengths (St, Obj_Type, Bounds, Exp_Bounds);
               end if;
            when Ghdl_Rtik_Subtype_Scalar
              | Ghdl_Rtik_Type_B1
              | Ghdl_Rtik_Type_E8
              | Ghdl_Rtik_Type_E32 =>
               Check_Range (St, Obj_Type, Ctxt, Exp_Bounds);
            when others =>
               null;
         end case;
      end if;

      Res.Bounds := Bounds;
      if Class = Extname_Signal then
         declare
            Status : Scalar_Status;
            Sig : Ghdl_Signal_Ptr;
            Val_Off : Ghdl_Rti_Loc;
         begin
            Get_First_Scalar_Signal
              (Obj_Type, Ctxt, Addr, Bounds, Status, Sig, Val_Off);
            if Status = Scalar_Unhandled then
               Error_Name (St, "type of signal ", Name,
                           " is not supported by this backend");
            end if;
            if Status = Scalar_Found and then Sig = null then
               if Soft_Fail (St, False) then
                  Res.all := (Sig => Null_Address,
                              Val => Null_Address,
                              Bounds => Null_Address);
                  return;
               end if;
               Error_Name (St, "", Name, " is not yet elaborated");
            end if;
            if Sig = null then
               --  No scalar subelement, so no value.
               Res.Val := Null_Address;
            else
               Res.Val := Sig.Value_Ptr.all'Address - Val_Off;
            end if;
            case Obj_Type.Kind is
               when Ghdl_Rtik_Type_E8
                 | Ghdl_Rtik_Type_E32
                 | Ghdl_Rtik_Type_B1
                 | Ghdl_Rtik_Type_I32
                 | Ghdl_Rtik_Type_I64
                 | Ghdl_Rtik_Type_P32
                 | Ghdl_Rtik_Type_P64
                 | Ghdl_Rtik_Type_F64
                 | Ghdl_Rtik_Subtype_Scalar =>
                  --  The signal itself.
                  Res.Sig := To_Addr_Acc (Addr).all;
               when others =>
                  Res.Sig := Addr;
            end case;
         end;
      else
         Res.Sig := Null_Address;
         Res.Val := Addr;
      end if;
   end Ghdl_Extname_Object;
end Grt.External_Names;
