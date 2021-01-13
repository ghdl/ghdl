--  GHDL Run Time (GRT) - Tree displayer.
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
with System; use System;
with Grt.Disp_Rti; use Grt.Disp_Rti;
with Grt.Rtis; use Grt.Rtis;
with Grt.Stdio; use Grt.Stdio;
with Grt.Astdio; use Grt.Astdio;
with Grt.Types; use Grt.Types;
with Grt.Errors; use Grt.Errors;
with Grt.Rtis_Addr; use Grt.Rtis_Addr;
with Grt.Hooks; use Grt.Hooks;

package body Grt.Disp_Tree is
   --  Set by --disp-tree, to display the design hierarchy.
   type Disp_Tree_Kind is
     (
      Disp_Tree_None,  --  Do not disp tree.
      Disp_Tree_Inst,  --  Disp entities, arch, package, blocks, components.
      Disp_Tree_Proc,  --  As above plus processes
      Disp_Tree_Port   --  As above plus ports and signals.
     );
   Disp_Tree_Flag : Disp_Tree_Kind := Disp_Tree_None;


   --  Get next interesting child.
   procedure Get_Tree_Child (Parent : Ghdl_Rtin_Block_Acc;
                             Index : in out Ghdl_Index_Type;
                             Child : out Ghdl_Rti_Access)
   is
   begin
      --  Exit if no more children.
      while Index < Parent.Nbr_Child loop
         Child := Parent.Children (Index);
         Index := Index + 1;
         case Child.Kind is
            when Ghdl_Rtik_Package
              | Ghdl_Rtik_Entity
              | Ghdl_Rtik_Architecture
              | Ghdl_Rtik_Block
              | Ghdl_Rtik_For_Generate
              | Ghdl_Rtik_If_Generate
              | Ghdl_Rtik_Case_Generate
              | Ghdl_Rtik_Instance =>
               return;
            when Ghdl_Rtik_Signal
              | Ghdl_Rtik_Port
              | Ghdl_Rtik_Guard =>
               if Disp_Tree_Flag >= Disp_Tree_Port then
                  return;
               end if;
            when Ghdl_Rtik_Process =>
               if Disp_Tree_Flag >= Disp_Tree_Proc then
                  return;
               end if;
            when others =>
               null;
         end case;
      end loop;
      Child := null;
   end Get_Tree_Child;

   procedure Disp_Tree_Child (Rti : Ghdl_Rti_Access; Ctxt : Rti_Context)
   is
   begin
      case Rti.Kind is
         when Ghdl_Rtik_Entity
           | Ghdl_Rtik_Process
           | Ghdl_Rtik_Architecture
           | Ghdl_Rtik_Block
           | Ghdl_Rtik_If_Generate
           | Ghdl_Rtik_Case_Generate =>
            declare
               Blk : constant Ghdl_Rtin_Block_Acc :=
                 To_Ghdl_Rtin_Block_Acc (Rti);
            begin
               Disp_Name (Blk.Name);
            end;
         when Ghdl_Rtik_Package_Body
           | Ghdl_Rtik_Package =>
            declare
               Blk : Ghdl_Rtin_Block_Acc;
               Lib : Ghdl_Rtin_Type_Scalar_Acc;
            begin
               Blk := To_Ghdl_Rtin_Block_Acc (Rti);
               if Rti.Kind = Ghdl_Rtik_Package_Body then
                  Blk := To_Ghdl_Rtin_Block_Acc (Blk.Parent);
               end if;
               Lib := To_Ghdl_Rtin_Type_Scalar_Acc (Blk.Parent);
               Disp_Name (Lib.Name);
               Put ('.');
               Disp_Name (Blk.Name);
            end;
         when Ghdl_Rtik_For_Generate =>
            declare
               Gen : constant Ghdl_Rtin_Generate_Acc :=
                 To_Ghdl_Rtin_Generate_Acc (Rti);
               Bod : constant Ghdl_Rtin_Block_Acc :=
                 To_Ghdl_Rtin_Block_Acc (Gen.Child);
               Iter : constant Ghdl_Rtin_Object_Acc :=
                 To_Ghdl_Rtin_Object_Acc (Bod.Children (0));
               Addr, Bounds : Address;
            begin
               Disp_Name (Gen.Name);
               Addr := Loc_To_Addr (Iter.Common.Depth, Iter.Loc, Ctxt);
               Bounds := Null_Address;
               Put ('(');
               Disp_Value (stdout, Iter.Obj_Type, Ctxt, Addr, Bounds, False);
               Put (')');
            end;
         when Ghdl_Rtik_Signal
           | Ghdl_Rtik_Port
           | Ghdl_Rtik_Guard
           | Ghdl_Rtik_Iterator =>
            Disp_Name (To_Ghdl_Rtin_Object_Acc (Rti).Name);
         when Ghdl_Rtik_Instance =>
            Disp_Name (To_Ghdl_Rtin_Instance_Acc (Rti).Name);
         when others =>
            null;
      end case;

      case Rti.Kind is
         when Ghdl_Rtik_Package
           | Ghdl_Rtik_Package_Body =>
            Put (" [package]");
         when Ghdl_Rtik_Entity =>
            Put (" [entity]");
         when Ghdl_Rtik_Architecture =>
            Put (" [arch]");
         when Ghdl_Rtik_Process =>
            Put (" [process]");
         when Ghdl_Rtik_Block =>
            Put (" [block]");
         when Ghdl_Rtik_For_Generate =>
            Put (" [for-generate]");
         when Ghdl_Rtik_If_Generate =>
            Put (" [if-generate ");
            if Ctxt.Base = Null_Address then
               Put ("false");
            else
               Put ("true");
            end if;
            Put ("]");
         when Ghdl_Rtik_Case_Generate =>
            Put (" [case-generate]");
         when Ghdl_Rtik_Signal =>
            Put (" [signal]");
         when Ghdl_Rtik_Port =>
            Put (" [port ");
            case Rti.Mode and Ghdl_Rti_Signal_Mode_Mask is
               when Ghdl_Rti_Signal_Mode_In =>
                  Put ("in");
               when Ghdl_Rti_Signal_Mode_Out =>
                  Put ("out");
               when Ghdl_Rti_Signal_Mode_Inout =>
                  Put ("inout");
               when Ghdl_Rti_Signal_Mode_Buffer =>
                  Put ("buffer");
               when Ghdl_Rti_Signal_Mode_Linkage =>
                  Put ("linkage");
               when others =>
                  Put ("?");
            end case;
            Put ("]");
         when Ghdl_Rtik_Guard =>
            Put (" [guard]");
         when Ghdl_Rtik_Iterator =>
            Put (" [iterator]");
         when Ghdl_Rtik_Instance =>
            Put (" [instance]");
         when others =>
            null;
      end case;
   end Disp_Tree_Child;

   procedure Disp_Tree_Block
     (Blk : Ghdl_Rtin_Block_Acc; Ctxt : Rti_Context; Pfx : String);

   procedure Disp_Tree_Block1
     (Blk : Ghdl_Rtin_Block_Acc; Ctxt : Rti_Context; Pfx : String)
   is
      Child : Ghdl_Rti_Access;
      Child2 : Ghdl_Rti_Access;
      Index : Ghdl_Index_Type;

      procedure Disp_Header (Nctxt : Rti_Context;
                             Force_Cont : Boolean := False)
      is
      begin
         Put (Pfx);

         if Blk.Common.Kind /= Ghdl_Rtik_Entity
           and Child2 = null
           and Force_Cont = False
         then
            Put ("`-");
         else
            Put ("+-");
         end if;

         Disp_Tree_Child (Child, Nctxt);
         New_Line;
      end Disp_Header;

      procedure Disp_Sub_Block
        (Sub_Blk : Ghdl_Rtin_Block_Acc; Nctxt : Rti_Context)
      is
         Npfx : String (1 .. Pfx'Length + 2);
      begin
         Npfx (1 .. Pfx'Length) := Pfx;
         Npfx (Pfx'Length + 2) := ' ';
         if Child2 = null then
            Npfx (Pfx'Length + 1) := ' ';
         else
            Npfx (Pfx'Length + 1) := '|';
         end if;
         Disp_Tree_Block (Sub_Blk, Nctxt, Npfx);
      end Disp_Sub_Block;

   begin
      Index := 0;
      Get_Tree_Child (Blk, Index, Child);
      while Child /= null loop
         Get_Tree_Child (Blk, Index, Child2);

         case Child.Kind is
            when Ghdl_Rtik_Process
              | Ghdl_Rtik_Block =>
               declare
                  Nblk : constant Ghdl_Rtin_Block_Acc :=
                    To_Ghdl_Rtin_Block_Acc (Child);
                  Nctxt : Rti_Context;
               begin
                  Nctxt := (Base => Ctxt.Base + Nblk.Loc,
                            Block => Child);
                  Disp_Header (Nctxt, False);
                  Disp_Sub_Block (Nblk, Nctxt);
               end;
            when Ghdl_Rtik_For_Generate =>
               declare
                  Gen : constant Ghdl_Rtin_Generate_Acc :=
                    To_Ghdl_Rtin_Generate_Acc (Child);
                  Nctxt : Rti_Context;
                  Length : Ghdl_Index_Type;
                  Old_Child2 : Ghdl_Rti_Access;
               begin
                  Nctxt := (Base => To_Addr_Acc (Ctxt.Base + Gen.Loc).all,
                            Block => Gen.Child);
                  Length := Get_For_Generate_Length (Gen, Ctxt);
                  Disp_Header (Nctxt, Length > 1);
                  Old_Child2 := Child2;
                  if Length > 1 then
                     Child2 := Child;
                  end if;
                  for I in 1 .. Length loop
                     Disp_Sub_Block
                       (To_Ghdl_Rtin_Block_Acc (Gen.Child), Nctxt);
                     if I /= Length then
                        Nctxt.Base := Nctxt.Base + Gen.Size;
                        if I = Length - 1 then
                           Child2 := Old_Child2;
                        end if;
                        Disp_Header (Nctxt);
                     end if;
                  end loop;
                  Child2 := Old_Child2;
               end;
            when Ghdl_Rtik_If_Generate
              | Ghdl_Rtik_Case_Generate =>
               declare
                  Nctxt : constant Rti_Context :=
                    Get_If_Case_Generate_Child (Ctxt, Child);
               begin
                  Disp_Header (Nctxt);
                  if Nctxt.Base /= Null_Address then
                     Disp_Sub_Block
                       (To_Ghdl_Rtin_Block_Acc (Nctxt.Block), Nctxt);
                  end if;
               end;
            when Ghdl_Rtik_Instance =>
               declare
                  Inst : Ghdl_Rtin_Instance_Acc;
                  Sub_Ctxt : Rti_Context;
                  Sub_Blk : Ghdl_Rtin_Block_Acc;
                  Npfx : String (1 .. Pfx'Length + 4);
                  Comp : Ghdl_Rtin_Component_Acc;
                  Ch : Ghdl_Rti_Access;
               begin
                  Disp_Header (Ctxt);
                  Inst := To_Ghdl_Rtin_Instance_Acc (Child);
                  Get_Instance_Context (Inst, Ctxt, Sub_Ctxt);
                  Sub_Blk := To_Ghdl_Rtin_Block_Acc (Sub_Ctxt.Block);
                  if Inst.Instance.Kind = Ghdl_Rtik_Component
                    and then Disp_Tree_Flag >= Disp_Tree_Port
                  then
                     --  Disp generics and ports of the component.
                     Comp := To_Ghdl_Rtin_Component_Acc (Inst.Instance);
                     for I in 1 .. Comp.Nbr_Child loop
                        Ch := Comp.Children (I - 1);
                        if Ch.Kind = Ghdl_Rtik_Port then
                           --  Disp only port (and not generics).
                           Put (Pfx);
                           if Child2 = null then
                              Put ("  ");
                           else
                              Put ("| ");
                           end if;
                           if I = Comp.Nbr_Child and then Sub_Blk = null then
                              Put ("`-");
                           else
                              Put ("+-");
                           end if;
                           Disp_Tree_Child (Ch, Sub_Ctxt);
                           New_Line;
                        end if;
                     end loop;
                  end if;
                  if Sub_Blk /= null then
                     Npfx (1 .. Pfx'Length) := Pfx;
                     if Child2 = null then
                        Npfx (Pfx'Length + 1) := ' ';
                     else
                        Npfx (Pfx'Length + 1) := '|';
                     end if;
                     Npfx (Pfx'Length + 2) := ' ';
                     Npfx (Pfx'Length + 3) := '`';
                     Npfx (Pfx'Length + 4) := '-';
                     Put (Npfx);
                     Disp_Tree_Child (Sub_Blk.Parent, Sub_Ctxt);
                     New_Line;
                     Npfx (Pfx'Length + 3) := ' ';
                     Npfx (Pfx'Length + 4) := ' ';
                     Disp_Tree_Block (Sub_Blk, Sub_Ctxt, Npfx);
                  end if;
               end;
            when others =>
               Disp_Header (Ctxt);
         end case;

         Child := Child2;
      end loop;
   end Disp_Tree_Block1;

   procedure Disp_Tree_Block
     (Blk : Ghdl_Rtin_Block_Acc; Ctxt : Rti_Context; Pfx : String)
   is
   begin
      case Blk.Common.Kind is
         when Ghdl_Rtik_Architecture =>
            declare
               Npfx : String (1 .. Pfx'Length + 2);
               Nctxt : Rti_Context;
            begin
               --  The entity.
               Nctxt := (Base => Ctxt.Base,
                         Block => Blk.Parent);
               Disp_Tree_Block1
                 (To_Ghdl_Rtin_Block_Acc (Blk.Parent), Nctxt, Pfx);
               --  Then the architecture.
               Put (Pfx);
               Put ("`-");
               Disp_Tree_Child (To_Ghdl_Rti_Access (Blk), Ctxt);
               New_Line;
               Npfx (1 .. Pfx'Length) := Pfx;
               Npfx (Pfx'Length + 1) := ' ';
               Npfx (Pfx'Length + 2) := ' ';
               Disp_Tree_Block1 (Blk, Ctxt, Npfx);
            end;
         when Ghdl_Rtik_Package_Body =>
            Disp_Tree_Block1
              (To_Ghdl_Rtin_Block_Acc (Blk.Parent), Ctxt, Pfx);
         when others =>
            Disp_Tree_Block1 (Blk, Ctxt, Pfx);
      end case;
   end Disp_Tree_Block;

   procedure Disp_Hierarchy
   is
      Ctxt : Rti_Context;
      Parent : Ghdl_Rtin_Block_Acc;
      Child : Ghdl_Rti_Access;
   begin
      if Disp_Tree_Flag = Disp_Tree_None then
         return;
      end if;

      Ctxt := Get_Top_Context;
      Parent := To_Ghdl_Rtin_Block_Acc (Ctxt.Block);

      Disp_Tree_Child (Parent.Parent, Ctxt);
      New_Line;
      Disp_Tree_Block (Parent, Ctxt, "");

      for I in 1 .. Ghdl_Rti_Top.Nbr_Child loop
         Child := Ghdl_Rti_Top.Children (I - 1);
         Ctxt := (Base => Null_Address,
                  Block => Child);
         Disp_Tree_Child (Child, Ctxt);
         New_Line;
         Disp_Tree_Block (To_Ghdl_Rtin_Block_Acc (Child), Ctxt, "");
      end loop;
   end Disp_Hierarchy;

   function Disp_Tree_Option (Option : String) return Boolean
   is
      Opt : constant String (1 .. Option'Length) := Option;
   begin
      if Opt'Length >= 11 and then Opt (1 .. 11) = "--disp-tree" then
         if Opt'Length = 11 then
            Disp_Tree_Flag := Disp_Tree_Port;
         elsif Opt (12 .. Opt'Last) = "=port" then
            Disp_Tree_Flag := Disp_Tree_Port;
         elsif Opt (12 .. Opt'Last) = "=proc" then
            Disp_Tree_Flag := Disp_Tree_Proc;
         elsif Opt (12 .. Opt'Last) = "=inst" then
            Disp_Tree_Flag := Disp_Tree_Inst;
         elsif Opt (12 .. Opt'Last) = "=none" then
            Disp_Tree_Flag := Disp_Tree_None;
         else
            Error ("bad argument for --disp-tree option, try --help");
         end if;
         return True;
      else
         return False;
      end if;
   end Disp_Tree_Option;

   procedure Disp_Tree_Help
   is
      procedure P (Str : String) renames Put_Line;
   begin
      P (" --disp-tree[=KIND] disp the design hierarchy after elaboration");
      P ("       KIND is inst, proc, port (default)");
   end Disp_Tree_Help;

   Disp_Tree_Hooks : aliased constant Hooks_Type :=
     (Desc => new String'
        ("disp-tree: display design hierarchy (--disp-tree)"),
      Option => Disp_Tree_Option'Access,
      Help => Disp_Tree_Help'Access,
      Init => null,
      Start => Disp_Hierarchy'Access,
      Finish => null);

   procedure Register is
   begin
      Register_Hooks (Disp_Tree_Hooks'Access);
   end Register;

end Grt.Disp_Tree;
