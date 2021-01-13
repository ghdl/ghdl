--  Mcode back-end for ortho - Optimization.
--  Copyright (C) 2006 Tristan Gingold
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
with Ortho_Code.Flags;

package body Ortho_Code.Opts is
   procedure Relabel_Jump (Jmp : O_Enode)
   is
      Label : O_Enode;
      Bb : O_Enode;
   begin
      Label := Get_Jump_Label (Jmp);
      if Get_Expr_Kind (Label) = OE_Label then
         Bb := O_Enode (Get_Label_Info (Label));
         if Bb /= O_Enode_Null then
            Set_Jump_Label (Jmp, Bb);
         end if;
      end if;
   end Relabel_Jump;

   procedure Jmp_To_Bb (Subprg : Subprogram_Data_Acc)
   is
      First : O_Enode;
      Stmt : O_Enode;
      Prev : O_Enode;
      Cur_Bb : O_Enode;
   begin
      --  Get first statement after entry.
      First := Get_Stmt_Link (Subprg.E_Entry);

      --  First loop:
      --  If a label belongs to a BB (ie, is at the beginning of a BB),
      --  then link it to the BB.
      Stmt := First;
      Cur_Bb := O_Enode_Null;
      loop
         case Get_Expr_Kind (Stmt) is
            when OE_Leave =>
               exit;
            when OE_BB =>
               Cur_Bb := Stmt;
            when OE_Label =>
               if Cur_Bb /= O_Enode_Null then
                  Set_Label_Info (Stmt, Int32 (Cur_Bb));
               end if;
            when OE_Jump
              | OE_Jump_T
              | OE_Jump_F =>
               --  This handles backward jump.
               Relabel_Jump (Stmt);
            when others =>
               Cur_Bb := O_Enode_Null;
         end case;
         Stmt := Get_Stmt_Link (Stmt);
      end loop;

      --  Second loop:
      --  Transform jump to label to jump to BB.
      Stmt := First;
      Prev := O_Enode_Null;
      loop
         case Get_Expr_Kind (Stmt) is
            when OE_Leave =>
               exit;
            when OE_Jump
              | OE_Jump_T
              | OE_Jump_F =>
               --  This handles forward jump.
               Relabel_Jump (Stmt);
               --  Update PREV.
               Prev := Stmt;
            when OE_Label =>
               --  Remove the Label.
               --  Do not update PREV.
               if Get_Label_Info (Stmt) /= 0 then
                  Set_Stmt_Link (Prev, Get_Stmt_Link (Stmt));
               end if;
            when others =>
               Prev := Stmt;
         end case;
         Stmt := Get_Stmt_Link (Stmt);
      end loop;
   end Jmp_To_Bb;

   type Oe_Kind_Bool_Array is array (OE_Kind) of Boolean;
   Is_Passive_Stmt : constant Oe_Kind_Bool_Array :=
     (OE_Label | OE_BB | OE_End | OE_Beg => True,
      others => False);

   --  Return the next statement after STMT which really execute instructions.
   function Get_Fall_Stmt (Stmt : O_Enode) return O_Enode
   is
      Res : O_Enode;
   begin
      Res := Stmt;
      loop
         Res := Get_Stmt_Link (Res);
         case Get_Expr_Kind (Res) is
            when OE_Label
              | OE_BB
              | OE_End
              | OE_Beg =>
               null;
            when others =>
               return Res;
         end case;
      end loop;
   end Get_Fall_Stmt;
   pragma Unreferenced (Get_Fall_Stmt);

   procedure Thread_Jump (Subprg : Subprogram_Data_Acc)
   is
      First : O_Enode;
      Stmt : O_Enode;
      Prev, Next : O_Enode;
      Kind : OE_Kind;
   begin
      --  Get first statement after entry.
      First := Get_Stmt_Link (Subprg.E_Entry);

      --  First loop:
      --  If a label belongs to a BB (ie, is at the beginning of a BB),
      --  then link it to the BB.
      Stmt := First;
      Prev := O_Enode_Null;
      loop
         Next := Get_Stmt_Link (Stmt);
         Kind := Get_Expr_Kind (Stmt);
         case Kind is
            when OE_Leave =>
               exit;
            when OE_Jump =>
               --  Remove the jump if followed by the label.
               --    * For _T/_F: should convert to a ignore value.
               --  Discard unreachable statements after the jump.
               declare
                  N_Stmt : O_Enode;
                  P_Stmt : O_Enode;
                  Label : O_Enode;
                  Flag_Discard : Boolean;
                  K_Stmt : OE_Kind;
               begin
                  N_Stmt := Next;
                  P_Stmt := Stmt;
                  Label := Get_Jump_Label (Stmt);
                  Flag_Discard := True;
                  loop
                     if N_Stmt = Label then
                        --  Remove STMT.
                        Set_Stmt_Link (Prev, Next);
                        exit;
                     end if;
                     K_Stmt := Get_Expr_Kind (N_Stmt);
                     if K_Stmt = OE_Label then
                        --  Do not discard anymore statements, since they are
                        --  now reachable.
                        Flag_Discard := False;
                     end if;
                     if not Is_Passive_Stmt (K_Stmt) then
                        if not Flag_Discard then
                           --  We have found the next statement.
                           --  Keep the jump.
                           Prev := Stmt;
                           exit;
                        else
                           --  Delete insn.
                           N_Stmt := Get_Stmt_Link (N_Stmt);
                           Set_Stmt_Link (P_Stmt, N_Stmt);
                        end if;
                     else
                        --  Iterate.
                        P_Stmt := N_Stmt;
                        N_Stmt := Get_Stmt_Link (N_Stmt);
                     end if;
                  end loop;
               end;
            when others =>
               Prev := Stmt;
         end case;
         Stmt := Next;
      end loop;
   end Thread_Jump;

   procedure Optimize_Subprg (Subprg : Subprogram_Data_Acc)
   is
   begin
      --  Jump optimisation:
      --  * discard insns after a OE_JUMP.
      --  * Remove jump if followed by label
      --    (through label, BB, comments, end, line)
      --  * Redirect jump to jump (infinite loop !)
      --  * Revert jump_t/f if expr is not (XXX)
      --  * Jmp_t/f L:; jmp L2; L1:  ->  jmp_f/t L2
      Thread_Jump (Subprg);
      if Flags.Flag_Opt_BB then
         Jmp_To_Bb (Subprg);
      end if;
   end Optimize_Subprg;
end Ortho_Code.Opts;

