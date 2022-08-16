with Types; use Types;
with PSL.Types; use PSL.Types;
with PSL.Errors; use PSL.Errors;
with PSL.CSE; use PSL.CSE;

package body PSL.Rewrites is
--     procedure Location_Copy (Dst, Src : Node) is
--     begin
--        Set_Location (Dst, Get_Location (Src));
--     end Location_Copy;

   --  Return [*0]
   function Build_Empty return Node is
      Res, Tmp : Node;
   begin
      Res := Create_Node (N_Star_Repeat_Seq);
      Tmp := Create_Node (N_Number);
      Set_Value (Tmp, 0);
      Set_Low_Bound (Res, Tmp);
      return Res;
   end Build_Empty;

   --  Return N[*]
   function Build_Star (N : Node) return Node is
      Res : Node;
   begin
      Res := Create_Node (N_Star_Repeat_Seq);
      Set_Sequence (Res, N);
      return Res;
   end Build_Star;

   --  Return N[+]
   function Build_Plus (N : Node) return Node is
      Res : Node;
   begin
      Res := Create_Node (N_Plus_Repeat_Seq);
      Set_Sequence (Res, N);
      return Res;
   end Build_Plus;

   --  Return N!
   function Build_Strong (N : Node) return Node is
      Res : Node;
   begin
      Res := Create_Node (N_Strong);
      Set_Property (Res, N);
      return Res;
   end Build_Strong;

   --  Return T[*]
   function Build_True_Star return Node is
   begin
      return Build_Star (True_Node);
   end Build_True_Star;

   function Build_Binary (K : Nkind; L, R : Node) return Node is
      Res : Node;
   begin
      Res := Create_Node (K);
      Set_Left (Res, L);
      Set_Right (Res, R);
      return Res;
   end Build_Binary;

   function Build_Concat (L, R : Node) return Node is
   begin
      return Build_Binary (N_Concat_SERE, L, R);
   end Build_Concat;

   function Build_Repeat (N : Node; Cnt : Uns32) return Node is
      Res : Node;
   begin
      if Cnt = 0 then
         raise Internal_Error;
      end if;
      Res := N;
      for I in 2 .. Cnt loop
         Res := Build_Concat (Res, N);
      end loop;
      return Res;
   end Build_Repeat;

   function Build_Overlap_Imp_Seq (S : Node; P : Node) return Node
   is
      Res : Node;
   begin
      Res := Create_Node (N_Overlap_Imp_Seq);
      Set_Sequence (Res, S);
      Set_Property (Res, P);
      return Res;
   end Build_Overlap_Imp_Seq;

   function Rewrite_Boolean (N : Node) return Node
   is
      Res : Node;
   begin
      case Get_Kind (N) is
         when N_Name =>
            Res := Get_Decl (N);
            pragma Assert (Res /= Null_Node);
            return Res;
         when N_Not_Bool =>
            Set_Boolean (N, Rewrite_Boolean (Get_Boolean (N)));
            return N;
         when N_And_Bool
            | N_Or_Bool
            | N_Imp_Bool
            | N_Equiv_Bool =>
            Set_Left (N, Rewrite_Boolean (Get_Left (N)));
            Set_Right (N, Rewrite_Boolean (Get_Right (N)));
            return N;
         when N_HDL_Expr =>
            return Get_HDL_Hash (N);
         when N_HDL_Bool =>
            return N;
         when others =>
            Error_Kind ("rewrite_boolean", N);
      end case;
   end Rewrite_Boolean;

   function Rewrite_Star_Repeat_Seq (Seq : Node;
                                     Lo, Hi : Uns32) return Node
   is
      Res : Node;
   begin
      pragma Assert (Lo <= Hi);

      if Lo = Hi then

         if Lo = 0 then
            --  r[*0]  -->  [*0]
            return Build_Empty;
         elsif Lo = 1 then
            --  r[*1]  -->  r
            return Seq;
         end if;
         --  r[*c+]  -->  r;r;r...;r (c times)
         return Build_Repeat (Seq, Lo);
      end if;

      --  r[*0:1]  -->  [*0] | r
      --  r[*0:2]  -->  [*0] | r;{[*0]|r}

      --  r[*0:n]  -->  [*0] | r;r[*0:n-1]
      --  r[*l:h]  -->  r[*l] ; r[*0:h-l]
      Res := Build_Binary (N_Or_Seq, Build_Empty, Seq);
      for I in Lo + 2 .. Hi loop
         Res := Build_Concat (Seq, Res);
         Res := Build_Binary (N_Or_Seq, Build_Empty, Res);
      end loop;
      if Lo > 0 then
         Res := Build_Concat (Build_Repeat (Seq, Lo), Res);
      end if;

      return Res;
   end Rewrite_Star_Repeat_Seq;

   function Rewrite_Star_Repeat_Seq (Seq : Node;
                                     Lo, Hi : Node) return Node
   is
      Cnt_Lo : Uns32;
      Cnt_Hi : Uns32;
   begin
      --  r[*]  must have been handled.
      pragma Assert (Lo /= Null_Node);

      Cnt_Lo := Get_Value (Lo);
      if Hi = Null_Node then
         Cnt_Hi := Cnt_Lo;
      elsif Get_Kind (Hi) = N_Inf then
         --  r[*N to inf]  -->  r[*N] ; r[*]
         if Cnt_Lo = 0 then
            return Build_Star (Seq);
         else
            return Build_Concat (Rewrite_Star_Repeat_Seq (Seq, Cnt_Lo, Cnt_Lo),
                                 Build_Star (Seq));
         end if;
      else
         Cnt_Hi := Get_Value (Hi);
      end if;
      return Rewrite_Star_Repeat_Seq (Seq, Cnt_Lo, Cnt_Hi);
   end Rewrite_Star_Repeat_Seq;

   function Rewrite_Star_Repeat_Seq (N : Node) return Node
   is
      Seq : constant Node := Get_Sequence (N);
      Lo : constant Node := Get_Low_Bound (N);
   begin
      if Lo = Null_Node then
         --  r[*]  -->  r[*]
         return N;
      else
         return Rewrite_Star_Repeat_Seq (Seq, Lo, Get_High_Bound (N));
      end if;
   end Rewrite_Star_Repeat_Seq;

   function Rewrite_Goto_Repeat_Seq (B : Node; Lo, Hi : Node) return Node
   is
      Res : Node;
   begin
      --  b[->]  -->  {(~b)[*];b}
      Res := Build_Concat (Build_Star (Build_Bool_Not (B)), B);

      if Lo = Null_Node then
         return Res;
      end if;

      --  b[->l:h]  -->  {b[->]}[*l:h]
      return Rewrite_Star_Repeat_Seq (Res, Lo, Hi);
   end Rewrite_Goto_Repeat_Seq;

   function Rewrite_Goto_Repeat_Seq (B : Node; Lo, Hi : Uns32) return Node
   is
      Res : Node;
   begin
      --  b[->]  -->  {(~b)[*];b}
      Res := Build_Concat (Build_Star (Build_Bool_Not (B)), B);

      --  b[->l:h]  -->  {b[->]}[*l:h]
      return Rewrite_Star_Repeat_Seq (Res, Lo, Hi);
   end Rewrite_Goto_Repeat_Seq;

   function Rewrite_Equal_Repeat_Seq (N : Node) return Node
   is
      B : constant Node := Get_Boolean (N);
      Lo : constant Node := Get_Low_Bound (N);
      Hi : constant Node := Get_High_Bound (N);
   begin
      --  b[=l:h]  -->  {b[->l:h]};(~b)[*]
      return Build_Concat (Rewrite_Goto_Repeat_Seq (B, Lo, Hi),
                           Build_Star (Build_Bool_Not (B)));
   end Rewrite_Equal_Repeat_Seq;

   function Rewrite_Within (N : Node) return Node is
      Res : Node;
   begin
      Res := Build_Concat (Build_Concat (Build_True_Star, Get_Left (N)),
                           Build_True_Star);
      return Build_Binary (N_Match_And_Seq, Res, Get_Right (N));
   end Rewrite_Within;

   function Rewrite_And_Seq (L : Node; R : Node) return Node is
   begin
      return Build_Binary (N_Or_Seq,
                           Build_Binary (N_Match_And_Seq,
                                         L,
                                         Build_Concat (R, Build_True_Star)),
                           Build_Binary (N_Match_And_Seq,
                                         Build_Concat (L, Build_True_Star),
                                         R));
   end Rewrite_And_Seq;
   pragma Unreferenced (Rewrite_And_Seq);

   procedure Rewrite_Instance (N : Node)
   is
      Assoc : Node := Get_Association_Chain (N);
   begin
      while Assoc /= Null_Node loop
         case Get_Kind (Get_Formal (Assoc)) is
            when N_Const_Parameter =>
               null;
            when N_Boolean_Parameter =>
               Set_Actual (Assoc, Rewrite_Boolean (Get_Actual (Assoc)));
            when N_Sequence_Parameter =>
               Set_Actual (Assoc, Rewrite_SERE (Get_Actual (Assoc)));
            when N_Property_Parameter =>
               Set_Actual (Assoc, Rewrite_Property (Get_Actual (Assoc)));
            when others =>
               Error_Kind ("rewrite_instance", Get_Formal (Assoc));
         end case;
         Assoc := Get_Chain (Assoc);
      end loop;
   end Rewrite_Instance;

   function Rewrite_SERE (N : Node) return Node is
      S : Node;
   begin
      case Get_Kind (N) is
         when N_Star_Repeat_Seq =>
            S := Get_Sequence (N);
            if S = Null_Node then
               S := True_Node;
            else
               S := Rewrite_SERE (S);
            end if;
            Set_Sequence (N, S);
            return Rewrite_Star_Repeat_Seq (N);
         when N_Plus_Repeat_Seq =>
            S := Get_Sequence (N);
            if S = Null_Node then
               S := True_Node;
            else
               S := Rewrite_SERE (S);
            end if;
            Set_Sequence (N, S);
            return N;
         when N_Goto_Repeat_Seq =>
            return Rewrite_Goto_Repeat_Seq
              (Rewrite_SERE (Get_Boolean (N)),
               Get_Low_Bound (N), Get_High_Bound (N));
         when N_Equal_Repeat_Seq =>
            Set_Boolean (N, Rewrite_SERE (Get_Boolean (N)));
            return Rewrite_Equal_Repeat_Seq (N);
         when N_Braced_SERE =>
            return Rewrite_SERE (Get_SERE (N));
         when N_Clocked_SERE =>
            Set_SERE (N, Rewrite_SERE (Get_SERE (N)));
            Set_Boolean (N, Rewrite_Boolean (Get_Boolean (N)));
            return N;
         when N_Within_SERE =>
            Set_Left (N, Rewrite_SERE (Get_Left (N)));
            Set_Right (N, Rewrite_SERE (Get_Right (N)));
            return Rewrite_Within (N);
--           when N_And_Seq =>
--              return Rewrite_And_Seq (Rewrite_SERE (Get_Left (N)),
--                                      Rewrite_SERE (Get_Right (N)));
         when N_Concat_SERE
           | N_Fusion_SERE
           | N_Match_And_Seq
           | N_And_Seq
           | N_Or_Seq =>
            Set_Left (N, Rewrite_SERE (Get_Left (N)));
            Set_Right (N, Rewrite_SERE (Get_Right (N)));
            return N;
         when N_Booleans =>
            return Rewrite_Boolean (N);
         when N_Name =>
            return Get_Decl (N);
         when N_Sequence_Instance =>
            Rewrite_Instance (N);
            return N;
         when N_Endpoint_Instance =>
            return N;
         when N_Boolean_Parameter
           | N_Sequence_Parameter
           | N_Const_Parameter =>
            return N;
         when others =>
            Error_Kind ("rewrite_SERE", N);
      end case;
   end Rewrite_SERE;

   function Rewrite_Until (N : Node) return Node
   is
      Res : Node;
      B : Node;
      L : Node;
      S : Node;
   begin
      if Get_Inclusive_Flag (N) then
         --  B1 until_ B2 --> {B1[+]:B2}
         Res := Build_Binary (N_Fusion_SERE,
                              Build_Plus (Rewrite_Boolean (Get_Left (N))),
                              Rewrite_Boolean (Get_Right (N)));
         if Get_Strong_Flag (N) then
            Res := Build_Strong (Res);
         end if;
      else
         --  P until B  -->  {(!B)[+]} |-> P
         B := Rewrite_Boolean (Get_Right (N));
         L := Build_Plus (Build_Bool_Not (B));
         Res := Build_Overlap_Imp_Seq (L, Rewrite_Property (Get_Left (N)));

         if Get_Strong_Flag (N) then
            --  p until! b  -->  (p until b) && ({b[->]}!)
            S := Build_Strong
              (Rewrite_Goto_Repeat_Seq (B, Null_Node, Null_Node));
            Res := Build_Binary (N_And_Prop, Res, S);
         end if;
      end if;
      return Res;
   end Rewrite_Until;

   function Rewrite_Next_Event_A (B : Node;
                                  Lo, Hi : Uns32;
                                  P : Node;
                                  Strong : Boolean) return Node
   is
      Res : Node;
   begin
      Res := Rewrite_Goto_Repeat_Seq (B, Lo, Hi);
      Res := Build_Overlap_Imp_Seq (Res, P);

      if Strong then
         Res := Build_Binary
           (N_And_Prop,
            Res,
            Build_Strong (Rewrite_Goto_Repeat_Seq (B, Lo, Lo)));
      end if;

      return Res;
   end Rewrite_Next_Event_A;

   function Rewrite_Next_Event (B : Node;
                                N : Uns32;
                                P : Node;
                                Strong : Boolean) return Node is
   begin
      return Rewrite_Next_Event_A (B, N, N, P, Strong);
   end Rewrite_Next_Event;

   function Rewrite_Next_Event (B : Node;
                                Num : Node;
                                P : Node;
                                Strong : Boolean) return Node
   is
      N : Uns32;
   begin
      if Num = Null_Node then
         N := 1;
      else
         N := Get_Value (Num);
      end if;
      return Rewrite_Next_Event (B, N, P, Strong);
   end Rewrite_Next_Event;

   function Rewrite_Next (Num : Node; P : Node; Strong : Boolean) return Node
   is
      N : Uns32;
   begin
      if Num = Null_Node then
         N := 1;
      else
         N := Get_Value (Num);
      end if;
      return Rewrite_Next_Event (True_Node, N + 1, P, Strong);
   end Rewrite_Next;

   function Rewrite_Next_A (Lo, Hi : Uns32;
                            P : Node; Strong : Boolean) return Node
   is
   begin
      return Rewrite_Next_Event_A (True_Node, Lo + 1, Hi + 1, P, Strong);
   end Rewrite_Next_A;

   function Rewrite_Next_Event_E (B1 : Node;
                                  Lo, Hi : Uns32;
                                  B2 : Node; Strong : Boolean) return Node
   is
      Res : Node;
   begin
      Res := Build_Binary (N_Fusion_SERE,
                           Rewrite_Goto_Repeat_Seq (B1, Lo, Hi),
                           B2);
      if Strong then
         Res := Build_Strong (Res);
      end if;
      return Res;
   end Rewrite_Next_Event_E;

   function Rewrite_Next_E (Lo, Hi : Uns32;
                            B : Node; Strong : Boolean) return Node
   is
   begin
      return Rewrite_Next_Event_E (True_Node, Lo + 1, Hi + 1, B, Strong);
   end Rewrite_Next_E;

   function Rewrite_Before (N : Node) return Node
   is
      Res : Node;
      R : Node;
      B1, B2 : Node;
      N_B2 : Node;
   begin
      --  b1 before b2
      B1 := Rewrite_Boolean (Get_Left (N));
      B2 := Rewrite_Boolean (Get_Right (N));
      N_B2 := Build_Bool_Not (B2);
      --  (!b1 && !b2)[*]
      Res := Build_Star (Build_Bool_And (Build_Bool_Not (B1), N_B2));

      if Get_Inclusive_Flag (N) then
         R := B1;
      else
         R := Build_Bool_And (B1, N_B2);
      end if;
      Res := Build_Concat (Res, R);
      if Get_Strong_Flag (N) then
         Res := Build_Strong (Res);
      end if;
      return Res;
   end Rewrite_Before;

   function Rewrite_Or (L, R : Node) return Node
   is
      B, P : Node;
   begin
      if Get_Kind (L) in N_Booleans then
         if Get_Kind (R) in N_Booleans then
            return Build_Bool_Or (L, R);
         else
            B := L;
            P := R;
         end if;
      elsif Get_Kind (R) in N_Booleans then
         B := R;
         P := L;
      else
         --  Not in the simple subset.
         raise Program_Error;
      end if;

      --  B || P  --> (~B) -> P
      return Build_Binary (N_Log_Imp_Prop, Build_Bool_Not (B), P);
   end Rewrite_Or;

   function Rewrite_Property (N : Node) return Node is
   begin
      case Get_Kind (N) is
         when N_Star_Repeat_Seq
           | N_Plus_Repeat_Seq
           | N_Equal_Repeat_Seq
           | N_Goto_Repeat_Seq
           | N_Sequence_Instance
           | N_Endpoint_Instance
           | N_Braced_SERE
           | N_And_Seq
           | N_Or_Seq =>
            return Rewrite_SERE (N);
         when N_Imp_Seq
           | N_Overlap_Imp_Seq =>
            Set_Sequence (N, Rewrite_Property (Get_Sequence (N)));
            Set_Property (N, Rewrite_Property (Get_Property (N)));
            return N;
         when N_Log_Imp_Prop =>
            --  b -> p   -->  {b} |-> p
            return Build_Overlap_Imp_Seq
              (Rewrite_Boolean (Get_Left (N)),
               Rewrite_Property (Get_Right (N)));
         when N_Eventually =>
            return Build_Strong
              (Build_Binary (N_Fusion_SERE,
                             Build_Plus (True_Node),
                             Rewrite_SERE (Get_Property (N))));
         when N_Until =>
            return Rewrite_Until (N);
         when N_Next =>
            return Rewrite_Next (Get_Number (N),
                                 Rewrite_Property (Get_Property (N)),
                                 Get_Strong_Flag (N));
         when N_Next_Event =>
            return Rewrite_Next_Event (Rewrite_Boolean (Get_Boolean (N)),
                                       Get_Number (N),
                                       Rewrite_Property (Get_Property (N)),
                                       Get_Strong_Flag (N));
         when N_Next_A =>
            return Rewrite_Next_A (Get_Value (Get_Low_Bound (N)),
                                   Get_Value (Get_High_Bound (N)),
                                   Rewrite_Property (Get_Property (N)),
                                   Get_Strong_Flag (N));
         when N_Next_Event_A =>
            return Rewrite_Next_Event_A
              (Rewrite_Boolean (Get_Boolean (N)),
               Get_Value (Get_Low_Bound (N)),
               Get_Value (Get_High_Bound (N)),
               Rewrite_Property (Get_Property (N)),
               Get_Strong_Flag (N));
         when N_Next_E =>
            return Rewrite_Next_E (Get_Value (Get_Low_Bound (N)),
                                   Get_Value (Get_High_Bound (N)),
                                   Rewrite_Property (Get_Property (N)),
                                   Get_Strong_Flag (N));
         when N_Next_Event_E =>
            return Rewrite_Next_Event_E
              (Rewrite_Boolean (Get_Boolean (N)),
               Get_Value (Get_Low_Bound (N)),
               Get_Value (Get_High_Bound (N)),
               Rewrite_Property (Get_Property (N)),
               Get_Strong_Flag (N));
         when N_Before =>
            return Rewrite_Before (N);
         when N_Booleans =>
            return Rewrite_Boolean (N);
         when N_Name =>
            return Get_Decl (N);
         when N_Never
           | N_Always
           | N_Strong =>
            --  Fully handled by psl.build
            Set_Property (N, Rewrite_Property (Get_Property (N)));
            return N;
         when N_Clock_Event =>
            Set_Property (N, Rewrite_Property (Get_Property (N)));
            Set_Boolean (N, Rewrite_Boolean (Get_Boolean (N)));
            return N;
         when N_And_Prop =>
            Set_Left (N, Rewrite_Property (Get_Left (N)));
            Set_Right (N, Rewrite_Property (Get_Right (N)));
            return N;
         when N_Or_Prop =>
            return Rewrite_Or (Rewrite_Property (Get_Left (N)),
                               Rewrite_Property (Get_Right (N)));
         when N_Abort
            | N_Async_Abort
            | N_Sync_Abort =>
            Set_Boolean (N, Rewrite_Boolean (Get_Boolean (N)));
            Set_Property (N, Rewrite_Property (Get_Property (N)));
            return N;
         when N_Property_Instance =>
            Rewrite_Instance (N);
            return N;
         when N_Paren_Prop =>
            --  Note: discard it.
            return Rewrite_Property (Get_Property (N));
         when others =>
            Error_Kind ("rewrite_property", N);
      end case;
   end Rewrite_Property;

   procedure Rewrite_Unit (N : Node) is
      Item : Node;
   begin
      Item := Get_Item_Chain (N);
      while Item /= Null_Node loop
         case Get_Kind (Item) is
            when N_Name_Decl =>
               null;
            when N_Assert_Directive =>
               Set_Property (Item, Rewrite_Property (Get_Property (Item)));
            when N_Property_Declaration =>
               Set_Property (Item, Rewrite_Property (Get_Property (Item)));
            when others =>
               Error_Kind ("rewrite_unit", Item);
         end case;
         Item := Get_Chain (Item);
      end loop;
   end Rewrite_Unit;
end PSL.Rewrites;
