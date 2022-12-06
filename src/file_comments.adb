--  Comments table.
--  Copyright (C) 2022 Tristan Gingold
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

with Grt.Algos;

with Simple_IO; use Simple_IO;
with Utils_IO; use Utils_IO;

package body File_Comments is
   Flag_Trace : constant Boolean := False;

   Ctxt : Comment_Context;

   procedure Comment_Init_Scan (File : Source_File_Entry) is
   begin
      Ctxt := (File => File,
               State => State_Before,
               Next => No_Comment_Index + 1,
               Last_Node => 0,
               Line_Start => Source_Ptr_Bad);

      --  Create entry for FILE if not already created.
      if Comments_Table.Last < Ctxt.File then
         while Comments_Table.Last < Ctxt.File loop
            Comments_Table.Append (File_Comments_Table'(Table => null,
                                                        Priv => <>));
         end loop;
      end if;

      --  Always reset the table.
      File_Comments_Tables.Init (Comments_Table.Table (Ctxt.File), 16);
   end Comment_Init_Scan;

   procedure Comment_Close_Scan is
   begin
      Ctxt.File := No_Source_File_Entry;
   end Comment_Close_Scan;

   --  Gather last comments to the current node.
   --  Called at the end of a block.
   procedure Comment_Gather_Existing
   is
      Fc : File_Comments_Table renames
        Comments_Table.Table (Ctxt.File);
      Last : constant Comment_Index := File_Comments_Tables.Last (Fc);
   begin
      if Flag_Trace then
         Put ("Comment_Gather_Existing: ");
         Put_Uns32 (Uns32 (Ctxt.Next));
         Put ("..");
         Put_Uns32 (Uns32 (Last));
         Put (" -> ");
         Put_Uns32 (Ctxt.Last_Node);
         New_Line;
      end if;

      for I in Ctxt.Next .. Last loop
         pragma Assert (Fc.Table (I).N = 0);
         Fc.Table (I).N := Ctxt.Last_Node;
      end loop;
      Ctxt.Next := Last + 1;
   end Comment_Gather_Existing;

   function Is_Empty_Line (Line_Start : Source_Ptr) return Boolean
   is
      Fc : File_Comments_Table renames
        Comments_Table.Table (Ctxt.File);
      Last : constant Comment_Index := File_Comments_Tables.Last (Fc);
   begin
      --  The start of the line is after the last comment, so the line is
      --  empty.
      return Line_Start > Fc.Table (Last).Last;
   end Is_Empty_Line;

   --  Very important: this procedure is called only after a comment has
   --  been scanned and added.
   --  So this is either the newline after a comment (in that case
   --   LINE_START is less than the last comment),
   --  or an empty line (in that case LINE_START is greater than the last
   --   comment).
   procedure Comment_Newline (Line_Start : Source_Ptr) is
   begin
      case Ctxt.State is
         when State_Before =>
            null;
         when State_Block =>
            --  Detect empty line.
            --  This can happen only after a comments has been added.
            if Is_Empty_Line (Line_Start) then
               --  Attach existing comments.
               Comment_Gather_Existing;
            end if;
         when State_Line =>
            --  If a comment appear before the newline, the state would be
            --  changed to State_Line_Cont; so here no comment on the same
            --  line.
            --
            --  The following comments will be attached to the next node.
            Ctxt.State := State_Before;
         when State_Line_Cont =>
            --  If the line is empty, change to State_Block.
            if Is_Empty_Line (Line_Start) then
               Ctxt.State := State_Block;
            end if;
      end case;
   end Comment_Newline;

   procedure Add_Comment (Start, Last : Source_Ptr;
                          Line_Start : Source_Ptr)
   is
      pragma Assert (Ctxt.File /= No_Source_File_Entry);
      N : Uns32;
   begin
      if Flag_Trace then
         Put ("Add_Comment, file=");
         Put_Uns32 (Uns32 (Ctxt.File));
         Put (", start=");
         Put_Uns32 (Uns32 (Start));
         Put ("..");
         Put_Uns32 (Uns32 (Last));
         Put (" => ");
         Put_Uns32 (Uns32 (File_Comments_Tables.Last
                             (Comments_Table.Table (Ctxt.File)) + 1));
         Put (", state=");
      end if;

      case Ctxt.State is
         when State_Before =>
            --  Will be attached later.
            N := 0;
            if Flag_Trace then
               Put ("before");
            end if;
         when State_Block =>
            --  Will be attached on the next empty line.
            N := 0;
            if Flag_Trace then
               Put ("block");
            end if;
         when State_Line =>
            --  Is it on the same line ?
            if Flag_Trace then
               Put ("line");
               Put (" (start=");
               Put_Uns32 (Uns32 (Ctxt.Line_Start));
               Put (", cmt=");
               Put_Uns32 (Uns32 (Line_Start));
               Put (")");
            end if;
            if Line_Start = Ctxt.Line_Start then
               N := Ctxt.Last_Node;
               Ctxt.Next := File_Comments_Tables.Last
                 (Comments_Table.Table (Ctxt.File)) + 2;
               Ctxt.State := State_Line_Cont;
            else
               --  Not the same line, for the next node.
               N := 0;
               Ctxt.State := State_Before;
            end if;
         when State_Line_Cont =>
            --  Attached on the next empty line.
            if Flag_Trace then
               Put ("line_cont");
            end if;
            N := Ctxt.Last_Node;
            Ctxt.Next := File_Comments_Tables.Last
              (Comments_Table.Table (Ctxt.File)) + 2;
      end case;

      if Flag_Trace then
         Put (", node=");
         Put_Uns32 (N);
         New_Line;
      end if;

      --  Append a comment entry.
      File_Comments_Tables.Append
        (Comments_Table.Table (Ctxt.File),
         Comment_Record'(Start => Start, Last => Last, N => N));
   end Add_Comment;

   procedure Save_Comments (Rng : out Comments_Range)
   is
      use File_Comments_Tables;
      pragma Assert (Ctxt.File /= No_Source_File_Entry);
      Fc : File_Comments_Table renames Comments_Table.Table (Ctxt.File);
   begin
      Rng := (First => Ctxt.Next, Last => Last (Fc));
      Ctxt.Next := Rng.Last + 1;
   end Save_Comments;

   procedure Gather_Comments_Before (Rng : Comments_Range; N : Uns32)
   is
      use File_Comments_Tables;
      pragma Assert (Ctxt.File /= No_Source_File_Entry);
   begin
      if Rng.Last /= No_Comment_Index then
         if Flag_Trace then
            Put ("Gather_Comments_Before, file=");
            Put_Uns32 (Uns32 (Ctxt.File));
            Put (", rng=");
            Put_Uns32 (Uns32 (Rng.First));
            Put ("..");
            Put_Uns32 (Uns32 (Rng.Last));
            Put (", node=");
            Put_Uns32 (N);
            New_Line;
         end if;

         declare
            Fc : File_Comments_Table renames Comments_Table.Table (Ctxt.File);
         begin
            for I in Rng.First .. Rng.Last loop
               Fc.Table (I).N := N;
            end loop;

            Ctxt.Next := Rng.Last + 1;
         end;
      end if;
   end Gather_Comments_Before;

   procedure Gather_Comments_Block (Rng : Comments_Range; N : Uns32) is
   begin
      Gather_Comments_Before (Rng, N);
      Ctxt.State := State_Block;
      Ctxt.Last_Node := N;
   end Gather_Comments_Block;

   procedure Gather_Comments_Line (Rng : Comments_Range;
                                   Pos : Source_Ptr;
                                   N : Uns32) is
   begin
      Gather_Comments_Before (Rng, N);
      Ctxt.State := State_Line;
      Ctxt.Last_Node := N;
      Ctxt.Line_Start := Pos;
   end Gather_Comments_Line;

   procedure Gather_Comments_End is
   begin
      case Ctxt.State is
         when State_Before =>
            --  Discard unattached comments.
            declare
               Fc : File_Comments_Table renames
                 Comments_Table.Table (Ctxt.File);
               Last : Comment_Index;
            begin
               loop
                  Last := File_Comments_Tables.Last (Fc);
                  exit when Last = No_Comment_Index;
                  exit when Fc.Table (Last).N /= 0;
                  File_Comments_Tables.Decrement_Last (Fc);
               end loop;
            end;
         when State_Block =>
            Comment_Gather_Existing;
         when State_Line
           | State_Line_Cont =>
            --  All comments are attached.
            null;
      end case;
      Ctxt.State := State_Before;
   end Gather_Comments_End;

   procedure Gather_Comments (N : Uns32)
   is
      Rng : Comments_Range;
   begin
      Save_Comments (Rng);
      Gather_Comments_Block (Rng, N);
   end Gather_Comments;

   procedure Sort_Comments_By_Node
   is
      pragma Assert (Ctxt.File /= No_Source_File_Entry);
      Fc : File_Comments_Table renames Comments_Table.Table (Ctxt.File);

      function Lt (L, R : Positive) return Boolean
      is
         Lc : Comment_Record renames Fc.Table (Comment_Index (L));
         Rc : Comment_Record renames Fc.Table (Comment_Index (R));
      begin
         if Lc.N < Rc.N then
            return True;
         elsif Lc.N = Rc.N then
            return Lc.Start < Rc.Start;
         end if;
         return False;
      end Lt;

      procedure Swap (P1 : Positive; P2 : Positive)
      is
         L : Comment_Record renames Fc.Table (Comment_Index (P1));
         R : Comment_Record renames Fc.Table (Comment_Index (P2));
         T : Comment_Record;
      begin
         T := L;
         L := R;
         R := T;
      end Swap;

      procedure Sort is new Grt.Algos.Heap_Sort
        (Lt => Lt, Swap => Swap);
   begin
      Sort (Natural (File_Comments_Tables.Last (Fc)));
   end Sort_Comments_By_Node;

   function Find_First_Comment (File : Source_File_Entry; N : Uns32)
                               return Comment_Index is
   begin
      if Comments_Table.Last < File then
         --  No comments for FILE.
         return No_Comment_Index;
      end if;
      declare
         Fc : File_Comments_Table renames Comments_Table.Table (File);
         Nd : Uns32;
         F, L, M : Comment_Index;
      begin
         F := File_Comments_Tables.First;
         L := File_Comments_Tables.Last (Fc);
         while F <= L loop
            M := F + (L - F) / 2;
            Nd := Fc.Table (M).N;
            if Nd = N then
               --  Found, but must return the first comment.
               while M > No_Comment_Index + 1
                 and then Fc.Table (M - 1).N = N
               loop
                  M := M - 1;
               end loop;
               return M;
            elsif Nd < N then
               F := M + 1;
            else
               pragma Assert (Nd > N);
               L := M - 1;
            end if;
         end loop;
         return No_Comment_Index;
      end;
   end Find_First_Comment;

   procedure Get_Comment (File : Source_File_Entry;
                          Idx : Comment_Index;
                          Start, Last : out Source_Ptr)
   is
      pragma Assert (Comments_Table.Last >= File);
      Fc : File_Comments_Table renames Comments_Table.Table (File);
   begin
      Start := Fc.Table (Idx).Start;
      Last := Fc.Table (Idx).Last;
   end Get_Comment;

   function Get_Comment_Start (File : Source_File_Entry;
                               Idx : Comment_Index) return Source_Ptr
   is
      Start, Last : Source_Ptr;
   begin
      Get_Comment (File, Idx, Start, Last);
      return Start;
   end Get_Comment_Start;

   function Get_Comment_Last (File : Source_File_Entry;
                              Idx : Comment_Index) return Source_Ptr
   is
      Start, Last : Source_Ptr;
   begin
      Get_Comment (File, Idx, Start, Last);
      return Last;
   end Get_Comment_Last;

   function Get_Next_Comment (File : Source_File_Entry; Idx : Comment_Index)
                             return Comment_Index
   is
      use File_Comments_Tables;
      pragma Assert (Comments_Table.Last >= File);
      Fc : File_Comments_Table renames Comments_Table.Table (File);
   begin
      if Idx < Last (Fc)
        and then Fc.Table (Idx + 1).N = Fc.Table (Idx).N
      then
         return Idx + 1;
      else
         return No_Comment_Index;
      end if;
   end Get_Next_Comment;

   procedure Finalize is
   begin
      for I in Comments_Table.First .. Comments_Table.Last loop
         File_Comments_Tables.Free (Comments_Table.Table (I));
      end loop;
      Comments_Table.Free;
   end Finalize;

   procedure Initialize is
   begin
      Comments_Table.Init;
   end Initialize;
end File_Comments;
