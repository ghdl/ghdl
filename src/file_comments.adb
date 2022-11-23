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

--  All the variables declared in this package are set by Parse_Option function
--  and can by read as soon as the command line is parsed.
--
--  Since the names are not prefixed, this package is expected to be with'ed
--  but not to be use'd.

with Grt.Algos;

package body File_Comments is
   procedure Add_Comment (File : Source_File_Entry;
                          Start, Last : Source_Ptr)
   is
      pragma Assert (File > No_Source_File_Entry);
   begin
      --  Create entry for FILE if not already created.
      if Comments_Table.Last < File then
         while Comments_Table.Last < File loop
            Comments_Table.Append
              (File_Comment_Record'(Comments => <>,
                                    Next => File_Comments_Tables.First));
         end loop;
         File_Comments_Tables.Init (Comments_Table.Table (File).Comments, 16);
      end if;

      --  Append a comment entry.
      File_Comments_Tables.Append
        (Comments_Table.Table (File).Comments,
         Comment_Record'(Start => Start, Last => Last, N => 0));
   end Add_Comment;

   procedure Discard_Comments (File : Source_File_Entry) is
   begin
      if Comments_Table.Last < File then
         --  No comments for FILE.
         return;
      end if;
      raise Internal_Error;
   end Discard_Comments;

   procedure Save_Comments (File : Source_File_Entry;
                            Rng : out Comments_Range_Type)
   is
      use File_Comments_Tables;
   begin
      if Comments_Table.Last < File then
         --  No comments for FILE.
         Rng := (First | Last => No_Comment_Index);
         return;
      end if;
      declare
         Fc : File_Comment_Record renames Comments_Table.Table (File);
      begin
         Rng := (First => Fc.Next, Last => Last (Fc.Comments));
         Fc.Next := Rng.Last + 1;
      end;
   end Save_Comments;

   procedure Gather_Comments (File : Source_File_Entry;
                              Rng : Comments_Range_Type;
                              N : Uns32)
   is
      use File_Comments_Tables;
   begin
      if Rng.Last = No_Comment_Index then
         return;
      end if;

      pragma Assert (File <= Comments_Table.Last);
      declare
         Fc : File_Comment_Record renames Comments_Table.Table (File);
      begin
         for I in Rng.First .. Rng.Last loop
            Fc.Comments.Table (I).N := N;
         end loop;
      end;
   end Gather_Comments;

   procedure Gather_Comments (File : Source_File_Entry; N : Uns32)
   is
      Rng : Comments_Range_Type;
   begin
      Save_Comments (File, Rng);
      Gather_Comments (File, Rng, N);
   end Gather_Comments;

   procedure Rename_Comments (File : Source_File_Entry;
                              Prev : Uns32;
                              N : Uns32) is
   begin
      raise Internal_Error;
   end Rename_Comments;

   procedure Sort_Comments_By_Node_1 (Fc : File_Comment_Record)
   is
      function Lt (L, R : Positive) return Boolean
      is
         Lc : Comment_Record renames Fc.Comments.Table (Comment_Index (L));
         Rc : Comment_Record renames Fc.Comments.Table (Comment_Index (R));
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
         L : Comment_Record renames Fc.Comments.Table (Comment_Index (P1));
         R : Comment_Record renames Fc.Comments.Table (Comment_Index (P2));
         T : Comment_Record;
      begin
         T := L;
         L := R;
         R := T;
      end Swap;

      procedure Sort is new Grt.Algos.Heap_Sort
        (Lt => Lt, Swap => Swap);
   begin
      Sort (Natural (File_Comments_Tables.Last (Fc.Comments)));
   end Sort_Comments_By_Node_1;

   procedure Sort_Comments_By_Node (File : Source_File_Entry) is
   begin
      if File > Comments_Table.Last then
         --  No comments gathered, nothing to do.
         return;
      end if;
      Sort_Comments_By_Node_1 (Comments_Table.Table (File));
   end Sort_Comments_By_Node;

   function Find_First_Comment (File : Source_File_Entry; N : Uns32)
                               return Comment_Index is
   begin
      if Comments_Table.Last < File then
         --  No comments for FILE.
         return No_Comment_Index;
      end if;
      declare
         Fc : File_Comment_Record renames Comments_Table.Table (File);
         Nd : Uns32;
         F, L, M : Comment_Index;
      begin
         F := File_Comments_Tables.First;
         L := File_Comments_Tables.Last (Fc.Comments);
         while F <= L loop
            M := F + (L - F) / 2;
            Nd := Fc.Comments.Table (M).N;
            if Nd = N then
               --  Found, but must return the first comment.
               while M > No_Comment_Index + 1
                 and then Fc.Comments.Table (M - 1).N = N
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
      Fc : File_Comment_Record renames Comments_Table.Table (File);
   begin
      Start := Fc.Comments.Table (Idx).Start;
      Last := Fc.Comments.Table (Idx).Last;
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
      Fc : File_Comment_Record renames Comments_Table.Table (File);
   begin
      if Idx < Last (Fc.Comments)
        and then Fc.Comments.Table (Idx + 1).N = Fc.Comments.Table (Idx).N
      then
         return Idx + 1;
      else
         return No_Comment_Index;
      end if;
   end Get_Next_Comment;
end File_Comments;
