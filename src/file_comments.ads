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

with Types; use Types;
with Dyn_Tables;
with Tables;

package File_Comments is
   --  Add a comment for FILE.
   --  This procedure is called from a scanner when a comment is scanned.
   --
   --  For a line comment, START is the position of the token that starts the
   --  comment (the '--' in vhdl).  LAST is the position of the last character
   --  of the comment (before the new line).
   procedure Add_Comment (File : Source_File_Entry;
                          Start, Last : Source_Ptr);

   --  Discard unassigned comments ?
   procedure Discard_Comments (File : Source_File_Entry);

   --  Assign node N to the last comments scanned.
   --  This procedure is called by the parser when a node that could be
   --  annotated with a comment is parsed.
   procedure Gather_Comments (File : Source_File_Entry;
                              N : Uns32);

   --  Reassign comments to node N.
   procedure Rename_Comments (File : Source_File_Entry;
                              Prev : Uns32;
                              N : Uns32);

   --  Sort comments; to be done once all comments have been gathered and
   --  before searching comments.
   --  Discard unassigned comments ?
   procedure Sort_Comments_By_Node (File : Source_File_Entry);

   type Comment_Index is new Nat32;
   No_Comment_Index : constant Comment_Index := 0;

   --  Return the first comment index for node N.
   --  Return No_Comment_Index if not found.
   function Find_First_Comment (File : Source_File_Entry; N : Uns32)
                               return Comment_Index;

   --  Return the source bounds of comment IDX.
   procedure Get_Comment (File : Source_File_Entry;
                          Idx : Comment_Index;
                          Start, Last : out Source_Ptr);

   --  Return the next comment after IDX.
   --  Return No_Comment_Index if no related comment exists.
   function Get_Next_Comment (File : Source_File_Entry;
                              Idx : Comment_Index)
                             return Comment_Index;
private
   type Comment_Record is record
      --  Comment range in the source.
      Start : Source_Ptr;
      Last : Source_Ptr;

      --  Associated node.
      N : Uns32;
   end record;

   package File_Comments_Tables is new Dyn_Tables
     (Table_Component_Type => Comment_Record,
      Table_Index_Type => Comment_Index,
      Table_Low_Bound => 1);

   type File_Comment_Record is record
      --  Table of comments for a file.
      Comments : File_Comments_Tables.Instance;
      --  Next unassigned comment.
      Next : Comment_Index;
   end record;

   --  Table of comments, indexed by files.
   package Comments_Table is new Tables
     (Table_Component_Type => File_Comment_Record,
      Table_Index_Type => Source_File_Entry,
      Table_Low_Bound => No_Source_File_Entry + 1,
      Table_Initial => 8);
end File_Comments;
