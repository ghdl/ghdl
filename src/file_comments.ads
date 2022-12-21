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

with Types; use Types;
with Dyn_Tables;
with Tables;

package File_Comments is
   --  Usage of File_Comments:
   --  There are two parts: the generating part which gather comments and
   --  associate them to nodes, and the user part which allow to get comments
   --  for a node.
   --
   --  The generating part is combined work done by the scanner and the
   --  parser.  The scanner calls Add_Comment on evey comment, and
   --  Comment_Newline on every newlines after a comment.
   --  The parser does the remaining work: it initializes and finishes the
   --  process (calls Comment_Init_Scan and Comment_Close_Scan).
   --  It also associate comments with nodes.
   --
   --  There are two modes of association: block and line.
   --
   --  Line is the simplest mode: it starts by calling Gather_Comments_Line.
   --  First, it associates previous comments to the node, and then if a
   --  comment appear on the same line as the node, all consecutive comments
   --  are associated with the node.  Consecutive comments mean comments
   --  without empty lines.  Another declaration or statement will also
   --  interrupt this association because the comments will be associated
   --  with this new declaration or statement.  After interruption, comments
   --  are not associated anymore; they will be associated by the next
   --  call.  Finally, Gather_Comments_End will simply discard unassociated
   --  comments that appears at an end (or before an 'end').
   --
   --  Block is the default mode.  Gathered but unassociated comments are
   --  simply associated with a node.  The following comments are also
   --  associated with the current node only when an empty line appears.
   --  The block mode is made more complex by the possibility of saving
   --  a range of comments because parsing and scanning needs to be
   --  continued before building a node (eg: to distinguish package
   --  declaration and package instantiation, or to distinguish process and
   --  sensitized process).
   --
   --  Before use, Sort_Comments_By_Node must be called to sort comments.
   --  Then the iterator can be called to get the comments associated to a
   --  node.

   --  To be called at begin/end of scan to initialize the context.
   --  TODO: nested context ?
   procedure Comment_Init_Scan (File : Source_File_Entry);
   procedure Comment_Close_Scan;

   --  Add a comment for FILE.
   --  This procedure is called from a scanner when a comment is scanned.
   --
   --  For a line comment, START is the position of the token that starts the
   --  comment (the '--' in vhdl).  LAST is the position of the last character
   --  of the comment (before the new line).
   --  LINE_START is the start of the current line (to detect comments in
   --  the same line as a node).
   procedure Add_Comment (Start, Last : Source_Ptr;
                          Line_Start : Source_Ptr);

   --  A newline *after a comment* has been scanned.
   --  If this is a blank line, comments before the blank line are attached
   --  to the previous node.
   procedure Comment_Newline (Line_Start : Source_Ptr);

   type Comments_Range is private;

   --  Save comments recently scanned and not yet gathered.
   procedure Save_Comments (Rng : out Comments_Range);

   --  Assign node N to the saved RNG comments.
   --  This procedure is called by the parser when a node that could be
   --  annotated with a comment is parsed.
   procedure Gather_Comments_Block (Rng : Comments_Range;
                                    N : Uns32);
   procedure Gather_Comments_Line (Pos : Source_Ptr;
                                   N : Uns32);

   --  Assign node N to the last comments scanned.
   --  Identical to Save_Comments followed by above Gather_Comments.
   procedure Gather_Comments (N : Uns32);

   --  To be called at the end of a lexical block.
   --  Assign last comments to the block (if any).
   procedure Gather_Comments_End;

   --  Sort comments; to be done once all comments have been gathered and
   --  before searching comments.
   --  Discard unassigned comments ?
   procedure Sort_Comments_By_Node;

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

   --  Simpler functions for python binding.
   function Get_Comment_Start (File : Source_File_Entry;
                               Idx : Comment_Index) return Source_Ptr;
   function Get_Comment_Last (File : Source_File_Entry;
                              Idx : Comment_Index) return Source_Ptr;

   --  Return the next comment after IDX.
   --  Return No_Comment_Index if no related comment exists.
   function Get_Next_Comment (File : Source_File_Entry;
                              Idx : Comment_Index)
                             return Comment_Index;

   --  For the whole package.
   procedure Initialize;
   procedure Finalize;
private
   type Comments_Range is record
      --  Range of saved comments.
      First, Last : Comment_Index;
   end record;

   type Comment_Record is record
      --  Comment range in the source.
      Start : Source_Ptr;
      Last : Source_Ptr;

      --  Associated node.
      N : Uns32;
   end record;

   type Comment_State is
     (
      --  Keep comments, to be attached.
      --  This is the initial state.
      State_Before,

      --  Comments until the first newline are attached to LAST_NODE.
      State_Block,

      --  If the next comment is on the same line, it will be attached to
      --  LAST_NODE, and so will be the next comments.
      State_Line,

      --  Continuation of line.  Any comment is attached to the LAST_NODE,
      --  until an empty line.
      State_Line_Cont
     );

   type Comment_Context is record
      --  Current file.
      File : Source_File_Entry;

      --  Current state.
      State : Comment_State;

      --  Next unassigned comment.
      Next : Comment_Index;

      Last_Newline : Comment_Index;

      --  Node to attach for next comments.
      Last_Node : Uns32;

      Line_Start : Source_Ptr;
   end record;

   package File_Comments_Tables is new Dyn_Tables
     (Table_Component_Type => Comment_Record,
      Table_Index_Type => Comment_Index,
      Table_Low_Bound => 1);

   subtype File_Comments_Table is File_Comments_Tables.Instance;

   --  Table of comments, indexed by files.
   package Comments_Table is new Tables
     (Table_Component_Type => File_Comments_Table,
      Table_Index_Type => Source_File_Entry,
      Table_Low_Bound => No_Source_File_Entry + 1,
      Table_Initial => 8);
end File_Comments;
