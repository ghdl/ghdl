--  PSL - Operator priorities
--  Copyright (C) 2002-2016 Tristan Gingold
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

package PSL.Priorities is
   --  Operator priorities, defined by PSL1.1 4.2.3.2
   type Priority is
     (
      Prio_Lowest,

      --  always, never, G
      Prio_FL_Invariance,

      --  ->, <->
      Prio_Bool_Imp,

      --  |->, |=>
      Prio_Seq_Imp,

      --  U, W, until*, before*
      Prio_FL_Bounding,

      --  next*, eventually!, X, X!, F
      Prio_FL_Occurence,

      --  abort
      Prio_FL_Abort,

      --  ( )
      Prio_FL_Paren,

      --  ;
      Prio_Seq_Concat,

      --  :
      Prio_Seq_Fusion,

      --  |
      Prio_Seq_Or,

      --  &, &&
      Prio_Seq_And,

      --  within
      Prio_Seq_Within,

      --  [*], [+], [=], [->]
      Prio_SERE_Repeat,

      --  { }
      Prio_SERE_Brace,

      --  @
      Prio_Clock_Event,

      --  !
      Prio_Strong,

      --  union
      Prio_Union,

      --  !
      Prio_Bool_Not,

      Prio_HDL
     );
end PSL.Priorities;
