--  PSL - Operator priorities
--  Copyright (C) 2002-2016 Tristan Gingold
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
--  along with GHDL; see the file COPYING.  If not, write to the Free
--  Software Foundation, 59 Temple Place - Suite 330, Boston, MA
--  02111-1307, USA.

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
