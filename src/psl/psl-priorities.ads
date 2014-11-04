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
