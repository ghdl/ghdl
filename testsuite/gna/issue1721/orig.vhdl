entity top5 is
end entity;

architecture a of top5 is
    signal a,b,c,d : std_logic := '0';
    signal clk_sys, clk1, clk2 : std_logic;
begin

  -- Following throws bug occured with:
  --  "psl.sem_property: cannot handle N_CLOCKED_SER"
  -- Clocked SERE shall be allowed according to 6.1.1.1 of PSL LRM 2003
  -- psl my_seq : assert never {a;b;c} @ rising_edge(clk_sys);

  -- Following throws bug occured with: "build_sere_fa: cannot handle N_IMP_SEQ"
  -- This is strange because with "always" this is working.
  -- According to PSL LRM 2003 FL_Property production for "always" is the
  -- same as for "never":
  --   FL_Property ::=
  --        always FL_Property
  --      | always Sequence
  --
  --   FL_Property ::=
  --        never FL_Property
  --      | never Sequence
  -- Therefore I think if sequence implication works with one, it shall work
  -- also with other.
  -- psl my_seq : assert never {a} |=> {b; c};

  -- Following error is thrown: "translate_psl_expr: cannot handle N_IMP_BOOL"
  -- Combination of "never" + implication is not very usefull, since
  -- implication is always true apart from case where 1 -> 0, therefore it
  -- will mostly fail, however, it should not crash tool (the same goes for
  -- previous case too)
  -- psl my_seq : assert never (a -> b);

  -- Following error is thrown:
  --  "top.vhd:43:28:error: operand of a 'never' operator must be a boolean
  --   or a sequence"
  -- While for "assert always", this works. IMHO this should work for both,
  -- equally since productions for never|always are the same.
  -- It is not true that operand of never must be a boolean or a sequence.
  -- It can be FL_property or sequence, which is much wider set.
  -- psl my_seq : assert never (a) -> eventually! (b);
  
  -- Following error is thrown:
  -- "top.vhd:43:58:error: left-hand side operand of logical '->' must be
  --  a boolean"
  -- This is not true, LHS and RHS of implication shall be FL_Property, not
  -- just boolean (6.2.1.6.1)
  -- psl my_seq : assert always (a -> next b -> next c -> next d);

end;
