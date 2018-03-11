entity TOP is
end entity TOP;

architecture ARCH of TOP is
  signal S1, S2, S3: BIT;
  alias DONE_SIG is <<signal .TOP.DUT.DONE: BIT>>; -- Legal
  constant DATA_WIDTH: INTEGER:= <<signal .TOP.DUT.DATA: BIT_VECTOR>>'LENGTH;
    -- Illegal, because .TOP.DUT.DATA has not yet been elaborated
    --  when the expression is evaluated
begin
  P1: process ( DONE_SIG ) is -- Legal
  begin
    if DONE_SIG then -- Legal ...;
    end if;
  end process P1;

  MONITOR: entity WORK.MY_MONITOR port map (DONE_SIG);
  -- Illegal, because .TOP.DUT.DONE has not yet been elaborated
  -- when the association element is elaborated
  DUT: entity WORK.MY_DESIGN port map (s1, S2, S3);
  MONITOR2: entity WORK.MY_MONITOR port map (DONE_SIG);
  -- Legal, because .TOP.DUT.DONE has now been elaborated
  B1: block
    constant DATA_WIDTH: INTEGER := <<signal .TOP.DUT.DATA: BIT_VECTOR>>'LENGTH
    -- Legal, because .TOP.DUT.DATA has now been elaborated
  begin
  end block B1;
  B2: block
    constant C0: INTEGER := 6;
    constant C1: INTEGER := <<constant .TOP.B3.C2: INTEGER>>;
    -- Illegal, because .TOP.B3.C2 has not yet been elaborated
  begin
  end block B2;
  B3: block
    constant C2: INTEGER := <<constant .TOP.B2.C0: INTEGER>>; -- Legal
  begin
  end block B3;
  -- Together, B2 and B3 are illegal, because they cannot be ordered
  -- so that the objects are elaborated in the order .TOP.B2.C0,
  -- then .TOP.B3.C2, and finally .TOP.B2.C1.
end architecture ARCH;
