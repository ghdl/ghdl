entity uncons1 is
end;

architecture behav of uncons1 is
  signal s1, s2 : bit;
begin
  b : block
     -- port (p : bit_vector := (others => '1'));
     port (p : bit_vector := "01110");
     port map (p(0) => s1, p(1) => s2);
  begin
  end block;
end;
