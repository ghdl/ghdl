entity repro1 is
  generic (gf : boolean := false);
end;

architecture behav of repro1 is
  component comp1 is
    port (a : bit);
  end component;

  component comp2 is
    port (a : bit);
  end component;

  signal s : bit;
begin
  g1 : if gf generate
    i1 : comp1 port map (a => s);
  end generate;
  
  i2 : comp2 port map (a => s);

  s <= '0', '1' after 1 ns;
end;

