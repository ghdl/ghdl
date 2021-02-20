entity repro1_ch is
  generic (v : natural);
  port (i : bit);
end;

architecture behav of repro1_ch is
begin
  assert v > 5;
end;

entity repro1 is
end;

architecture behav of repro1 is
  component comp is
    generic (v : natural);
    port (i : bit);
  end component;

  signal s : bit;

  for inst : comp use entity work.repro1_ch(behav);
begin
  inst: comp port map (i => s);
end;
