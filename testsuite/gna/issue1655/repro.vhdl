entity repro_ch is
  generic (v : natural);
  port (i : bit);
end;

architecture behav of repro_ch is
begin
  assert v > 5;
end;

entity repro is
end;

architecture behav of repro is
  component comp is
    port (i : bit);
  end component;

  signal s : bit;

  for inst : comp use entity work.repro_ch(behav);
begin
  inst: comp port map (i => s);
end;
