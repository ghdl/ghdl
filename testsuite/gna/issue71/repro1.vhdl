entity repro1 is
end repro1;

architecture behav of repro1 is
  signal s1, s2 : bit;

  component comp port (i : in bit; o : out bit);
  end component;
begin

  s1 <= '1';

  c : comp port map (i => s1, o => s2);
end behav;
