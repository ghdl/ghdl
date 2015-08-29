entity repro is
end repro;

architecture behav of repro is
  component comp is
    port (s : bit);
  end component;
  signal s : bit;
begin
  c : comp port map (s);
end behav;

entity comp is
  port (s : bit);
end comp;

configuration conf of repro is
  for behav
   for c : comp
     use entity work.compx (behav);
   end for;
  end for;
end conf;

architecture behav of comp is
begin
  assert s = '1';
end behav;

