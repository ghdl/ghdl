entity repro2s is
  generic (g : string := "hello");
  port (p : string := g);
end repro2s;

architecture behav of repro2s is
begin
  assert p = "hello" severity failure;
end behav;

entity repro2 is
  generic (g : string := "hello");
end repro2;

architecture behav of repro2 is
begin

  comp : entity work.repro2s port map (p => open);
end behav;
