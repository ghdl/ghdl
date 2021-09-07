entity repro1 is
  port (p : string);
end repro1;

architecture behav of repro1 is
begin
  assert p = "hello" severity failure;
end behav;

entity repro is
  generic (g : string := "hello");
end repro;

architecture behav of repro is
begin

  comp : entity work.repro1 port map (p => g);
end behav;
