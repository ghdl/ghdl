entity repro2b is
  generic (type t);
  port (s : t);
end repro2b;

architecture behav of repro2b is
begin
end behav;

entity repro2 is
  generic (type t);
  port (s : t);
end repro2;

architecture behav of repro2 is
begin
  inst: entity work.repro2b
    generic map (t => t)
    port map (s => s);
end behav;
