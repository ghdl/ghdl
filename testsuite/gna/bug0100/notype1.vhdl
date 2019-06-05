entity sub1 is
  port (i : bit_vector);
end;

entity notype1 is
end;

architecture behav of notype1 is
  type counter_t is array (2 downto 0) of (31 downto 0);
  signal cnts : counter_t;
begin
  i : entity work.sub1
    port map (i => cnts (0));
end;
