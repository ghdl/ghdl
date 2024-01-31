entity repro1b is
  port (a : bit_vector (31 downto 0));
end;

architecture behav of repro1b is
begin
end;

entity repro1 is
end;

architecture behav of repro1 is
  signal v1, v2 : bit_vector (15 downto 0);
begin
  dut: entity work.repro1b
    port map (
      a (15 downto 0) => v1,
      a (0) => v1 (0),
      a (31 downto 16) => v2);
end;
