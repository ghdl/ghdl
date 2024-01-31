entity repro2b is
  port (a : bit_vector (31 downto 0));
end;

architecture behav of repro2b is
begin
end;

entity repro2 is
end;

architecture behav of repro2 is
  signal v1, v2 : bit_vector (15 downto 0);
begin
  dut: entity work.repro2b
    port map (
      a (0) => v1 (0),
      a (15 downto 0) => v1,
      a (31 downto 16) => v2);
end;
