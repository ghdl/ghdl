entity repro4b is
  port (a : bit_vector (31 downto 0));
end;

architecture behav of repro4b is
begin
end;

entity repro4 is
end;

architecture behav of repro4 is
  signal v1, v2 : bit_vector (15 downto 0);
begin
  dut: entity work.repro4b
    port map (
      a (15 downto 0) => v1,
      a (23 downto 8) => v2,
      a (31 downto 24) => v2(7 downto 0));
end;
