library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_ent is
end;

architecture behav of tb_ent is
  signal r : unsigned (31 downto 0);
begin
  dut: entity work.ent
    generic map (g => x"ffff_0001")
    port map (res => r);

  process
  begin
    wait for 1 ns;
    assert r = x"ffff0001" severity failure;
    wait;
  end process;
end behav;

