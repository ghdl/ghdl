entity tb_issue is
end tb_issue;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_issue is
  signal f : std_logic;
  signal b : std_logic_vector (7 downto 0);
begin
  dut: entity work.issue
    port map (f, b);

  process
  begin
    f <= '1';
    wait for 1 ns;
    assert b = b"1000_0000" severity failure;

    f <= '0';
    wait for 1 ns;
    assert b = b"0000_0000" severity failure;

    wait;
  end process;
end behav;
