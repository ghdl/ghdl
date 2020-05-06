entity tb_testcase is
end tb_testcase;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_testcase is
  signal din  : std_logic_vector (3 downto 0);
  signal dout : std_logic_vector (1 downto 0);
begin
  dut: entity work.testcase
    port map (din, dout);

  process
  begin
    din <= "0001";
    wait for 1 ns;
    assert dout = "01" severity failure;

    din <= "0010";
    wait for 1 ns;
    assert dout = "10" severity failure;

    din <= "1010";
    wait for 1 ns;
    assert dout = "01" severity failure;

    din <= "1001";
    wait for 1 ns;
    assert dout = "00" severity failure;

    wait;
  end process;
end behav;
