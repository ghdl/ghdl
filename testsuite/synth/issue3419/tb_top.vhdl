library ieee;
use ieee.std_logic_1164.all;

entity tb_top is
end entity;

architecture a of tb_top is
  signal io : std_logic_vector(1 downto 0);
begin
  dut : entity work.top port map (io => io);

  process
  begin
    wait for 1 ns;
    report to_string(io);
    assert io = "10" severity failure;
    wait;
  end process;
end architecture;
