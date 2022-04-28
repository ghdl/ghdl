entity tb_ent1 is
end tb_ent1;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_ent1 is
  signal clk, rst : std_logic;
  signal inp : std_logic_vector(15 downto 0);
  signal data : std_logic_vector(63 downto 0);
begin
  dut: entity work.ent1
    port map (clk, rst, inp, data);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    rst <= '1';
    pulse;

    rst <= '0';
    inp <= x"a001";
    pulse;
    inp <= x"b002";
    pulse;
    inp <= x"c003";
    pulse;
    inp <= x"d004";
    pulse;
    assert data = x"d004c003b002a001" severity failure;
    wait;
  end process;
end behav;
