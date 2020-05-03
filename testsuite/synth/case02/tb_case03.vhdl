entity tb_case03 is
end tb_case03;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_case03 is
  signal clk : std_logic;
  signal opc : std_logic_vector (2 downto 0);
  signal arg : std_logic_vector (7 downto 0);
  signal res : std_logic_vector (7 downto 0);
  signal par : std_logic;
begin
  dut: entity work.case03
    port map (clk, opc, arg, res, par);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    opc <= "000";
    arg <= x"45";
    pulse;
    assert res = x"ba" severity failure;
    assert par = '1' severity failure;

    opc <= "010";
    arg <= x"55";
    pulse;
    assert res = x"aa" severity failure;
    assert par = '0' severity failure;

    opc <= "110";
    arg <= x"57";
    pulse;
    assert res = x"57" severity failure;
    assert par = '0' severity failure;

    wait;
  end process;
end behav;
