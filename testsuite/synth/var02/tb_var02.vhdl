entity tb_var02 is
end tb_var02;

library ieee;
use ieee.std_logic_1164.all;

architecture behav of tb_var02 is
  signal clk : std_logic;
  signal sel : std_logic_vector (3 downto 0);
  signal res : std_logic_vector (7 downto 0);
begin
  dut: entity work.var02
    port map (
      clk => clk,
      sel => sel,
      res => res);

  process
    procedure pulse is
    begin
      clk <= '0';
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
    end pulse;
  begin
    pulse;
    assert res = x"03" report "res=" & to_hstring (res) severity failure;

    wait;
  end process;
end behav;
