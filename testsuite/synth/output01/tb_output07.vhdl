library ieee;
use ieee.std_logic_1164.all;

entity tb_output07 is
end tb_output07;

architecture behav of tb_output07 is
  signal i : std_logic;
  signal clk : std_logic;
  signal o : std_logic_vector (1 downto 0);
begin
  inst: entity work.output07
    port map (clk => clk, i => i, o => o);

  process
    procedure pulse is
    begin
      wait for 1 ns;
      clk <= '1';
      wait for 1 ns;
      clk <= '0';
    end pulse;
  begin
    clk <= '0';
    wait for 1 ns;
    assert o = "10" severity failure;

    i <= '1';
    pulse;
    assert o = "01" severity failure;

    i <= '0';
    pulse;
    assert o = "10" severity failure;

    wait;
  end process;
end behav;

