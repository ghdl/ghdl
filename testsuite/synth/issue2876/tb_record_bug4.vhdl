library ieee;
use ieee.std_logic_1164.all;
use std.env.finish;

entity tb_record_bug4 is
  generic (
    NEL1 : natural := 3;
    NEL2 : natural := 4);
end;

architecture arch of tb_record_bug4 is

signal clk  : std_logic  := '0';
signal rst  : std_logic  := '1';
signal test : std_logic_vector(7 downto 0);

begin

  uut : entity work.record_bug4
    generic map
    (NEL1 => NEL1, NEL2 => NEL2)
    port map (
        clk_i   => clk,
        rst_i   => rst,
        test_o  => test
    );

clk <= not clk after 5 ns;
rst <= '0' after 20 ns;

process
begin
    wait for 40 ns + NEL2 * 20 ns;
    assert test = X"AA" report "Wrong test signal value " & to_hstring(test) severity failure;
    report "Test OK!";
    finish;
end process;

end arch;
