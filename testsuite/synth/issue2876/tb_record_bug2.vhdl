library ieee;
use ieee.std_logic_1164.all;
use std.env.finish;

entity tb_record_bug2 is
end;

architecture arch of tb_record_bug2 is

signal clk  : std_logic  := '0';
signal rst  : std_logic  := '1';
signal test : std_logic_vector(7 downto 0);

begin

uut : entity work.record_bug2
    port map (
        clk_i   => clk,
        rst_i   => rst,
        test_o  => test
    );

clk <= not clk after 5 ns;
rst <= '0' after 20 ns;

--  process (clk)
--  begin
--    if rising_edge(clk) then
--      report "i=" & to_hstring(<<signal uut.i : std_logic_vector(1 downto 0)>>) & ", j=" & to_hstring(<<signal uut.j : std_logic_vector(1 downto 0)>>);
--      report "v=" & to_hstring(<<signal uut.v : std_logic_vector(95 downto 0)>>);
--    end if;
--  end process;

--  Result:
-- @35ns:(report note): i=0, j=0
-- @35ns:(report note): v=000000BB0000000000000000
-- @45ns:(report note): i=1, j=0
-- @45ns:(report note): v=AA0000BB0000000000000000
-- @55ns:(report note): i=2, j=0
-- @55ns:(report note): v=AA0000BBAA00000000000000
-- @65ns:(report note): i=0, j=1
-- @65ns:(report note): v=AA0000BBAA000000AA000000
-- @75ns:(report note): i=1, j=1
-- @75ns:(report note): v=AAAA00BBAA000000AA000000
-- @85ns:(report note): i=2, j=1
-- @85ns:(report note): v=AAAA00BBAAAA0000AA000000
-- @95ns:(report note): i=0, j=2
-- @95ns:(report note): v=AAAA00BBAAAA0000AAAA0000
-- @100ns:(report note): Test OK!

process
begin
    wait for 100 ns;
    assert test = X"AA" report "Wrong test signal value " & to_hstring(test) severity failure;
    report "Test OK!";
    finish;
end process;

end arch;
