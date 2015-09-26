
library ieee;
use ieee.std_logic_1164.all;
library alib;
use alib.acomp;

entity tb is

end; 

architecture arch of tb is

    signal clk:  std_logic := '0';
    signal lclk: std_ulogic;

    signal stop: boolean := false;
    signal lclk_count: integer := 0;

    component acomp is
        port (x: in std_ulogic; y: out std_ulogic);
    end component;

begin

    clk <= not clk after 10 ns when (not stop) else '0';

    ainst: acomp
        port map (clk, lclk);

    process (lclk) is
    begin
        if rising_edge(lclk) then
            lclk_count <= lclk_count + 1;
        end if;
    end process;

    process is
    begin
        report "start test";
        wait for 1 ms;
        report "end test";
        report "lclk_count = " & integer'image(lclk_count);
        assert lclk_count > 20000 report "test failed";
        if lclk_count > 20000 then report "test passed"; end if;
        stop <= true;
        wait;
  end process;

end architecture;
