
-- This testbench fails with GHDL 0.29 and with GHDL svn_2013-02-13.

library ieee;
use ieee.std_logic_1164.all;

entity clkgen is

    -- NOTE: Removing the default value makes the testbench work as expected.
    port ( b: out std_ulogic := '0' );

end entity;

architecture arch of clkgen is
    signal a: std_ulogic;
begin
    a <= '0', '1' after 10 ns, '0' after 20 ns;
    b <= a;
end architecture;


library ieee;
use ieee.std_logic_1164.all;

entity tb is
end entity;

architecture arch of tb is

    -- NOTE: Declaring C as std_ulogic makes the testbench work as expected.
    signal c: std_logic;

begin

    u0: entity work.clkgen port map ( b => c );

    process
    begin

        wait for 1 ns;

        if c = '0' then
            report "good: C is '0' as expected";
        else
            -- This fails with GHDL 0.29.
            report "BAD: C is not '0'" severity failure;
        end if;

        wait until c = '1' for 50 ns;
        -- This is ok with GHDL 0.29.
        assert c = '1' severity failure;

        wait until c = '0' for 50 ns;

        if c = '0' then
            report "good: C is '0' as expected";
        else
            -- This fails with GHDL 0.29.
            report "BAD: C is not '0'" severity failure;
        end if;

        wait;
    end process;

end architecture;

