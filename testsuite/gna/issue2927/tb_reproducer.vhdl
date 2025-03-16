library ieee;
use ieee.std_logic_1164.all;

entity tb_reproducer is
end entity tb_reproducer;

architecture rtl of tb_reproducer is
    signal reset : std_logic := '0';
    signal clk : std_logic := '0';
begin
    reproducer_inst : entity work.reproducer(rtl)
        port map(
            clk => clk,
            reset => reset
        );
    main : process
    begin
        wait;
    end process;
end architecture;
