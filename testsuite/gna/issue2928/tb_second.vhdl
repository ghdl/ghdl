library ieee;
use ieee.std_logic_1164.all;

entity tb_second is
end entity tb_second;

architecture rtl of tb_second is
    signal clk : std_logic := '0';
begin
    reproducer_inst : entity work.reproducer
        port map(
            clk => clk
        );

    main : process
    begin
        wait;
    end process;
end architecture;
