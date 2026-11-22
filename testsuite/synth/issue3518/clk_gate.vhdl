library ieee;
use ieee.std_logic_1164.all;

entity clk_gate is
    port (
        -- Clock input
        clk_in              : in    std_logic;

        -- Clock Enable
        clk_en              : in    std_logic;

        -- Scan Enable
        scan_mode           : in    std_logic;

        -- Gated clocks
        clk_out             : out   std_logic
    );
end clk_gate;

architecture rtl of clk_gate is

    signal clk_en_q        : std_logic;

begin
    
    p_clk_en_latch : process(clk_in, clk_en, scan_mode)
    begin
        if (clk_in = '0') then
            clk_en_q <= clk_en or scan_mode;
        end if;
    end process;

    clk_out <= clk_in AND clk_en_q;

end rtl;
