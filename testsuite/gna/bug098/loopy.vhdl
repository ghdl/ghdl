library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity loopy is
end loopy;

architecture foo of loopy is
    constant R: integer := 4;
    constant L: integer := 16;
    constant W: integer := 16;
    constant M: integer := 4;
    type  t_reg_x is array ( 0 to L-1 ) of signed( W-1 downto 0 );
    signal reg_x : t_reg_x  := ( others => ( others => '0' ) );
    type t_mux_in_x is array ( 0 to L - 1 ) of signed( W - 1 downto 0 );
    signal mux_in_x: t_mux_in_x :=  ( others => ( others => '0') );
begin
    process (reg_x)
    begin
        for r in 0 to R-1 loop
            for m in 0 to M-1 loop
                mux_in_x(r * M + m) <= reg_x(m * R + r);
            end loop;
        end loop;
    end process;
end architecture;
