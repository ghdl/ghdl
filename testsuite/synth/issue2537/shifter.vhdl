library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity shifter is
    generic (WIDTH : natural := 8);
    port (
        clk, ld_en : in std_ulogic;
        par_in : in std_ulogic_vector (1 to WIDTH);
        n_shift : integer range -(WIDTH-1) to (WIDTH-1);
        par_out : out std_ulogic_vector (1 to WIDTH)
    );
end entity;

architecture rtl of shifter is
    signal q : std_ulogic_vector (1 to WIDTH);
begin

    par_out <= q;

    load_or_shift: process (clk) is
    begin
        if rising_edge(clk) then
            if ld_en = '1' then
                q <= par_in;
            else
                q <= q sla n_shift;
            end if;
        end if;
    end process;

end architecture;

