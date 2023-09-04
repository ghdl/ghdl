library ieee;
use ieee.std_logic_1164.all;

entity norm is
    generic (gen_n_caracteristicas : integer := 6);
    port (
        i_clk     : in bit;
        o_x_norm  : out std_logic_vector(gen_n_caracteristicas - 1 downto 0)
    );
end norm;

architecture redimensionamento of norm is
    signal iniciar   : bit := '0';
    signal contador  : integer := 0;
    signal resultado : std_logic_vector(gen_n_caracteristicas - 1 downto 0);
begin
    calcula : process (i_clk) begin
        if iniciar = '1' then
            contador <= 0;
            resultado <= (others=>'0');
        end if;

        if contador < gen_n_caracteristicas then
            resultado(contador) <= '1';
            contador <= contador + 1;
        end if;
    end process calcula;
end redimensionamento;
