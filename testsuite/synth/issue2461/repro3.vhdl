library ieee;
use ieee.std_logic_1164.all;

entity norm is
    generic (gen_n_caracteristicas : integer := 4);
    port (
        i_clk     : in bit;
        o_ocupado : out bit
    );
end norm;

architecture redimensionamento of norm is
    signal resetar   : bit := '0';
    signal contador  : integer := 0;
    signal resultado : std_logic_vector(gen_n_caracteristicas - 1 downto 0);
begin
    calcula : process (i_clk) begin
        if resetar = '1' then
            o_ocupado <= '0';
            contador <= 0;
        end if;

        if o_ocupado = '1' and contador < gen_n_caracteristicas then
            resultado(contador) <= '0';
            contador <= contador + 1;
        elsif contador = gen_n_caracteristicas  then
            o_ocupado <= '0';
        end if;
    end process calcula;
end redimensionamento;
