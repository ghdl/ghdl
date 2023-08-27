library ieee;
use ieee.std_logic_1164.all;

entity norm is
    generic (gen_n_caracteristicas : integer := 6);
    port (
        init     : in bit;
        o  : out std_logic_vector(gen_n_caracteristicas - 1 downto 0)
    );
end norm;

architecture redimensionamento of norm is
    signal contador  : integer := 0;
begin
    calcula : process (init) begin
        if init = '1' then
            contador <= 0;
            o <= (others=>'0');
        end if;

        if contador < gen_n_caracteristicas then
            o(contador) <= '1';
--          o <= not o;
            contador <= contador + 1;
        end if;
    end process calcula;
end redimensionamento;
