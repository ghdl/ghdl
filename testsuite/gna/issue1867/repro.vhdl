library ieee;
use ieee.std_logic_1164.all;

entity one is
    generic (
        PORT_WIDTH : integer
    );
    port (
        INPUT : in std_logic_vector(PORT_WIDTH-1 downto 0)
    );
end entity;

architecture rtl of one is
begin
end architecture;

-----------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity two is
end entity;

architecture rtl of two is
    signal ctr : integer range 0 to 7;
begin
    one_inst : entity work.one
    generic map (
        PORT_WIDTH => 3
    )
    port map (
        INPUT => to_slv(ctr, 3)
    );
end architecture;
