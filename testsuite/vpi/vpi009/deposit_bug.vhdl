library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity deposit_bug is
    port (
        oe : in std_logic;
        d : in std_logic_vector(7 downto 0);
        o : out std_logic_vector(7 downto 0)
    );
end entity deposit_bug;

architecture rtl of deposit_bug is
begin
    o <= d when (oe = '1') else (others => 'Z');

end architecture;
