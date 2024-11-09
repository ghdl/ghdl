library ieee;
use ieee.std_logic_1164.all;

entity deposit_bug is
    port (
        oe : in std_logic;
        d : in std_logic;
        o : out std_logic
    );
end entity deposit_bug;

architecture rtl of deposit_bug is
begin
    o <= d when (oe = '1') else 'Z';

end architecture;
