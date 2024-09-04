library ieee;
use ieee.std_logic_1164.all;

entity top is
end top;

architecture synth of top is

    signal reg_dst : std_logic := '0';
    signal reg_src : std_logic := '0';

begin

    reg_dst <= reg_src;
    reg_dst <= '0';

end architecture;
