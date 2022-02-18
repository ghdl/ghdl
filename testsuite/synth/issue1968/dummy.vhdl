library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.NUMERIC_STD.all;
use work.dummy_pkg.all;

entity dummy is
    port (
        signal A_i : in std_logic_vector(31 downto 0);
        signal B_i : in std_logic_vector(31 downto 0);
        signal C_i : in std_logic_vector(31 downto 0);
        signal o : out std_logic_vector(31 downto 0)
    );
end dummy;

architecture rtl of dummy is
begin

--    this_works(A_i, B_i, C_i, o);

    this_doesnt_work(A_i, B_i, C_i, o);

end rtl;
