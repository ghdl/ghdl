-- Massively reduced testcase - the actual file I'm attempting to build is:
-- https://github.com/MiSTer-devel/Intv_MiSTer/blob/master/rtl/intv/ivoice.vhd

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;

ENTITY ivoice2 IS
  PORT (
    pc : natural range 0 to 7;
    romd : std_logic_vector(15  DOWNTO 0);
    sound     : OUT std_logic_vector(7 downto 0)
    );
END ;

ARCHITECTURE rtl OF ivoice2 IS
BEGIN
  sound <=romd(7+pc downto pc);
END ARCHITECTURE rtl;
