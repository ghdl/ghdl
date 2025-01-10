library IEEE;
use IEEE.STD_LOGIC_1164.all;
package NUMERIC_STD is
  type UNSIGNED is array (natural range <>) of STD_LOGIC;
  function "+" (L : UNSIGNED; R :  NATURAL) return UNSIGNED;
end NUMERIC_STD;

library ieee;
use ieee.std_logic_1164.all;
use work.numeric_std.all;

entity circuit is
port (hex1: out std_logic_vector(6 downto 0));
end circuit;

architecture description of circuit is
  signal counter: unsigned(7 downto 0):=x"00";

begin
  process begin
    counter<=counter+1;
  end process;
end description;
