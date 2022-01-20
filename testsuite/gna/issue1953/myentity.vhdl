library ieee;
use ieee.std_logic_1164.all;

entity myEntity is
end myEntity;

architecture behavioral of myEntity is
  constant sizeArray : integer := 3;
  type arrayT is array (0 to sizeArray-1) of std_logic;

  constant myArray : arrayT := ('0','1');
begin
end behavioral;
