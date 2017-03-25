library IEEE;
use IEEE.std_logic_1164.all;

entity bug1 is
end entity bug1;

architecture behavioural of bug1 is

  signal outEn      : std_logic;

begin

  outEn <= 1;

end architecture behavioural;
