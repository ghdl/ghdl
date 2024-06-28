library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity repro is
   port (
      o : out std_logic_vector(15 downto 0)
   );
end;

architecture behav of repro is
  signal v : natural;
begin
  o <= to_stdlogicvector(v, 16);
end;

