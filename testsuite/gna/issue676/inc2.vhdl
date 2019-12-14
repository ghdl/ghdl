library ieee ;
use ieee.std_logic_1164.all;
use ieee.numeric_std_unsigned.all;

entity Inc2 is

generic(
    N : positive := 4
    );

port(
    A    : in  std_logic_vector(N-1 downto 0);
    Sum  : out std_logic_vector(N downto 0)
    );

end Inc2;

architecture RTL of Inc2 is
begin
  sum <= ('0', A) + 1;
end RTL;
