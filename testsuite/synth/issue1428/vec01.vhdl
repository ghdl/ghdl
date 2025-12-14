library ieee;
use ieee.std_logic_1164.all;

entity vec01 is
  port (a : out std_logic;
        b : std_logic_vector(7 downto 0));
end;

architecture behav of vec01 is
  type my_vec is array(integer range <>) of std_logic;
  signal s : my_vec(-1 to 6);
begin
  s <= my_vec(b);
  s(-1) <= b(0);
--  s(0) <= b(1);
  a <= not s(2);
end behav;
