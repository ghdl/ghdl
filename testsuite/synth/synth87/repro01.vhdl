library ieee;
use ieee.std_logic_1164.all;

entity repro01 is
  port (a, b, c : in std_logic;
        z : out std_logic);
end repro01;

architecture behav of repro01 is
  subtype logic is std_logic;

  type my_rec is record
    a : std_logic_vector(7 downto 0);
  end record;
  subtype my_rec2 is my_rec;
begin
  process(A, B, C)
    variable temp : logic;
  begin
    temp := A and B;
    Z <= temp or C;
  end process;
end behav;
