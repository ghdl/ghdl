LIBRARY ieee;
USE ieee.std_logic_1164.all;

entity add03 is
  port (
    a, b : std_logic_vector(8 DOWNTO 0);
    borrow : std_logic;
    res : out std_logic_vector(8 DOWNTO 0));
end add03;

LIBRARY ieee;
USE ieee.std_logic_arith.all;

architecture behav of add03 is
  signal t : signed(8 DOWNTO 0);
begin
  t <= signed(a) - signed(b) - borrow;
  res <= std_logic_vector(t);
end behav;
