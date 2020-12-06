library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity uns is
  port (
    o1 : out std_logic_vector(1 downto 0);
    o2 : out std_logic_vector (31 downto 0));
end entity;

architecture a of uns is
  signal v32 : unsigned(31 downto 0);
  signal v2 : unsigned(1 downto 0);
begin
    v2 <= to_unsigned(1, 2);
    o1 <= std_logic_vector(v2);

    v32 <= resize(to_unsigned(3, 2), 32);   -- <<<<<<< HERE
    o2 <= std_logic_vector(v32);
end;
