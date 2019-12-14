library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity alu is
  port (
    a : in std_logic;
    b : in std_logic;
    y : out std_logic
  );
end alu;

architecture mux of alu is
  signal mux1: std_logic_vector(7 downto 0);
begin

  process(a, b)
  begin
    y <= mux1(a & b); -- now allowed
  end process;
end mux;
