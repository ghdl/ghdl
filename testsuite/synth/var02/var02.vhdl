library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity var02 is
  port (clk : std_logic;
        sel : std_logic_vector (3 downto 0);
        res : out std_logic_vector (7 downto 0));
end var02;

architecture behav of var02 is
begin
  process (clk)
    variable r6 : signed(5 downto 0);
    variable r8 : signed(7 downto 0);
  begin
    if rising_edge (clk) then
      r8 := to_signed(3, 8);
      r6 := resize(r8, 6);
      r8 := resize(r6, 8);
      res <= std_logic_vector(r8);
    end if;
  end process;
end behav;
