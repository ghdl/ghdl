library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

entity ReproduceBug is
  port(
    clk      : in std_logic;
    inp    : in unsigned(7 downto 0);
    outp   : out unsigned(7 downto 0)
  );
end ReproduceBug;

architecture rtl of ReproduceBug is
  -- only to get rid of "latch inferrence" warning
 -- signal outputLcl : unsigned(7 downto 0) := (others => '0'); 
begin

  Main: process(clk)
  begin
    if rising_edge(Clk) then
      outp <= inp ror 1; -- can also be 'rol'
    end if;
  end process;

--  output <= outputLcl; 

end rtl;
