library ieee;
use ieee.std_logic_1164.all;

entity comp1 is
  port (
    a_i  : in std_logic_vector(3 downto 0);
    x_o : out std_logic
  );
end entity;

architecture arch of comp1 is
begin
  x_o <= a_i(0) and a_i(1) after 5 ns;
end arch;

library ieee;
use ieee.std_logic_1164.all;

entity mwe is
end entity;

architecture arch of mwe is
  signal clk : std_logic := '0';
  signal a   : std_logic_vector(3 downto 0);
  signal x   : std_logic;
begin

  process
  begin
    a <= "1100";
    wait for 10 us;
    a <= "1011";

    wait until clk'stable;
  end process;

  x_comp1 : entity work.comp1
  port map (
    a_i  => std_logic_vector'(a), -- this crashes if 'wait until clk'stable' isn't commented out
  --  a_i  => a, -- this works
    x_o  => x
  );

end arch;
