library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue is
  port (
    clk : in std_logic;
    idx : in std_logic_vector(7 downto 0);
    led : out std_logic
  );
end issue;

architecture implementation of issue is

begin

    process(clk)
    begin
      if (rising_edge (clk)) then
        if idx <= x"10000000" then
          led <= '1';
        else
          led <= '0';
        end if;
      end if;
    end process;

end implementation;
