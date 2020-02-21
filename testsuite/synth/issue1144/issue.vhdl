library ieee;
use ieee.std_logic_1164.all;

entity issue is
    port (i_data     : in  std_logic_vector(8 downto 0);
          o_data     : out std_logic_vector(3 downto 0);
          clock      : in  std_logic);
end issue;

architecture rtl of issue is
    alias i_hi is i_data(3 downto 0);
begin
        o_data <= i_hi;
end architecture;
