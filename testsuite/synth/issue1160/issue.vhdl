library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity issue is
    generic (N     : natural := 3);
       port (data  : out signed(N-1 downto 0));
end issue;

architecture rtl of issue is
    subtype my_type is signed(N-1 downto 0);
begin
    data <= to_signed(1,my_type'length);
end architecture;
