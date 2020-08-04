library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity bar is
  port (
    input    : in unsigned(3 downto 0);
    output   : out std_logic
  );
end bar;

architecture bar of bar is
begin
  output <= '1' when input(3 downto 0) = conv_unsigned(7, 4)
                else '0';
end bar;


