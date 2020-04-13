library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity top is
port (
	ch : in integer range 0 to 7;
	din : in unsigned(7 downto 0);
	dout : out unsigned(7 downto 0)
);
end entity;

architecture arch of top is
begin
	dout <= din srl (ch + 1);
end architecture;
