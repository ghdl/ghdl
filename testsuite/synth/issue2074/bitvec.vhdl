library ieee;
use ieee.std_logic_1164.all;

entity bitvec is
port (
	clk : in std_logic;
	d : in bit_vector(7 downto 0);
	q : out bit_vector(7 downto 0)
);
end entity;

architecture rtl of bitvec is
	constant a : bit_vector(7 downto 0) := X"5a";
begin
	q <= d and a;
end architecture;
