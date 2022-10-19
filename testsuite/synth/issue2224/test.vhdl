library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_misc.all;

entity test is
port (
	clk : in std_logic;
	d : in std_logic_vector(2 downto 0);
	q : out std_logic
);
end entity;

architecture rtl of test is
begin
	q <= and_reduce(d);
end architecture;
