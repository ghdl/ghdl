library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.env.all;

entity tbAdder is
end entity tbAdder;

architecture Bhv of tbAdder is
	constant cWidth : natural := 8;	
	signal iA, iB : std_ulogic_vector(cWidth-1 downto 0) := (others => '0');
	signal oRes : std_ulogic_vector(cWidth-1 downto 0);
	signal oCarry : std_ulogic;
begin
	UUT: entity work.Adder
		generic map(
			gWidth => cWidth
		)
		port map(
			iA     => iA,
			iB     => iB,
			oCarry => oCarry,
			oRes   => oRes
		);	
end architecture Bhv;
