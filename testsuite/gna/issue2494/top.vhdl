library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.all;
use work.compa_pkg.all;


entity	top is
	port (
		clk  :in std_logic;
		rst  :in std_logic
		);
end entity;

architecture behv of top is
signal siga :std_logic_vector(7 downto 0);
signal sigb	:test_rec(a(11 downto 0),b(11 downto 0));
begin

sigb.b <= 12x"AB";

compa_i	:entity compa
	port map (
		clk 	=> clk,
		rst		=> rst,
		siga	=> siga,
		sigb	=> sigb
		);


end architecture;

