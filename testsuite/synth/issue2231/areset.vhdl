library ieee;
use ieee.std_logic_1164.all;

entity areset is
	port (
		clk_sys : in std_logic;
		reset_n : in std_logic;
		d : in std_logic;
		q : out std_logic_vector(1 downto 0)
	);
end entity;

architecture rtl of areset is
	signal q_i : std_logic_vector(1 downto 0);
begin

	process(clk_sys,reset_n) begin
		if reset_n='0' then
			q_i(0)<='0';
		elsif rising_edge(clk_sys) then
			q_i(1)<=d;
			q_i(0)<='1';
		end if;
	end process;

	q<=q_i;

end architecture;
