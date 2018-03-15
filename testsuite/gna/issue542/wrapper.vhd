library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity wrapper is
	port(
	clk   : in std_logic;
	reset : in std_logic;
	write : in std_logic;
	ack   : out std_logic
);
end wrapper;

architecture a of wrapper is

	-- compiling with std=93 produces an error here
	component write is
		port(
					clk   : in std_logic;
					reset : in std_logic;
					write : in std_logic;
					ack   : out std_logic
				);
	end component;

begin

	--dut : entity work.write(a) -- compilation works with this type of instanciation/declaration, std=08 and component declaration on line 17 commented
	dut: component write
		port map(
					clk   => clk,
					reset => reset,
					write => write, --compiling with std=08 produces a error here
					ack   => ack
				);

end architecture;
