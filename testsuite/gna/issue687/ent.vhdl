library ieee;
use ieee.std_logic_1164.all;
entity dut is
	port (
		sig_i : in std_logic_vector;
		sig_o : out std_logic_vector
	);
end entity;
architecture arch of dut is
begin
	sig_o <= sig_i;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
entity tb is
end entity;
architecture bench of tb is
	signal sin : std_ulogic_vector(1 downto 0);
	signal sout : std_ulogic_vector(31 downto 0);
begin
	stim : process
	begin
		wait for 1 ns;
		report to_string(sin);
		report to_string(sout);
		std.env.finish;
	end process;
	dut_inst: entity work.dut port map (
		sig_i => sin,
		sig_o => sout
	);
end architecture;
