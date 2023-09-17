library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity dut is
    Port (
    	clk		: in std_logic
    );
end dut;
architecture dut_arch of dut is
	signal local_sig : std_logic;
begin
	proc: process begin
		local_sig <= '0';
		wait for 50 ns;
		local_sig <= '1';
	end process;
end dut_arch;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity tb is
end tb;
architecture tb_arch of tb is
	component dut is
	port
	(
		clk : in std_logic
	);
	end component;
	signal clk		: std_logic;
	signal tb_sig		: std_logic;
begin
	dut_inst : dut
	port map
	(
		clk => clk
	);

	proc: process begin
		wait for 50 ns;
		tb_sig <= << signal .tb.dut_inst.local_sig : std_logic >>;
	end process;
end tb_arch;
