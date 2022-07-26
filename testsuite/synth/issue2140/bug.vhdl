library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity sub is
	port (
		clk       :  in std_ulogic;
		reset_n   :  in std_ulogic;

		src_valid :  in std_ulogic;
		src_ready : out std_ulogic
	);
end sub;

architecture rtl of sub is
begin
end architecture;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
	port(
		clk     : in std_ulogic;
		reset_n : in std_ulogic
	);
end bug;

architecture struct of bug is
	signal filtered_src_valid       : std_ulogic;
	signal filtered_src_ready       : std_ulogic;
	
	signal pipeline_src_valid       : std_ulogic;
	signal pipeline_src_ready       : std_ulogic;
	
	type state_t is (idle, active);
	signal state : state_t;
begin

pipeline_src_valid <= filtered_src_valid when (state = idle) or (state = active) else '0';

process(clk, reset_n)
begin
	if reset_n = '0' then
	elsif rising_edge(clk) then
		if state = idle then
			if filtered_src_valid = '1' and filtered_src_ready = '1'  then
				state <= active;
			end if;
		elsif state = active then
			if pipeline_src_valid = '1' and pipeline_src_ready = '1'  then
				state <= idle;
			end if;
		end if;
	end if;
end process;

pipeline : entity work.sub
	port map(
		clk      => clk,
		reset_n  => reset_n,

		src_valid => pipeline_src_valid,
		src_ready => pipeline_src_ready
	);

end architecture;
