--
-- dut
--
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity dut is
Generic (
	TARGETS_ADDR : std_logic_vector
);
end dut;
architecture a of dut is

	signal slv : std_logic_vector(1 downto 0);

	type slv_arr_t is array (0 to 1) of std_logic_vector(slv'range);

	function addr_arr_init (
		arg : std_logic_vector
	) return slv_arr_t is
		variable v : slv_arr_t;
	begin
		report "arg'length="&positive'image(arg'length);
		report "v(0)'length="&positive'image(v(0)'length);

		-- Bound check error
		v(0) := arg(1 downto 0);

		-- No bound check error
		--v(0) := arg;
		return v;
	end function;

	constant ADDR_ARR : slv_arr_t := addr_arr_init(TARGETS_ADDR);
begin

end a;

--
-- tb
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb is
end entity;

architecture bench of tb is
	constant C_SLV : std_logic_vector(1 downto 0) := "00";
begin

	dut : entity work.dut
	generic map	(
		-- Bound check error
		TARGETS_ADDR => "00"
		-- No bound check error
		--TARGETS_ADDR => C_SLV
	);

	stimulus : process
	begin
	report "pass";
	wait;
	end process;

end bench;
