--!
--! Copyright (C) 2010 - 2012 Creonic GmbH
--!
--! This file is part of the Creonic Viterbi Decoder, which is distributed
--! under the terms of the GNU General Public License version 2.
--!
--! @file
--! @brief  Generic single port RAM with a single read/write port
--! @author Matthias Alles
--! @date   2010/09/28
--!

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library dec_viterbi;
use dec_viterbi.pkg_helper.all;


entity generic_sp_ram is
	generic(
		DISTR_RAM  : boolean := false;
		WORDS      : integer := 8;
		BITWIDTH   : integer := 8
	);
	port(
		clk : in std_logic;
		rst : in std_logic;

		wen : in  std_logic;
		en  : in  std_logic;

		a   : in  std_logic_vector(no_bits_natural(WORDS - 1) - 1 downto 0);
		d   : in  std_logic_vector(BITWIDTH - 1 downto 0);
		q   : out std_logic_vector(BITWIDTH - 1 downto 0)
	);
end generic_sp_ram;


architecture rtl of generic_sp_ram is

	type t_ram is array(WORDS - 1 downto 0) of
	                      std_logic_vector(BITWIDTH - 1 downto 0);
	signal sp_ram : t_ram := (others => (others => '0'));

	function get_ram_style_xilinx(dist_ram : in boolean) return string is
	begin
		if dist_ram then
			return "pipe_distributed";
		else
			return "block";
		end if;
	end function;
	
	function get_ram_style_altera(dist_ram : in boolean) return string is
	begin
		if dist_ram then
			return "MLAB, no_rw_check";
		else
			return "AUTO";
		end if;
	end function;

	attribute RAM_STYLE : string;
	attribute RAM_STYLE of sp_ram : signal is get_ram_style_xilinx(DISTR_RAM);

	attribute ramstyle : string;
	attribute ramstyle of sp_ram : signal is get_ram_style_altera(DISTR_RAM);

begin

	--
	-- Do not register the address for reading, since the synthesis doesn't
	-- recognize then that this is a single-port RAM.
	--
	pr_sp_ram_rw: process(clk)
	begin
	if rising_edge(clk) then
		if en = '1' then
			if wen =  '1' then
				sp_ram(to_integer(UNSIGNED(a))) <= d;
			else
				q <= sp_ram(to_integer(UNSIGNED(a)));
			end if;
		end if;
	end if;
	end process pr_sp_ram_rw;

end rtl;
