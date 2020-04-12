library ieee;
use ieee.std_logic_1164.all;

entity delay_ul is
	generic (
		STAGES : natural := 4;
		RESET_ACTIVE_LEVEL : std_ulogic := '1'
	);
	port (
		Clock   : in std_ulogic;
		Reset   : in std_ulogic;
		Enable  : in std_ulogic;
		Sig_in  : in std_ulogic;
		Sig_out : out std_ulogic
	);
end entity;

architecture rtl of delay_ul is
begin
	gt: if STAGES > 0 generate
		reg: process(Clock, Reset, Enable)
			variable pl_regs : std_ulogic_vector(1 to STAGES);
		begin
			if Reset = RESET_ACTIVE_LEVEL then
				pl_regs := (others => '0');
			elsif rising_edge(Clock) and Enable = '1' then
				if STAGES = 1 then
					pl_regs(1) := Sig_in;
				else
					pl_regs := Sig_in & pl_regs(1 to pl_regs'high-1);
				end if;
			end if;

			Sig_out <= pl_regs(pl_regs'high);
		end process;
	end generate;

	gf: if STAGES = 0 generate
		Sig_out <= Sig_in;
	end generate;

end architecture;
