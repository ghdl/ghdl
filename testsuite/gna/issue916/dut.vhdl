library ieee;
use ieee.std_logic_1164.all;
entity avmm_csr is
	Port (
		reg_i : in std_ulogic_vector
    );
end avmm_csr;
architecture rtl of avmm_csr is
begin
end rtl;


library ieee;
use ieee.numeric_std_unsigned.all;
entity dut is
end entity dut;
architecture rtl of dut is
    signal int : natural;
begin
    inst : entity work.avmm_csr
    port map (
        reg_i           => to_slv(int, 2)
    );
end architecture rtl;
