entity ent is
end entity;

library ieee;
use ieee.std_logic_1164.all;
package avmm_csr_pkg is
   type avmm_csr_array_t is array (natural range <>) of std_logic_vector(31 downto 0);
end avmm_csr_pkg;


library ieee;
use work.avmm_csr_pkg.all;
entity avmm_csr is
	Port (
		reg_i : in avmm_csr_array_t
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
        reg_i(0)           => to_slv(int, 32)
    );
end architecture rtl;
