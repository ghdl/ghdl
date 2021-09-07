library ieee;
use ieee.std_logic_1164.all;
entity avmm_csr is
	Generic (width : positive);
	Port (
		reg_i : in std_ulogic_vector(width-1 downto 0)
    );
end avmm_csr;
architecture rtl of avmm_csr is
begin
end rtl;

library ieee;
use ieee.std_logic_1164.all;
entity dut is
end entity dut;
architecture rtl of dut is
    signal s : std_ulogic_vector(1 downto 0);
begin
    inst : entity work.avmm_csr
    generic map (width => 4)
    port map (
        reg_i => "11"&s
    );
end architecture rtl;

