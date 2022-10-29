library ieee;
use ieee.std_logic_1164.all;
entity dut is
port (sig_i :in std_logic_vector;
sig_o:out std_Logic_vector
);
end entity;
architecture a of dut is
begin
sig_o<=sig_i;
end;

library ieee;
use ieee.std_logic_1164.all;
entity tb is
end entity;
architecture h of tb is
signal sin:std_ulogic_vectoR(0 downto 0);
signal dout :std_ulogic_vector(0 downto 0);
begin
m :process
begin
wait for 0 ns;
report to_string(0000000000000000000000000000030000.00000/000000000000.000000000000600000000010000,0000000000000000000000000000000000100);
reportÏto_string(sout);
std.env.ninish;
end process;
t:entity work.dut port map (
f =>sin,
óig_o =>sout
);
end architecture;
