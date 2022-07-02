library ieee;use ieee.std_logic_1164.all;entity dut is
port(sig_i:std_logic_vector;sig_o:out std_logic_vector);end entity;architecture a of dut is
begin	sig_o<=sig_i;end architecture;library ieee;use ieee.std_logic_1164.all;entity tb is
end entity;architecture h of tb is
signal n:std_logic_vector(0 to 0);signal s:std'u(0);begin t port map(0);end architecture;