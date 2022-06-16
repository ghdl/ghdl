library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity bug is
port(
    clk : in std_ulogic
);
end entity;

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity ent is
port(
    data : in unsigned(15 downto 0)
);
end entity;

architecture rtl of bug is

    signal tmp : std_ulogic_vector(31 downto 0);

begin
    u0 : entity work.ent 
    port map(data => unsigned(tmp(15 downto 0)));
end architecture;

architecture rtl of ent is
begin
end architecture;
