library IEEE;
use IEEE.std_logic_1164.all;

entity bug is
port(
    clk : in std_ulogic
);
end entity;

library IEEE;
use IEEE.std_logic_1164.all;

entity ent is
port(
    data : in std_ulogic_vector
);
end entity;

architecture rtl of bug is

    type data_t is record
        a : std_ulogic;
        b : std_ulogic;
    end record;

    function to_sulv(data : data_t) return std_ulogic_vector is
        constant ret : std_ulogic_vector(1 downto 0) := data.a & data.b;
    begin
        return ret;
    end function;

    constant data : data_t := (a => '0', b => '1');
begin
    u0 : entity work.ent 
    port map(data => to_sulv(data));
end architecture;

architecture rtl of ent is

begin

end architecture;
