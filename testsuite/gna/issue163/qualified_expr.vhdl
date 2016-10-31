library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity qualified_expr is
    port (
        X       : in    std_logic_vector(7 downto 0);
        CIn : in    std_logic;
        Y       : out   std_logic_vector(7 downto 0)
    );
end entity;


architecture rtl of qualified_expr is
begin
    -- analyze error with GHDL
    Y <= std_logic_vector(unsigned(X) + unsigned'((0 to 0 => CIn)));

    -- analyses with GHDL but not with other tools
    --Y <= std_logic_vector(unsigned(X) + unsigned'(0 to 0 => CIn));
end architecture;
