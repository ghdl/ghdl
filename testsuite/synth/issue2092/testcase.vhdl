library ieee;
use ieee.std_logic_1164.all;

entity testcase is
    port(
        rst : in std_ulogic;
        clk : in std_ulogic
    );
end entity testcase;

architecture rtl of testcase is

    component testcase2 port (
        rst : in std_ulogic;
        clk : in std_ulogic
    );
    end component;

begin
    testcase2_0: testcase2
        port map (
            clk => clk,
            rst => rst
        );
end architecture rtl;
