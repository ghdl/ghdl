library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;


entity my_entity is
    generic (
        GEN: std_ulogic_vector := b"1111"
    );
    port (
        clk: in std_ulogic;
        a: in std_ulogic_vector(7 downto 0);
        b: out std_ulogic_vector(7 downto 0)
    );
end;

architecture behavioral of my_entity is

    alias GEN_DOWNTO: std_ulogic_vector(GEN'length - 1 downto 0) is GEN;

    function foo(x: std_ulogic_vector(7 downto 0))
    return std_ulogic_vector is
        variable ret: std_ulogic_vector(7 downto 0);
    begin
        ret := x;
        for i in natural(GEN_DOWNTO'length - 1) downto 0 loop
            ret(i) := GEN_DOWNTO(i) and x(i);
        end loop;
        return ret;
    end;

begin
    process (clk) begin
        if rising_edge(clk) then
            b <= foo(a);
        end if;
    end process;
end;
