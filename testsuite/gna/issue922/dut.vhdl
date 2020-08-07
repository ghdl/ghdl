library ieee;
use ieee.std_logic_1164.all;

entity dut is
generic (
    adr_width : positive := 16
);
end entity dut;
architecture rtl of dut is
    function deser(b : bit_vector) return bit_vector is
        variable ab : bit_vector(adr_width-1 downto 0);
        variable ba : bit_vector(adr_width-1 downto 0);
    begin
        -- Below causes crash
        (ab, ba) := b;
        return ab&ba;
    end function;
    signal s1 : bit_vector(adr_width*2-1 downto 0);
    signal s2 : bit_vector(adr_width-1 downto 0);
begin
    s1 <= deser(s2);
end architecture rtl;
