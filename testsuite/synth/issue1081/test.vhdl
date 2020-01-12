library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity test is
    port(
        clk: in std_ulogic;
        a_in  : in std_ulogic_vector(1 downto 0);
        b_out : out std_ulogic_vector(2 downto 0)
        );
end test;

architecture rtl of test is
    function test_fn(a: std_ulogic_vector(1 downto 0)) return std_ulogic_vector is
        variable n : integer range 0 to 3;
    begin
        case a is
            when "11" =>
                n := 0;
            when "10" =>
                n:= 1;
            when others =>
                --n := 0;
                return "000";
        end case;
        return "1" & std_ulogic_vector(to_unsigned(n, 2));
    end;
begin
    process(clk)
    begin
        b_out <= test_fn(a_in);
    end process;
end;
