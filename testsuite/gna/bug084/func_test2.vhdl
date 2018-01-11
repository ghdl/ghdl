library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity func_test2 is
    generic (NBITS: natural := 6);
end entity;

architecture fum of func_test2 is
    signal dividend:    std_logic_vector (NBITS - 1 downto 0);

    function mod5 (dividend: std_logic_vector) return std_logic is
        type remains is (r0, r1, r2, r3, r4); -- remainder values
        type remain_array is array (NBITS downto 0) of remains;
        type branch is array (remains, bit) of remains;
        constant br_table:  branch := ( r0 => ('0' => r0, '1' => r1),
                                        r1 => ('0' => r2, '1' => r3),
                                        r2 => ('0' => r4, '1' => r0),
                                        r3 => ('0' => r1, '1' => r2),
                                        r4 => ('0' => r3, '1' => r4)
                                      );
        variable  remaind:    remain_array := (others => r0);
        variable tbit:        bit_vector (NBITS - 1 downto 0);
    begin
        tbit := to_bitvector(dividend); -- little endian
        for i in dividend'length - 1 downto 0 loop
            remaind(i) := br_table(remaind(i + 1),tbit(i));
        end loop;
        if remaind(0) = r0 then
            return '1';
        else
            return '0';
        end if;
    end function;
begin
    process
        variable errors:   natural;
    begin
        errors := 0;
        for i in 0 to 2 ** NBITS - 1 loop
            dividend <= std_logic_vector(to_unsigned(i, NBITS));
            wait for 0 ns;
            report "mod5(" & integer'image(i) &") = " &
            std_ulogic'image(mod5(dividend));
        end loop;
        wait;
    end process;
end architecture;
    
