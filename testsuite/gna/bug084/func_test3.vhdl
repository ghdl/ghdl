entity func_test3 is
    generic (NBITS: natural := 6);
end entity;

architecture fum of func_test3 is
    type remains is (r0, r1, r2, r3, r4); -- remainder values

    function mod5 (dividend: bit_vector) return boolean is
        type remain_array is array (NBITS downto 0) of remains;
        type branch is array (remains, bit) of remains;
        constant br_table:  branch := ( r0 => ('0' => r0, '1' => r1),
                                        r1 => ('0' => r2, '1' => r3),
                                        r2 => ('0' => r4, '1' => r0),
                                        r3 => ('0' => r1, '1' => r2),
                                        r4 => ('0' => r3, '1' => r4)
                                      );
        variable  remaind:    remains := r0;
        variable tbit:        bit_vector (NBITS - 1 downto 0) := dividend;
    begin
        for i in dividend'length - 1 downto 0 loop
            remaind := br_table(remaind,tbit(i));
        end loop;
        return remaind = r0;
    end function;
begin
  assert mod5("101000");
end architecture;
    
