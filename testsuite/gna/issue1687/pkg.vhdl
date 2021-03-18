package pack_RC_Add_n_F is
    function RC_Add_n( A, B :bit_vector; Cin : bit) return bit_vector;
end pack_RC_Add_n_F;
    
package body pack_RC_Add_n_F is
    function RC_Add_n( A, B :bit_vector; Cin : bit) return bit_vector is
        variable C:bit := Cin;
        variable SUM:bit_vector(A'length downto 0);
    begin
        loop_add_m: for I in 0 to A'length-1 loop
            SUM(I) := (A(I) xor B(I)) xor C;
            C := (A(I) and B(I)) or (C and (A(I) xor B(I) ));
        end loop loop_add_m;
        SUM(A'length) := C;
        return SUM;
    end RC_Add_n;
end pack_RC_Add_n_F;
