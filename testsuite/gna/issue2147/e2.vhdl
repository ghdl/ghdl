entity e2 is end;

architecture a of e2 is
    type bv_acc is access bit_vector;
    function f return bv_acc is
    begin
        return new bit_vector'("01");
    end;

    -- This line causes the crash
    constant c : bit_vector (f'range) := (others => '0');
begin
end;

