package mwe1_pkg is
    generic (type user_type);
end package;

entity mwe1 is
end entity;

architecture rtl of mwe1 is
begin
    process is
        type bv_ptr_t is access bit_vector;
        package bv_mwe1_pkg is new work.mwe1_pkg generic map (bv_ptr_t);
    begin
        wait;
    end process;
end architecture;
