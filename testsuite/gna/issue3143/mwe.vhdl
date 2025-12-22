package pt_generic_pkg is
    generic (
        NB_ELEMENTS : positive
    );

    type pt_t is protected
    end protected;
end package;

package body pt_generic_pkg is
    type integer_ptr_t is access integer;
    type integer_ptr_array_t is array (natural range <>) of integer_ptr_t;
    type integer_ptr_array_ptr is access integer_ptr_array_t;

    type pt_t is protected body
        -- This line does not work
        variable storage : integer_ptr_array_ptr := new integer_ptr_array_t(0 to NB_ELEMENTS-1);

        -- This line works:
        -- variable storage : integer_ptr_array_ptr := new integer_ptr_array_t(0 to 9);
    end protected body;
end package body;

entity mwe is
end entity;

architecture test of mwe is
    package pt_pkg is new work.pt_generic_pkg generic map (NB_ELEMENTS => 10);
    shared variable sv : pt_pkg.pt_t;
begin
end architecture;
