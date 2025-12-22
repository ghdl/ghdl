entity mwe3 is
    generic (NB_ELEMENTS : positive := 5);
end entity;

architecture test of mwe3 is
    type pt_t is protected
    end protected;

    type integer_vector_ptr is access integer_vector;

    type pt_t is protected body
        -- This line does not work
        variable storage : integer_vector_ptr := new integer_vector(0 to NB_ELEMENTS-1);

        -- This line works:
        -- variable storage : integer_ptr_array_ptr := new integer_ptr_array_t(0 to 9);
    end protected body;
    shared variable sv : pt_t;
begin
end architecture;
