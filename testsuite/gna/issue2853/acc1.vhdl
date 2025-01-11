entity acc1 is
end entity;

architecture test of acc1 is
begin
    process is
        type test_t is record
            bv : bit_vector;
        end record;
        type ptr_t is access test_t;
    begin
      wait;
    end process;
end architecture;
