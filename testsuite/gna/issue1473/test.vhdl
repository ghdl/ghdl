library ieee;

entity test is
end test;

architecture test of test is

    type values_t is array (0 to VALUES - 1) of boolean;
    signal values: values_t;

    type elem_t is (a, b, c);
    type arr_t is array (0 to VALUES - 1) of elem_t;
    signal arr: arr_t;

begin
    arr <= (others => a);
end test;
