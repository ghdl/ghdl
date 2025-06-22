package gt2_dummy_pkg is
    -- 1:
    -- -- first simple case of gt usage in procedure.
    procedure gt_equal
    generic(type gt_generic_type)
    parameter(
        signal value_1 : gt_generic_type;
        signal value_2 : gt_generic_type
    );

    procedure slv_equal is new gt_equal
        generic map (gt_generic_type => bit_vector);
end package gt2_dummy_pkg;

package body gt2_dummy_pkg is

    -- 1:

    procedure gt_equal
    generic(type gt_generic_type)
    parameter(
        signal value_1 : gt_generic_type;
        signal value_2 : gt_generic_type
    ) 
    is
    begin
        assert value_1 = value_2
            report "gt_equal: value_1 and value_2 are not equal"
            severity error;
        wait until value_1'event or value_2'event; -- Wait for any change in either signal
    end procedure gt_equal;
end package body gt2_dummy_pkg;
