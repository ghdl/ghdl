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

    -- 2:
    -- -- second simple case of gt usage in procedure. (using function to not use 'range within the procedure)

    function is_part_of(a : bit; b : bit_vector) return boolean;

    procedure gt_is_a_part_of_b
    generic(
        type gt_generic_type_a;
        type gt_generic_type_b;
        function is_part_of(a : gt_generic_type_a; b : gt_generic_type_b) return boolean is <>
    )
    parameter(
        signal value_a : gt_generic_type_a;
        signal value_b : gt_generic_type_b
    );

    procedure slv_is_a_part_of_b is new gt_is_a_part_of_b
        generic map (gt_generic_type_a => bit, gt_generic_type_b => bit_vector);

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


    -- 2:

    function is_part_of(a : bit; b : bit_vector) return boolean is
        variable a_was_found_in_b : boolean := false;
    begin
        for i in b'range loop
            if a = b(i) then
                a_was_found_in_b := true;
                exit; -- Exit the loop if a is found in b
            end if;
        end loop;
        return a_was_found_in_b;
    end function is_part_of;


    procedure gt_is_a_part_of_b
    generic(
        type gt_generic_type_a;
        type gt_generic_type_b;
        function is_part_of(a : gt_generic_type_a; b : gt_generic_type_b) return boolean is <>
    )
    parameter(
        signal value_a : gt_generic_type_a;
        signal value_b : gt_generic_type_b
    )
    is
    begin
        assert is_part_of(value_a, value_b)
            report "gt_is_a_part_of_b: value_a is not a part of value_b"
            severity error;
        wait until value_a'event or value_b'event; -- Wait for any change in either signal
    end procedure gt_is_a_part_of_b;



end package body gt2_dummy_pkg;
