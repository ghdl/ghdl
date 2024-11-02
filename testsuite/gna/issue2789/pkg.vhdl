library IEEE;
    use IEEE.std_logic_1164.all;

package pkg is
    generic(
        type ELEMENT_TYPE;
        SIZE: integer
    );
    type arr_t is array(0 to SIZE) of ELEMENT_TYPE;
    subtype ctrl_t is std_logic_vector(0 to SIZE);

    type type_with_bits is record
        data: arr_t;
        bits: ctrl_t;
    end record;
end package pkg;
