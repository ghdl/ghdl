library ieee; 
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package attribute_on_shared_variable is

    type protected_t is protected
        impure function some_function return natural;
    end protected;

end package;

package body attribute_on_shared_variable is

    type protected_t is protected body
        variable i : integer;
        impure function some_function return natural is
        begin
            return i;
        end function;
    end protected body;

    shared variable si : protected_t; 

    attribute some_value : integer;
    attribute some_value of si : variable is si.some_function;

end package body;
