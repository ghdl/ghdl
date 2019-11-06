package test_package is
    function log2(input : in natural) return natural;
end package test_package;

package body test_package is
    function log2(input : in natural) return natural is
        variable retval : natural := 0;
        variable temp   : natural := input;
    begin
        while temp > 1 loop
            retval := retval + 1;
            temp := temp / 2;
        end loop;

        return retval;
    end function log2;
end package body test_package;
