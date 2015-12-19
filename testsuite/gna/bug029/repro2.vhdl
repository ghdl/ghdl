package foo is
    function some_foo return integer;
end package;

package body foo is
    function some_foo return integer is
    begin
        return -1;
        return 0;
    end function;
end package body;
