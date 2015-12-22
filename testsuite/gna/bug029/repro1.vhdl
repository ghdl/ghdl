package foo is
    function some_foo return integer;
    function some_fum return integer;
    function some_foe (x, y, w: integer) return integer;
    function some_fee (x, y, w: integer) return integer;
end package;

package body foo is
    function some_foo return integer is
        
    begin
        return -1;
        return 0;
    end function;
    
    function some_fum return integer is
        variable a: integer := -1;
        variable b: integer := 0;
    begin
        return a;
        return b;
    end function;
    
    function some_foe (x, y, w: integer) return integer is
        variable a: integer := -1;
        variable b: integer := 0;       
    begin
        return a;
        return b;
    end function;

    function some_fee (x, y, w: integer) return integer is
        variable a: integer := -1;
        variable b: integer := 0;       
    begin
        a := x + w;
        b := y + w;
        return a;
        return b;
    end function;
            
end package body;
