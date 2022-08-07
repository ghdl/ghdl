package p is 
    function f(arg : integer := integer'(1)) return boolean;
end;

package body p is
    function f(arg : integer := integer'(1)) return boolean
    is
    begin
        return false;
    end;
end;

entity e is end; 
architecture a of e is begin end;
