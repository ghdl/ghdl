package impurefunc is
    signal a:   bit;
    impure function impure_func return boolean;
end package;
package body impurefunc is
    impure function impure_func return boolean is
    begin
        a <= '1';
        return FALSE;
    end function;
end package body;
