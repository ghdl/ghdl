use std.textio.all;

entity e is end;

architecture a of e is
    function f return line is
    begin
        return new string'("");
    end;

    -- This line causes the crash
    constant c : bit := f'range;
begin
end;

