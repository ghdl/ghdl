use std.textio.all;

entity e is end;
architecture a of e is
begin
    process
        variable q : line(1 to 10) := new string(1 to 10);
    begin
        assert false severity failure;
        wait;
    end process;
end;
