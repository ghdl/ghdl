entity e is end;

architecture a of e is

    procedure p is
        variable a : integer;
    begin
        null;
    end;
begin
    process
    begin
        p;
        wait;
    end process;
end;
