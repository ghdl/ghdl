entity e is end;
architecture a of e is
    --crash
    procedure p(arg : string(1 to (1)));
    procedure p(arg : string(1 to (1))) is begin end;

    --no crash
    --procedure p(arg : string(1 to 1));
    --procedure p(arg : string(1 to 1)) is begin end;
begin
    assert false severity failure;
end;
