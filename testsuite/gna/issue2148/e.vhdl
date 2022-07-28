entity e is end;

architecture a of e is

    --------------------------------------------------------------------------------
    -- Variable is reported as unused, but it is used for its 'image attribute
    --------------------------------------------------------------------------------
    procedure p is
        variable a : integer;
    begin
        report integer'image(a);
    end;


    --------------------------------------------------------------------------------
    -- Variable b is reported as unused, but it is the return value of function f.
    --------------------------------------------------------------------------------
    type integer_vector_93 is array (natural range <>) of integer;
    function f return integer_vector_93 is
        variable b : integer;
    begin
        return (0 => b);
    end;
begin

    
    -- This code can be ignored. It is only to suppress non-spurious warnings
    -- about subprograms p and f being unused
    process
        constant c : integer_vector_93 := f;
    begin
        p;
        wait;
    end process;
end;
