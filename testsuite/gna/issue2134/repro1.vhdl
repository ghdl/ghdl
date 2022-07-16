entity repro1 is
end;

architecture behav of repro1 is
    type protected_t is protected
        impure function some_function return natural;
    end protected;

    type protected_t is protected body
        variable i : integer;
        impure function some_function return natural is
        begin
            return i;
        end function;
    end protected body;

    shared variable si : protected_t;

    attribute some_value : integer;
    attribute some_value of si : variable is 1;
begin
end behav;
