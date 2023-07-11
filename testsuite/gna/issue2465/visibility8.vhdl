entity visibility8 is
end entity;

architecture test of visibility8 is
    constant c1 : integer := 1;
    subtype t1 is integer;
    type t2 is range 1 to 3;
    signal s1 : bit_vector(1 to 6);
    shared variable v1 : integer;
    alias a1 is c1;
    constant f1 : file_open_kind := READ_MODE;
    type ft is file of character;
begin

    b1: block is
        signal s1 : bit_vector(1 to s1'length);   -- Error
    begin
    end block;

    p1: process is
        constant c1 : integer := c1;    -- Error
        subtype t1 is t1;               -- Error
        type t2 is range t2'left to 5;  -- Error
        variable v1 : integer := v1;    -- Error
        alias a1 is a1;                 -- Error
        file f1 : ft open f1 is "x";    -- Error
    begin
        wait;
    end process;

end architecture;
