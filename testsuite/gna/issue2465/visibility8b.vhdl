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

    p1: process is
        constant c1 : integer := c1;    -- Error
    begin
        wait;
    end process;

end architecture;
