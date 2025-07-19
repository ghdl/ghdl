entity mwe is
end entity;

architecture test of mwe is
    type rec_generic_t is record
        a : bit_vector;
        b : bit_vector;
    end record;

    procedure deserialize (
        signal bv  : in  bit_vector;
        signal rec : out rec_generic_t
    ) is
        -- variable rec_v : rec'subtype;
    begin
        -- does not work
        (rec.a, rec.b) <= bv;

        -- works
        -- (rec_v.a, rec_v.b) := bv;
        -- rec <= rec_v;
    end procedure;

    signal r  : rec_generic_t(a(0 to 5), b(0 to 1));
    signal bv : bit_vector(0 to 7);
begin
    deserialize(bv, r);
end architecture;
