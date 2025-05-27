package mwe_pkg is
    type sub_rec_t is record
        arg1 : bit_vector;
        arg2 : bit_vector;
    end record;
    type rec_t is record
        sub : sub_rec_t;
    end record;
end package;

--------------------------------------------------------------------------------

library work;
use work.mwe_pkg.all;

entity mwe is
    generic (
        ARG1_WIDTH : positive;
        ARG2_WIDTH : natural
    );
    port (
        rec : out rec_t
    );
end entity;

architecture rtl of mwe is
    pure function zeros (size : natural) return bit_vector is
        variable sulv : bit_vector(size-1 downto 0);
    begin
        sulv := (others => '0');
        return sulv;
    end function;

    signal rec_int : rec'subtype;
    signal targ1_int : bit_vector(ARG1_WIDTH-1 downto 0);
begin
    rec_int.sub <= (
        arg1  => targ1_int,
        arg2  => zeros(ARG2_WIDTH)
    );
end architecture;

--------------------------------------------------------------------------------

library work;
use work.mwe_pkg.all;

entity tb_mwe is
end entity;

architecture test of tb_mwe is
    constant ARG1_WIDTH : positive := 32;
    constant ARG2_WIDTH : natural := 0;
    signal rec : rec_t(
        sub(
            arg1(ARG1_WIDTH-1 downto 0),
            arg2(ARG2_WIDTH-1 downto 0)
        )
    );
begin
    InstDut : entity work.mwe
        generic map (
            ARG1_WIDTH => ARG1_WIDTH,
            ARG2_WIDTH => ARG2_WIDTH
        )
        port map (
            rec => rec
        );
end architecture;
