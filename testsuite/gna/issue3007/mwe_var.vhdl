entity mwe_var is
    generic (
        ARG1_WIDTH : positive := 12;
        ARG2_WIDTH : natural := 4);
end entity;

architecture rtl of mwe_var is
    type sub_rec_t is record
        arg1 : bit_vector;
        arg2 : bit_vector;
    end record;
    type rec_t is record
        sub : sub_rec_t;
    end record;

    pure function zeros (size : natural) return bit_vector is
    begin
        return (size - 1 downto 0 => '0');
    end function;

    procedure assign(r : out rec_t)
    is
      constant carg1 : bit_vector(ARG1_WIDTH-1 downto 0) := (others => '0');
    begin
      r.sub := (arg1  => carg1, arg2  => zeros(ARG2_WIDTH));
    end assign;
begin
  process
    variable rec : rec_t(sub(arg1(ARG1_WIDTH-1 downto 0),
                           arg2(ARG2_WIDTH-1 downto 0)));
  begin
    assign (rec);
    wait;
  end process;
end architecture;
