entity tb_issue2 is
end entity tb_issue2;

architecture dataflow of tb_issue2 is

    type ia is array (integer range <>) of bit_vector(0 downto 0);

    signal ip : ia(0 to 0) := (others => (others => '0'));

    signal ins : bit_vector(1 downto 0) := (others => '0');

begin

    lbl : for i in 0 to 0 generate
      ins(i downto i) <= ip(i);
    end generate;

end architecture dataflow;
