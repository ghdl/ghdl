entity shadow is
end entity;

architecture behaviour of shadow is

  signal s : bit_vector(3 downto 0);

begin

  process

    procedure example (s : in bit_vector) is
    begin
      s <= s;
    end procedure;

  begin
    wait;
  end process;

end architecture;
