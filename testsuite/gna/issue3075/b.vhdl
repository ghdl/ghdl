entity B is
end B;

architecture sim of B is
  type arr2d is array (natural range <>) of bit_vector(20 downto 0);
begin
  process
    procedure d(c : arr2d) is
    begin
    end procedure;
  begin
    d("0" & x"00000");
    wait;
  end process;
end architecture;
