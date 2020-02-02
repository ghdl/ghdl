entity repro2 is
  generic(
    BITS  : positive := 4);
  port(
    min : in  bit_vector(BITS - 1 downto 0) := "1010");
end entity;

architecture rtl of repro2 is
begin
  process
    variable sum : bit_vector(BITS - 2 downto 0);
    variable carry : bit;
  begin
    (carry, sum) := min;
    assert carry = '1';
    assert sum = "010";
    wait;
  end process;
end architecture;
