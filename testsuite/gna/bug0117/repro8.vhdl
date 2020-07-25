entity repro8 is
end repro8;

architecture behav of repro8 is
  procedure assign (a, b : out bit_vector; val : bit_vector) is
  begin
    (a, b) := val;
  end assign;
begin
  process
    variable a: bit_vector(7 downto 0);
    variable b: bit_vector(3 downto 0);
  begin
    assign (a, b, x"012");
    assert a = x"01";
    assert b = x"2";
    wait;
  end process;
end behav;
