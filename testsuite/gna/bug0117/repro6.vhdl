entity repro6 is
end repro6;

architecture behav of repro6 is
  type bv_array is array(natural range <>) of bit_vector;
  type bv_array_ptr is access bv_array;
begin
  process
    variable count : natural := 0;
    impure function seven return natural is
    begin
      report "seven";
      count := count + 1;
      return 7;
    end seven;

    subtype array8 is bv_array(seven downto 0);

    subtype array8_1 is array8(open)(0 to 3);
  begin
    assert count = 1 severity failure;
    wait;
  end process;
end behav;
