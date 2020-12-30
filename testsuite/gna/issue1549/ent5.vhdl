entity ent5 is
  generic (
    WIDTH : natural := 1);
end ent5;

architecture ent of ent5 is
  type bv_array_t is array (natural range <>) of bit_vector;
  subtype bv_array2_t is bv_array_t(open)(WIDTH - 1 downto 0);

  procedure write_data (constant c : bv_array2_t) is
  begin
    for i in c'range loop
      report integer'image(i) & " =>" & to_string(c(i));
    end loop;

    assert c(1)(0) = '1'; -- <<<<<====== This should not fail
  end procedure;

  constant data2 : bv_array2_t(0 to 1) := (0 => "0", 1 => "1");
begin
  process
  begin
    write_data(data2);
    wait;
  end process;
end ent;
