entity ent8 is
  generic (
    WIDTH : natural := 1);
end;

architecture ent of ent8 is
  type data_t is record
    t1 : bit_vector;
    t2 : bit_vector;
  end record;
  type bv_array_t is array (natural range <>) of data_t;
  subtype bv_array1_t is bv_array_t(open)(t1(WIDTH - 1 downto 0));
  subtype bv_array2_t is bv_array1_t(open)(t2(WIDTH - 1 downto 0));

  procedure write_data (constant c : bv_array2_t) is
  begin
    for i in c'range loop
      report integer'image(i) & " =>" & to_string(c(i).t1);
    end loop;

    assert c(1).t1(0) = '1'; -- <<<<<====== This should not fail
  end procedure;

begin
  process
    variable data2 : bv_array2_t(0 to 1);
  begin
    data2(0).t1 := "0";
    data2(0).t2 := "1";
    data2(1).t1 := "1";
    data2(1).t2 := "0";
    write_data(data2);
    wait;
  end process;
end ent;
