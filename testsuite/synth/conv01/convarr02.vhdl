entity convarr02 is
  generic (s : string := "hello");
end;

architecture behav of convarr02 is
  type my_rec is record
    b : bit_vector;
  end record;
  type my_rec_arr is array (natural range <>) of my_rec;
  type my_rec_arr2 is array (0 to 1) of my_rec_arr;

  procedure check(v : my_rec_arr2(open)(open)(b(0 to 1))) is
  begin
    for i in v'range loop
      for j in v'element'range loop
        assert v(i)(j).b (0) = '0';
      end loop;
    end loop;
  end check;
begin
  process
    variable v2: my_rec_arr2 (open)(2 to 3)(b(1 to 2));
  begin
    check(v2);
    wait;
  end process;
end;
