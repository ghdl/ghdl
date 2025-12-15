entity gen_file_vec is
end;

architecture behaviour of gen_file_vec is
begin
  process
    type bv4_arr is array (natural range <>) of bit_vector(3 downto 0);
    type bv4_arr_file is file of bv4_arr;
    file f : bv4_arr_file open write_mode is "bvfile.bin";
  begin
    write(f, (x"1", x"1"));
    write(f, (x"2", x"1", x"0"));
    write(f, (x"3", x"1", x"0", x"8"));
    write(f, (x"4", x"1", x"0", x"2", x"a"));
    wait;
  end process;
end architecture behaviour;
