entity gen_file_ints is
end;

architecture behaviour of gen_file_ints is
begin
  process
    type int_file is file of integer;
    file f : int_file open write_mode is "intfile.bin";
  begin
    for i in 0 to 9 loop
      write(f, i);
    end loop;
    wait;
  end process;
end architecture behaviour;
