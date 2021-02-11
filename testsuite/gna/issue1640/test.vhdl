package cpu_types is

  constant word_size : positive := 2147483649;
  constant address_size : positive := 24;

  subtype word is bit_vector(word_size - 1 downto 0);
  subtype address is bit_vector(address_size - 1 downto 0);

  type status_value is ( halted, idle, fetch, mem_read, mem_write,
                         io_read, io_write, int_ack );

end package cpu_types;

-- end code from book



package fg_08_01 is

  constant status :
    -- code from book
    work.cpu_types.status_value
    -- end code from book
    :=
    -- code from book
    work.cpu_types.status_value'(work.cpu_types.fetch)
    -- end code from book
    ;

end package fg_08_01;
