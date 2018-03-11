entity repro2 is
end;

architecture behav of repro2 is
  type t_axilite_write_address_channel is record
    awaddr  : bit_vector;
  end record;

  type t_axilite_if is record
    write_address_channel  : t_axilite_write_address_channel;
  end record;

  function get_w return natural is
  begin
    return 32;
  end get_w;
begin

  process
    constant addr_width : natural := get_w;

    variable init_if : t_axilite_if
      (  write_address_channel( awaddr( addr_width    -1 downto 0)));
  begin
    wait;
  end process;
end behav;
