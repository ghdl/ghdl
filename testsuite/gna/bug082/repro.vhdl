entity repro is
end;

architecture behav of repro is
  type t_axilite_if is record
    write_address_channel  : t_axilite_write_address_channel;
    write_data_channel     : t_axilite_write_data_channel;
    write_response_channel : t_axilite_write_response_channel;
    read_address_channel   : t_axilite_read_address_channel;
    read_data_channel      : t_axilite_read_data_channel;
  end record;
begin

  process
    variable init_if : t_axilite_if
      (  write_address_channel( awaddr( addr_width    -1 downto 0)), 
         write_data_channel(    wdata(  data_width    -1 downto 0),
                                wstrb(( data_width/8) -1 downto 0)),
         read_address_channel(  araddr( addr_width    -1 downto 0)),
         read_data_channel(     rdata(  data_width    -1 downto 0)));
  begin
    wait;
  end process;
end behav;
