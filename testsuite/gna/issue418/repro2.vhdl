entity repro2 is
end;

architecture behav of repro2 is
   -- AXI-Lite Interface signals
  type address_channel is record
    --DUT inputs
    awaddr  : bit_vector;
    awvalid : bit;
  end record;


  type t_if is record
    write_channel  : address_channel;
  end record;

  subtype ST_IF_32 is t_if  (
    write_channel (
        awaddr(31 downto 0)  )
    );

  signal s : st_if_32;
begin
  s.write_channel.awaddr <= x"0000_1000", x"1000_ffff" after 2 ns;
  process
  begin
    wait for 1 ns;
    assert s.write_channel.awvalid = '0';
    assert s.write_channel.awaddr(12) = '1';
    wait for 2 ns;
    assert s.write_channel.awaddr(14) = '1';
    wait;
  end process;
end;
