entity repro is
end;

architecture behav of repro is
  type my_channel is record
    valid : bit;
    ack : bit;
    data : bit_vector;
  end record;

  type my_bus is record
    waddr : my_channel;
    wdata : my_channel;
  end record;

  function init_channel (fmt : my_channel) return my_channel is
  begin
    return (valid => '0',
            ack => '0',
            data => (fmt.data'range => '0'));
  end init_channel;

  function init_bus (fmt: my_bus) return my_bus is
  begin
    return (waddr => init_channel (fmt.waddr),
            wdata => init_channel (fmt.wdata));
  end init_bus;

  constant chan8 : my_channel := (valid => '0', ack => '0', data => x"a5");
begin
  process
    variable b0 : my_bus (waddr(data (7 downto 0)), wdata (data (15 downto 0)));
    variable b1 : b0'subtype;
  begin
    b1 := init_bus(b1);
    assert b1.waddr.valid = '0';
    assert b1.waddr.ack = '0';
    assert b1.waddr.data = "00000000";

    assert b1.wdata.valid = '0';
    assert b1.wdata.ack = '0';
    assert b1.wdata.data = x"0000";

    wait;
  end process;
end behav;

