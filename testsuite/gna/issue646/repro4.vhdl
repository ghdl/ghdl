entity repro4 is
end entity;

architecture tb of repro4 is

  type channel is record
    data : bit_vector;
    ack : bit;
  end record;

  type my_bus is record
    rd : channel;
    wr : channel;
  end record;

  function init_channel (width : natural) return channel is
  begin
    return (data => (width - 1 downto 0 => '0'),
            ack => '0');
  end init_channel;

  function init_bus (width : natural) return my_bus is
  begin
    return (rd => init_channel (width),
            wr => init_channel (width));
  end init_bus;

  constant b : my_bus := init_bus (12);
begin
  assert b.rd.data(2) = '0';
end tb;


