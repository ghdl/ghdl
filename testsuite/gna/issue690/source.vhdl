library ieee;
use ieee.std_logic_1164.all;

entity source is
  generic(
    type data_type;
    procedure read(l: inout std.textio.line; value: out data_type; good: out boolean);
    stm_file: string
  );
  port(
    clk: in std_logic;
    resetn: in std_logic;

    data: out data_type;
    valid: out std_logic;
    ready: in std_logic;

    valid_i: in std_logic := '1'
  );
end entity source;

architecture behav of source is

  file stimuli: std.textio.text open read_mode is stm_file;

  type packet_t is record
    data: data_type;
    valid: std_logic;
  end record;

  impure function next_packet(file stimuli: std.textio.text) return packet_t is
    variable stimuli_line: std.textio.line;
    variable packet: packet_t;
    variable good: boolean := false;
  begin
    while not std.textio.endfile(stimuli) and not good loop
      std.textio.readline(stimuli, stimuli_line);
      read(stimuli_line, packet.data, good);
    end loop;

    packet.valid := '1' when good else '0';
    
    return packet;
  end function;

  signal packet: packet_t;
  signal init: std_logic;

begin

  process(clk) is
  begin
    if rising_edge(clk) then
      if resetn = '0' then
        packet.valid <= '0';
        init <= '0';
      else
        if init = '0' or (packet.valid = '1' and valid_i = '1' and ready = '1') then
          packet <= next_packet(stimuli);
          init <= '1';
        end if;
      end if;
    end if;
  end process;

  data <= packet.data;
  valid <= packet.valid and valid_i;

end architecture behav;
