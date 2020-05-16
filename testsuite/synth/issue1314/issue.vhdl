library ieee;
  use ieee.std_logic_1164.all;


entity sequencer is
  generic (
    seq : string
  );
  port (
    clk  : in  std_logic;
    data : out std_logic
  );
end entity sequencer;


architecture rtl of sequencer is

  signal index : natural := seq'low;

  function to_bit (a : in character) return std_logic is
    variable ret : std_logic;
  begin
    case a is
      when '0' | '_' => ret := '0';
      when '1' | '-' => ret := '1';
      when others    => ret := 'X';
    end case;
    return ret;
  end function to_bit;

begin

  process (clk) is
  begin
    if rising_edge(clk) then
      if (index < seq'high) then
        index <= index + 1;
      end if;
    end if;
  end process;

  data <= to_bit(seq(index));

end architecture rtl;


library ieee;
  use ieee.std_logic_1164.all;


entity hex_sequencer is
  generic (
    seq : string
  );
  port (
    clk  : in  std_logic;
    data : out std_logic_vector(3 downto 0)
  );
end entity hex_sequencer;


architecture rtl of hex_sequencer is

  signal index : natural := seq'low;

  function to_hex (a : in character) return std_logic_vector is
    variable ret : std_logic_vector(3 downto 0);
  begin
    case a is
      when '0' | '_' => ret := x"0";
      when '1'       => ret := x"1";
      when '2'       => ret := x"2";
      when '3'       => ret := x"3";
      when '4'       => ret := x"4";
      when '5'       => ret := x"5";
      when '6'       => ret := x"6";
      when '7'       => ret := x"7";
      when '8'       => ret := x"8";
      when '9'       => ret := x"9";
      when 'a' | 'A' => ret := x"A";
      when 'b' | 'B' => ret := x"B";
      when 'c' | 'C' => ret := x"C";
      when 'd' | 'D' => ret := x"D";
      when 'e' | 'E' => ret := x"E";
      when 'f' | 'F' | '-' => ret := x"F";
      when others => ret := x"X";
    end case;
    return ret;
  end function to_hex;

begin

  process (clk) is
  begin
    if rising_edge(clk) then
      if (index < seq'high) then
        index <= index + 1;
      end if;
    end if;
  end process;

  data <= to_hex(seq(index));

end architecture rtl;


library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;


entity issue is
  port (
    clk : in std_logic
  );
end entity issue;


architecture psl of issue is

  component sequencer is
    generic (
      seq : string
    );
    port (
      clk  : in  std_logic;
      data : out std_logic
    );
  end component sequencer;

  component hex_sequencer is
    generic (
      seq : string
    );
    port (
      clk  : in  std_logic;
      data : out std_logic_vector(3 downto 0)
    );
  end component hex_sequencer;

  signal req, ack  : std_logic;
  signal din, dout : std_logic_vector(3 downto 0);

begin


  --                                     0123456789
  SEQ_REQ  : sequencer generic map     ("_-______-____") port map (clk, req);
  SEQ_DIN  : hex_sequencer generic map ("4433344774444") port map (clk, din);
  SEQ_ACK  : sequencer generic map     ("___-______-__") port map (clk, ack);
  SEQ_DOUT : hex_sequencer generic map ("2244333447744") port map (clk, dout);


  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- Check for two possible values of din/dout
  NEXT_EVENT_0_a : assert always ((req and din = x"4") -> next_event(ack)(dout = x"4"));
  NEXT_EVENT_1_a : assert always ((req and din = x"7") -> next_event(ack)(dout = x"7"));


  -- Check for all possible values of din/dout
  check_transfer : for i in 0 to 15 generate
    signal i_slv : std_logic_vector(din'range);
  begin
    i_slv <= std_logic_vector(to_unsigned(i, 4));
    -- Without name it works
    assert always ((req and din = i_slv) -> next_event(ack)(dout = i_slv));
    -- This errors because of similar names of all asserts
    -- ERROR: Assert `count_id(cell->name) == 0' failed in kernel/rtlil.cc:1613.
    NEXT_EVENT_a : assert always ((req and din = i_slv) -> next_event(ack)(dout = i_slv));
  end generate check_transfer;


end architecture psl;
