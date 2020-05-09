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
  signal ch    : character;

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

  ch <= seq(index);

  data <= to_bit(ch);


end architecture rtl;


library ieee;
  use ieee.std_logic_1164.all;


entity psl_next_event_e is
end entity psl_next_event_e;


architecture psl of psl_next_event_e is
  signal clk : std_logic := '0';

  component sequencer is
    generic (
      seq : string
    );
    port (
      clk  : in  std_logic;
      data : out std_logic
    );
  end component sequencer;

  signal a, b, c : std_logic;

begin

  --                              012345678901234
  SEQ_A : sequencer generic map ("_-______-______") port map (clk, a);
  SEQ_B : sequencer generic map ("___-__-___-__-_") port map (clk, b);
  SEQ_C : sequencer generic map ("______-___-____") port map (clk, c);


  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- This assertion holds
  assert_NEXT_EVENT_a : assert always (a -> next_event_e(b)[1 to 2](c));

  process
  begin
    for i in 1 to 2*20 loop
      wait for 1 ns;
      clk <= not clk;
    end loop;
    wait;
  end process;
end architecture psl;
