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

  component hex_sequencer is
    generic (
      seq : string
    );
    port (
      clk  : in  std_logic;
      data : out std_logic_vector(3 downto 0)
    );
  end component hex_sequencer;

  signal a, b : std_logic_vector(3 downto 0);

begin


  --                                  0123456789
  SEQ_A : hex_sequencer generic map ("4444444444") port map (clk, a);
  SEQ_B : hex_sequencer generic map ("4444544444") port map (clk, b);


  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- Holds
  STABLE_0 : assert always stable(a);

  -- Doesn't hold at cycle 4
  STABLE_1 : assert always stable(b);

  -- Triggers GHDL bug
  STABLE_2 : assert always stable(a(1 downto 0));
  STABLE_3 : assert always stable(b(1 downto 0));

  -- Holds
  PREV_0 : assert always a = prev(a);

  -- Doesn't hold at cycle 4
  PREV_1 : assert always b = prev(b);

  -- Triggers GHDL bug
  PREV_2 : assert always always a(1 downto 0) = prev(a(1 downto 0));
  PREV_3 : assert always always b(1 downto 0) = prev(b(1 downto 0));


end architecture psl;
