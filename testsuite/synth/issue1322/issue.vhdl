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

  signal req, busy, done : std_logic;

begin


  --                                 0123456789
  SEQ_REQ  : sequencer generic map ("_-_______") port map (clk, req);
  SEQ_BUSY : sequencer generic map ("__-_-_-__") port map (clk, busy);
  SEQ_DONE : sequencer generic map ("_______-_") port map (clk, done);


  -- All is sensitive to rising edge of clk
  default clock is rising_edge(clk);

  -- Non consecutive repetition of 3 cycles without padding
  -- busy has to hold on 3 cycles between req & done
  -- This assertion holds
  -- Not yet supported
  SERE_0_a : assert always {req} |=> {busy[->3]; done};

  -- Non consecutive repetition of 2 to 4 cycles without padding
  -- busy has to hold on 2 to 4 cycles between req & done
  -- This assertion holds
  -- Not yet supported
  SERE_1_a : assert always {req} |=> {busy[->2 to 4]; done};


end architecture psl;
