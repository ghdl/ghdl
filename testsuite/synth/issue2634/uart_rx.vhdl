library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;



-- Entity declaration (DO NOT ALTER THIS PART!)
entity UART_RX is
    port(
        CLK      : in std_logic;
        RST      : in std_logic;
        DIN      : in std_logic;
        DOUT     : out std_logic_vector(7 downto 0);
        DOUT_VLD : out std_logic
    );
end entity;

architecture behavioral of UART_RX is
 
begin
 process(DIN,CLK)
  variable read_bit_cnt : std_logic_vector(4 downto 0); --:="00000";
  variable byte_read_state_cnt : std_logic_vector(3 downto 0); --:="000";
  variable PRE_DOUT : std_logic_vector(7 downto 0); --:="00000000";
  variable reading_process: std_logic; --:='0';
  variable pre_reading_process: std_logic; --:='0';
  variable pre_vdl: std_logic; --:= '0';
  variable is_idle: std_logic;
   begin
    if rising_edge(CLK) then
  if RST = '1' then --reset part
    is_idle := '0';
    read_bit_cnt := "00000";
    byte_read_state_cnt:="0000";
    PRE_DOUT:="00000000";
    reading_process:='0';
    pre_reading_process:='0';
  pre_vdl:='0';
  end if;
  if is_idle = '1' then --idle process
    if DIN = '1' then
      reading_process:='0';
      pre_reading_process:='0';
      pre_vdl:='1';
    elsif DIN'event and DIN'last_value='1' and DIN = '0' then
        is_idle:='0';
        pre_vdl:='0';

    end if;
  end if;
  if DIN = '0' and reading_process = '0' and is_idle='0' then -- detecting start
    PRE_DOUT:="00000000";
       pre_vdl := '0';
      pre_reading_process := '1';
  end if;
  if pre_reading_process = '1' then -- setting offset to be in midle
     if read_bit_cnt < 24 then
        read_bit_cnt := read_bit_cnt + 1;
     else
      read_bit_cnt := "00000";
      pre_reading_process := '0';
      reading_process := '1';
    end if;
   end if;
   -- now reading the data
   if reading_process = '1' then
     if read_bit_cnt = 0 then
      case byte_read_state_cnt is
        when "0000" =>
          PRE_DOUT(0) := DIN;
        when "0001" =>
          PRE_DOUT(1) := DIN;
        when "0010" =>
          PRE_DOUT(2) := DIN;
        when "0011" =>
          PRE_DOUT(3) := DIN;
        when "0100" =>
          PRE_DOUT(4) := DIN;
        when "0101" =>
          PRE_DOUT(5) := DIN;
        when "0110" =>
          PRE_DOUT(6) := DIN;
        when "0111" =>
          PRE_DOUT(7) := DIN;
        when others =>
          -- this state should be unreachable
       end case;
      read_bit_cnt := "00001";
      byte_read_state_cnt := byte_read_state_cnt + 1;
      else
        read_bit_cnt:=read_bit_cnt + 1;
        if read_bit_cnt = 16 then
          read_bit_cnt := "00000";
        end if;
     end if;
     if byte_read_state_cnt = 8 then
       reading_process := '0';
       pre_reading_process := '0';
       byte_read_state_cnt :="0000";
       read_bit_cnt := "00000";
      is_idle:='1';
     end if;
   end if;
  
  end if ;

  DOUT_VLD <= pre_vdl;
DOUT <= PRE_DOUT;
end process ;


end architecture;
