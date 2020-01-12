library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro2_1 is
  port (
    clk : std_logic;
    rst : std_logic;
    tx : out std_logic_vector(7 downto 0));
end repro2_1;

architecture behav of repro2_1 is
  subtype byte_t is std_logic_vector(7 downto 0);

  -- Define terminal newline characters (CR+LF)
  constant NEWLINE_CR : byte_t := x"0d";
  constant NEWLINE_LF : byte_t := x"0a";

  -- Create ROM array with all concatenated messages.
  type array_t is array(0 to 15) of byte_t;

  impure function get_msg_array return array_t is
    variable result : array_t := (others => (others => '0'));
    variable ridx   : integer := 0;

    procedure append(constant msg : string) is
    begin
        -- Append the message to the output array.
        for c in 0 to msg'length-1 loop
            result(ridx) := x"00";
            ridx := ridx + 1;
        end loop;
        -- Then append the CR+LF characters.
        result(ridx+0) := NEWLINE_CR;
        result(ridx+1) := NEWLINE_LF;
        ridx := ridx + 2;
    end procedure;
  begin
    -- For each fixed message...
    append("xx");
    return result;
  end function;

  constant MESSAGE_ROM : array_t := get_msg_array;
begin
  process (clk)
    variable p : natural;
  begin
    if rising_edge(clk) then
      if rst = '1' then
        p := 0;
      else
        tx <= message_rom (p);
        if p = message_rom'right then
          p := message_rom'left;
        else
          p := p + 1;
        end if;
      end if;
    end if;
  end process;
end behav;

