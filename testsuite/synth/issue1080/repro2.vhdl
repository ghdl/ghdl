library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity repro2 is
end repro2;

architecture behav of repro2 is
  constant ERR_COUNT : natural := 3;
  subtype byte_t is std_logic_vector(7 downto 0);

  function I2S(a: integer; l: integer) return std_logic_vector is
  begin
    return std_logic_vector(TO_UNSIGNED(a,l));
  end;

  -- Generics and arrays don't mix; use this function to index.
  constant TOTAL_MSGS : integer := ERR_COUNT + 2;
  subtype msgidx_t is integer range 0 to TOTAL_MSGS-1;

  impure function get_err_msg(n : integer) return string is
  begin
    case n is
        when  0 => return "OK";
        when  1 => return "START";
        when  2 => return "ERR_MSG00";    -- Note +2 offset
        when  3 => return "ERR_MSG01-1";
        when  4 => return "ERR_MSG02--2";
        when  5 => return "ERR_MSG03";
        when  6 => return "ERR_MSG04";
        when  7 => return "ERR_MSG05";
        when  8 => return "ERR_MSG06";
        when  9 => return "ERR_MSG07";
        when 10 => return "ERR_MSG08";
        when 11 => return "ERR_MSG09";
        when 12 => return "ERR_MSG10";
        when 13 => return "ERR_MSG11";
        when 14 => return "ERR_MSG12";
        when 15 => return "ERR_MSG13";
        when 16 => return "ERR_MSG14";
        when 17 => return "ERR_MSG15";
        when others => return "UNK";
    end case;
  end function;

  impure function get_err_len(n : msgidx_t) return integer is
    constant msg : string := get_err_msg(n);
  begin
    return msg'length;
  end function;

  -- Calculate total length of all active messages (including startup).
  impure function get_total_bytes return integer is
    constant EXTRA_CHARS : integer := 2;    -- Msg + CR + LF
    variable total : integer := 0;
  begin
    for n in 0 to TOTAL_MSGS-1 loop
        total := total + get_err_len(n) + EXTRA_CHARS;
    end loop;
    return total;
  end function;

  constant TOTAL_BYTES : integer := get_total_bytes;

  -- Define terminal newline characters (CR+LF)
  constant NEWLINE_CR : byte_t := i2s(13, 8);
  constant NEWLINE_LF : byte_t := i2s(10, 8);

  -- Create ROM array with all concatenated messages.
  type array_t is array(0 to TOTAL_BYTES-1) of byte_t;
  subtype romaddr_t is integer range 0 to TOTAL_BYTES-1;

  impure function get_msg_array return array_t is
    variable result : array_t := (others => (others => '0'));
    variable ridx   : integer := 0;

    procedure append(constant msg : string) is
    begin
        -- Append the message to the output array.
        for c in 0 to msg'length-1 loop
            result(ridx) := i2s(character'pos(msg(msg'left+c)), 8);
            ridx := ridx + 1;
        end loop;
        -- Then append the CR+LF characters.
        result(ridx+0) := NEWLINE_CR;
        result(ridx+1) := NEWLINE_LF;
        ridx := ridx + 2;
    end procedure;
  begin
    -- For each fixed message...
    for n in 0 to TOTAL_MSGS-1 loop
        append(get_err_msg(n));
    end loop;
    return result;
  end function;

  constant MESSAGE_ROM : array_t := get_msg_array;
begin
end behav;

