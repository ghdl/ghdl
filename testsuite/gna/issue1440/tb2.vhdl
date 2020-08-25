library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb2 is
end;

architecture tb of tb2 is
  signal cnt, prev : std_logic_vector(3 downto 0) := (others=>'0');

  function tostr (v : std_logic_vector) return string
  is
    alias av : std_logic_vector (1 to v'length) is v;
    variable res : string (1 to v'length);
  begin
    for i in av'range loop
      case av (i) is
        when '0' => res (i) := '0';
        when '1' => res (i) := '1';
        when others => res (i) := '?';
      end case;
    end loop;
    return res;
  end tostr;
begin
    process
    begin
      for i in 0 to 20 loop
        wait for 1 ns;
        cnt <= std_logic_vector(unsigned(cnt) + 1);
        prev <= cnt;
        assert false
          report "value=" & tostr(cnt) & " last_value=" & tostr(cnt'last_value)
          severity note;
        assert prev = cnt'last_value severity failure;
      end loop;
      wait;
    end process;
end;
