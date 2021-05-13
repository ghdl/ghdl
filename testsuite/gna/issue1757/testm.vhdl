library ieee;
use ieee.std_logic_1164.all;

entity testm is

port (clk : in std_logic;
  data : in std_logic_vector(2+1 downto 0);
  q1 : out std_logic_vector(3 downto 0)
  );
end testm;

architecture rtl of testm is

-- img_log2 function
  function tlog2(d : positive) return natural is
    variable tmp : positive;
  begin
    tmp := 1;
    for count in 0 to d loop
      if (tmp >= d) then
        return count;
      end if;
      tmp := tmp*2;
    end loop;
    return d;
  end;

  constant SBITS : integer := tlog2(16);

  signal fred : std_logic_vector(SBITS - 1 downto 0);

begin

  fred <= data;

  process (fred)
  begin
    case (fred(1 downto 0)) is
      when "00" =>
        q1 <= data;

      when "01" =>
        q1 <= "0000";

      when "10" =>
        q1 <= data;

      when others  =>
        q1 <= "1111";
    end case;
  end process;
end rtl;
