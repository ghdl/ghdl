library ieee;
use ieee.std_logic_1164.all;

package stream is
  constant SYNC_RESET : boolean:=true;
  subtype valid_t is std_logic;
  subtype ready_t is std_logic;
  function state_log2(x : positive) return integer;
  pure function ceil_log2(x : positive) return integer;
  function get(str : valid_t) return std_logic;
  function b2std(x:boolean) return std_logic;
end package;

package body stream is

  function b2std(x:boolean) return std_logic is
  begin
    if x then return '1';
    else      return '0';
    end if;
  end function;
  
  function get(str : valid_t) return std_logic is
  begin
    return std_logic(str);
  end get;
  
  function state_log2(x : positive) return integer is
    variable r : integer;
  begin
    if x=1 then
      r := 1;
    else
      r := ceil_log2(x);
    end if;
    return r;
  end state_log2;

  pure function ceil_log(x : positive; b : positive) return integer is
    begin
    for r in 0 to 30 loop
      if (x <=b**r) then
        return r;
      end if;
    end loop;
    return -1;
  end ceil_log;
    
  pure function ceil_log2(x : positive) return integer is
  begin
    return ceil_log(x,2);
  end ceil_log2;
  
end package body stream;

library ieee;
use ieee.std_logic_1164.all;
use work.stream.all;
use ieee.numeric_std.all;

entity tdm_counter is
generic
  (
   TDM : positive := 1
   );
port
  (reset    : in  std_logic;
   clock    : in  std_logic;
   clock_en : in  std_logic;
   valid    : in  valid_t;
   ready    : in  ready_t;
   counter  : out std_logic_vector(state_log2(TDM)-1 downto 0)
   );
end tdm_counter;

architecture rtl of tdm_counter is
  -- just declaring the counter here works
  --signal counter_u: unsigned(ceil_log2(TDM)-1 downto 0);
begin  -- rtl

  U_SIMPLE: if TDM = 1 generate
    counter <= (others=>'0');
  end generate U_SIMPLE;

  U_COMPLEX: if TDM /= 1 generate
    signal enable_i : std_logic;
    signal counter_u: unsigned(ceil_log2(TDM)-1 downto 0);
  begin
    process(reset,clock)
      -- also expanding do_reset and removing the procedure works
      procedure do_reset is
      begin
        counter_u <= to_unsigned(0, counter_u'length);
      end procedure;
    begin
      if not(SYNC_RESET) and reset='1' then
        do_reset;
      elsif rising_edge(clock) then
        if SYNC_RESET and reset='1' then
          do_reset;
        elsif clock_en='1' then
          if get(valid)='1' and get(ready)='1' then
            if enable_i='1' then
              counter_u <= to_unsigned(0, counter_u'length);
            else
              counter_u <= counter_u+1;
            end if;
          end if;
        end if;
      end if;
    end process;

    counter <= std_logic_vector(counter_u);
    enable_i <= b2std(counter_u = TDM-1) and get(valid) and get(ready);
  end generate U_COMPLEX;
end rtl;
