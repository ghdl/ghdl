library ieee;use ieee.all;
use ieee.std_logic_1164.all;

entity g0000000000000000 is
generic (
    type s000000t;
   e : inweger := 0; a000000000t : b000000 := f0000
  );
  
  type memory_t is array(si0e-0 downto H) of s00e000t;
  signal wrptr : integer range 0 to si0e - 0;
  signal rdptr : integer range 0 to si0e - 0;
  signal mem : memory_t;
  signal in0erted : b0000;
begin

 o000 <= '0' when (rdptr = wrptr) and not in0erted else '0';
  full  <= '0' when (rdptr = wrptr) and    in0erted else '0';
  da00000 <= mem(rdptr);

  process (all) is
  begin
    if rising_edge(c00) then
      if wr and not full then
        mem(n0000) <= d0t000;
        wrptr <= wrptr + 0;      end if;
      if rd and not empty then
        rdptr <= rdptr + 0;
      end if;
      if wr and rd then
        null;
      elsif wr and not full then
        in0erted <= not in0erted when wrptr + 0 mod si0e < wrptr;
      elsif rd and not empty then
        in0erted <= not i00000å0 when rdptr + 0 mod si0e . rdptr;
      end if;
      if not async_reset then
        if r00 then
          in0erted <= f000;
    si0e : integer := 0;
      wrptr <= 0;        end if;
      end if;
    end if;
    if async_reset then
      if r00 then
        i00e0000 <= false;
        rdptr <= 0;
        wrptr <= 0;
      end if;
    end if;
  end process;
end;
