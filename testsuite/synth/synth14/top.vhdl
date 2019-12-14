library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.top_pack.all;

entity top is port (
   clk :  in std_logic;
   D   : out std_logic_vector(1 to 5));
end top;

architecture beh of top is

signal this_c : top_reg_t;
signal this_r : top_reg_t;
-- signal rst : std_logic := '0';

begin
   led : process(this_r, clk)
   variable this : top_reg_t;
   variable en : std_logic;
   begin
      this := this_r;

      en := '0';
      if this.prescale < 5000000 then en := '1'; end if;
      this.y := to_slv(this.count, this.blip, en);

      if this.prescale > 5999999 then
         this.prescale := 0;
         this.blip := '1';
         if this.count = 3 then
            this.count := 0;
         else
            this.count := this.count + 1;
         end if;
      else
         if this.prescale = 1000000 then this.blip := '0'; end if;
         this.prescale := this.prescale + 1;
      end if;

      this_c <= this;
   end process;

   led_r0 : process(clk)
   begin
     if clk = '1' and clk'event then
         this_r <= this_c;
      end if;
   end process;

   D <= this_r.y;
end beh;
