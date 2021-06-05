entity repro1 is
end;

architecture rtl of repro1 is
  signal v_32 : integer := 1;
  signal v_8 : integer range 0 to 255;
  signal res : integer;
  signal clk   :  bit;
begin
   process
   begin
     clk <= '0';
     for i in 1 to 5 * 2 loop
       wait for 10 ns;
       clk <= not clk;
     end loop;
     wait;
   end process;

   process (v_32) is
   begin
     report "V_32=" & integer'image (v_32);
   end process;

   process (v_8) is
   begin
     report "V_8=" & integer'image (v_8);
   end process;

   res <= v_32 + v_8;
end rtl;
