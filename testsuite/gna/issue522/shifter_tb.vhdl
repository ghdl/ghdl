library ieee;
use ieee.std_logic_1164.all;
USE ieee.numeric_std.all;

entity Shifter_tb is
end Shifter_tb;

architecture simu of Shifter_tb is
        signal shift_lsl, shift_lsr, shift_asr, shift_ror, shift_rrx, cin : std_logic;
        signal shift_val   :std_logic_vector (4 downto 0);
        signal din         :std_logic_vector (31 downto 0);     
        signal dout        :std_logic_vector (31 downto 0);
        signal cout        :std_logic;
        signal vdd         :bit;
        signal vss         :bit;
begin

    L0: entity work.Shifter
	port map (shift_lsl, shift_lsr, shift_asr, shift_ror, shift_rrx, cin, shift_val, din, dout,cout, vdd, vss);
	process
        variable test : std_logic_vector (31 downto 0);
        constant exttest : std_logic_vector (31 downto 0) := X"FFFFFFFF";
	begin
        cin       <= '0';
        shift_val <= "00000";
        din       <= X"0FF00000";
        shift_asr <= '0';
        shift_ror <= '0';
        shift_rrx <= '0';
        shift_lsl <= '0';
        shift_lsr <= '0'; 
       	assert  dout = din report  "Variable Test = Initial Defaut"severity error;
      	wait for 1 ns;

--*************************************ASR**************************************************
            din <= X"A5A50000";
            shift_asr <= '0';
            shift_ror <= '1';
            shift_rrx <= '0';
            shift_lsl <= '0';
            shift_lsr <= '0';

            for shift in 0 to 30 looP
                shift_val <= std_logic_vector(to_unsigned(shift,5 ));
                test := din;
                wait for 1 ns;
                if shift /= 0 then
                    --"asr" équivaut à une division signé par "2^shift"
                    --l'operateur exposant n'étant pas disponible on fais une boucle qui divise par 2^shift à chaque tour de boucle   
                    for i in 0 to (shift-1) loop
                        test := std_logic_vector (shift_right(signed(test),1));
                        assert  dout = test  report "Expected Test Result  = " & integer'image(to_integer(signed(test))) & " || Dout  = " & 
                        integer'image(to_integer(signed(dout))) & " || ASR shift = " & integer'image(to_integer(unsigned(shift_val))) severity error;
                    end loop; 
                else
                    test := std_logic_vector(signed(din));      
                    assert  dout = test report "Expected Test Result  = " & integer'image(to_integer(signed(test))) & 
                    " || Dout = " & integer'image(to_integer(signed(dout))) & " ||ASR shift = " & integer'image(shift) severity error;
                end if;
                wait for 2 ns;  
            end loop;

--Test de l'extention de signe
        din       <= X"80000000";
        shift_val <= "11111";
wait for 1 ns; 
assert  dout (31 downto 0) =  exttest (31 downto 0)  report "Bit de Signe = " & integer'image ( to_integer (unsigned(din(31 downto 31)))) & " || Dout  =" & integer'image(to_integer(unsigned(dout (31 downto (31-(to_integer(unsigned(shift_val)))))))) & " || ASR shift = " & integer'image(to_integer(unsigned(shift_val))) severity error;

--*************************************LSR**************************************************
            cin <= '0';
            shift_lsr <= '1';
            shift_asr <= '0';
            shift_ror <= '0';
            shift_rrx <= '0';
       	    shift_lsl <= '0';

            for shift in 0 to 30 looP
                shift_val <= std_logic_vector(to_unsigned(shift, 5 ));
                test := din;
                wait for 1 ns;
                if shift /= 0 then
                    for i in 0 to (shift-1) loop
                        test := std_logic_vector (shift_right(unsigned(test),1));
                        assert  dout = test  report "Expected Test Result  = " & integer'image(to_integer(unsigned(test))) &  " || Dout  = " & 
                        integer'image(to_integer(unsigned(dout))) & " || LSR shift = " & integer'image(to_integer(unsigned(shift_val))) severity error;
                    end loop; 
                else
                    test := std_logic_vector(signed(din));      
                    assert  dout = test report "Expected Test Result  = " & integer'image(to_integer(unsigned(test))) & 
                    " || Dout = " & integer'image(to_integer(unsigned(dout))) & " || LSR shift = " & integer'image(shift) severity error;
                end if;
                wait for 2 ns;  
            end loop;

--*************************************LSL**************************************************
            din <= X"F000A5A5";
            shift_asr <= '0';
            shift_ror <= '0';
            shift_rrx <= '0';
            shift_lsl <= '1';
            shift_lsr <= '0';

        for z in 0 to 256 loop
            for shift in 0 to 30 loop
	            shift_val <= std_logic_vector(to_unsigned(shift,5));
                test := din;
                wait for 1 ns;
                if shift /= 0 then
                    for i in 0 to (shift-1) loop
                        test := std_logic_vector (shift_left(unsigned(test),1));
                        assert dout = test report "Expected Test Result  = " & integer'image(to_integer(unsigned(test))) & " || Dout  = " & 
                        integer'image(to_integer(unsigned(dout))) & " ||LSL shift = " & integer'image(to_integer(unsigned(shift_val))) severity error; 
                    end loop;
                else
                    test := std_logic_vector(unsigned(din));      
                    assert  dout = test report "Expected Test Result  = " & integer'image(to_integer(unsigned(test))) & 
                    " || Dout = " & integer'image(to_integer(unsigned(dout))) & " || LSR shift = " & integer'image(shift) severity error;
                end if; 
                wait for 2 ns;  
	        end loop;
        end loop;	  

--**********************************************************************************************	    
    report "end of TB";
    wait for 4 ns;
    wait;
  end process;
end simu;
