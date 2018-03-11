LIBRARY IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.numeric_std.ALL;

entity Shifter is
port(shift_lsl :in std_logic;
        shift_lsr :in std_logic;
        shift_asr :in std_logic;
        shift_ror :in std_logic;
        shift_rrx :in std_logic;
        cin       :in std_logic;
        shift_val :in std_logic_vector (4 downto 0);
        din       :in std_logic_vector(31 downto 0);     
        dout      :out std_logic_vector(31 downto 0);
        cout      :out std_logic;
        vdd       :in bit;
        vss       :in bit);
end Shifter;

architecture ArchiShifter of Shifter is
    signal res1, res2, res3, res4, res5, res6, res7 : std_logic_vector(31 downto 0):=x"00000000";
    signal carryOut: std_logic;

    begin
        --LSL & LSR & ASR
        res1 <= din(30 downto 0) & '0'      when shift_val(0)='1'  and             shift_lsl='1'                           else 
                '0' & din(31 downto 1)      when shift_val(0)='1'  and (shift_lsr='1' or (shift_asr='1' and din(31)='0'))  else
                '1' & din(31 downto 1)      when shift_val(0)='1'  and (shift_asr='1' and din(31)='1' )                    else din;

        res2 <= res1(29 downto 0) & "00"    when shift_val(1)='1'  and             shift_lsl='1'                           else
                "00" & res1(31 downto 2)    when shift_val(1)='1'  and (shift_lsr='1' or (shift_asr='1' and din(31)='0'))  else
                "11" & res1(31 downto 2)    when shift_val(1)='1'  and (shift_asr='1'and din(31)='1' )                     else res1;
             
        res3 <= res2(27 downto 0) & x"0"    when shift_val(2)='1'  and             shift_lsl='1'                           else 
                x"0" & res2(31 downto 4)    when shift_val(2)='1'  and (shift_lsr='1' or (shift_asr='1' and din(31)='0'))  else 
                x"F" & res2(31 downto 4)    when shift_val(2)='1'  and (shift_asr='1'and din(31)='1' )                     else res2;

        res4 <= res3(23 downto 0) & x"00"   when shift_val(3)='1'  and             shift_lsl='1'                           else 
                x"00" & res3(31 downto 8)   when shift_val(3)='1'  and (shift_lsr='1' or (shift_asr='1' and din(31)='0'))  else
                x"FF" & res3(31 downto 8)   when shift_val(3)='1'  and (shift_asr='1' and din(31)='1' )                    else res3;

        res5 <= res4(15 downto 0) & x"0000" when shift_val(4)='1'  and             shift_lsl='1'                           else 
               x"0000" & res4(31 downto 16) when shift_val(4)='1'  and (shift_lsr='1' or (shift_asr='1' and din(31)='0'))  else
               x"FFFF" & res4(31 downto 16) when shift_val(4)='1'  and (shift_asr='1' and din(31)='1' )                    else res4;
                
        carryOut <= din(32-to_integer(unsigned(shift_val))) when shift_val /="00000" and shift_lsl='1' else 
                    din(to_integer(unsigned(shift_val))-1)  when shift_val /="00000" and (shift_lsr='1' or shift_asr='1' or shift_ror='1') else
                    din(0) when shift_rrx='1' else cin; 
        --RRX
        res6 <= cin & din(31 downto 1) when shift_rrx='1' else din;

        --ROR
        res7(31 downto (32-to_integer(unsigned(shift_val)))) <= din((to_integer(unsigned(shift_val))-1) downto 0) when shift_ror='1' and shift_val /="00000" 
                                                                else din; 
        res7((31-to_integer(unsigned(shift_val))) downto 0)  <= din(31 downto to_integer(unsigned(shift_val)))    when shift_ror='1' and shift_val /="00000" 
                                                                else din;


        dout <= res5 when shift_lsl='1' or shift_asr='1' or shift_lsr='1' else
                res6 when shift_rrx='1' else
                res7 when shift_ror='1' else
                din;
        cout <= carryOut;

end ArchiShifter;
