library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mwe is
    generic (
        skip : integer := 10
    );
    port (
        nreset : in std_logic;
        clk : in std_logic;
        pulse_in : in std_logic;
        pulse_out : out std_logic        
  );
end mwe;


architecture behav of mwe is
    type shared_counter is protected
        procedure reset;
        procedure increment;
        impure function value return integer;
        impure function pulsed return boolean;
    end protected shared_counter;

    type shared_counter is protected body
        variable cnt : integer range 0 to skip-1 := 0;
        variable flag_pulse : boolean := false;
        procedure reset is
        begin
            cnt := 0;
            flag_pulse := false;
        end reset;

        procedure increment is
        begin
            if cnt < skip-1 then 
                cnt := cnt + 1;
            else
                cnt := 0;
                flag_pulse := true;
                report "Pulse detected";
            end if;
        end increment;

        impure function value return integer is
        begin
            return cnt;
        end function value;

        impure function pulsed return boolean is
            variable pulsed_state : boolean;
        begin
            pulsed_state := flag_pulse;
            flag_pulse := false;
            return pulsed_state;
        end function pulsed;
    end protected body shared_counter;
    
    shared variable shrd_cnt : shared_counter;
begin

    output:process(clk,nreset)
    begin
        if nreset /= '1' then
            shrd_cnt.reset;
            pulse_out <= '0';
        elsif rising_edge(clk) then 
            pulse_out <= '0';    
            if shrd_cnt.pulsed then 
                pulse_out <= '1';    
            end if;
        end if;
    end process;

    pulse_cnt_update:process(nreset,pulse_in)
    begin
        if nreset = '1' then
            if pulse_in = '1' then 
                shrd_cnt.increment;
            end if;
        end if ;
    end process;

end behav;
