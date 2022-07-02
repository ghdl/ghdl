entity case4 is
end;architecture behav of case4 is
subtype bv4 is bit_vector(1 to 4);type vec0 is array(natural range<>)of bv4;constant s:vec0:=(x"0",""?="");procedure print(m:s)is
begin
end print;begin
process
begin
for i in 0 loop
case 0 is
when""=>p;end case;end loop;end process;end behav;