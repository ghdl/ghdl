library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity def_err2 is
end entity;

architecture test of def_err2 is

type SharedCounter is protected
procedure increment (N: Integer := 1);
procedure decrement (N: Integer := 1);
impure function value return Integer;
end protected SharedCounter;

type SharedCounter is protected body
variable counter: Integer := 0;
procedure increment (N: Integer := 1) is
begin
counter := counter + N;
end procedure increment;
procedure decrement (N: Integer := 1) is
begin
counter := counter - N;
end procedure decrement;
impure function value return Integer is
begin
return counter;
end function value;
end protected body;

subtype mc_t is SharedCounter;

type shared_cnt_lst is array(natural range <>) of mc_t;

begin

stim1: process

begin

wait;
end process;

end test;
