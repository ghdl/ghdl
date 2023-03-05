library IEEE; use IEEE.std_logic_1164.all;
entity to01 is 
  port (din : in std_logic; dout : out std_logic);
end;
architecture test of to01 is begin
  dout <= to_01(din);
end;
