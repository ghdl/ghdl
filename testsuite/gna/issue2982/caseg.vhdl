entity caseg is
  generic(variable_name            :integer:=1);
  port(port_name:		    in bit);
end;

architecture rtl of caseg is
begin    
    rxfifogen:case variable_name generate
      when b1: 0 =>
      when b2: 1 =>
      when others =>
    end generate rxfifogen;
end rtl; 
