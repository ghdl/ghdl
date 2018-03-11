entity foo is
end entity;

architecture fum of foo is
begin
   process
   begin
       report "integer'high = " & integer'image(integer'high);
       report "16#1FFFFFFFF# = " 
            & integer'image(16#1FFFFFFFF#);
       wait;
   end process;
end architecture;
