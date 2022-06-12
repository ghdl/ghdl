library ieee;
   use ieee.std_logic_1164.all;
   use ieee.numeric_std.all;

library work;
   use work.log;

entity test is
end entity;

architecture tb of test is
begin
   main : process is
   begin
      wait for 7.5 ns;

      log.logger.set_level(log.TRACE);

      log.trace("TRACE");
      log.debug("DEBUG");
      log.info("INFO");
      log.warn("WARN");
      log.error("ERROR");

      std.env.finish;
   end process;
end architecture;
