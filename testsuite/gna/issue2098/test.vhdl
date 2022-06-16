library std;
   use std.textio.all;

package log is

   type t_level is (TRACE, DEBUG, INFO, WARN, ERROR);

   type t_config is record
      level         : t_level;
      show_level    : boolean;
      time_unit     : time;
      show_sim_time : boolean;
      prefix        : string(1 to 32);
      separator     : string(1 to 3);
   end record;

   type t_logger is protected
      procedure set_config(c : t_config);

      procedure trace(msg : string);
      procedure debug(msg : string);
      procedure info(msg : string);
      procedure warn(msg : string);
      procedure error(msg : string);
   end protected;

   shared variable logger : t_logger;

   procedure set_config(cfg : t_config);

   procedure trace(msg : string);
   procedure debug(msg : string);
   procedure info(msg : string);
   procedure warn(msg : string);
   procedure error(msg : string);

   function config(
      level         : t_level := INFO;
      time_unit     : time := ns;
      prefix        : string(1 to 32) := (others => nul);
      separator     : string(1 to 3) := ": " & nul;
      show_level    : boolean := true;
      show_sim_time : boolean := true
   ) return t_config;

end package;

package body log is

   procedure trace(msg : string) is begin logger.trace(msg); end procedure;
   procedure debug(msg : string) is begin logger.debug(msg); end procedure;
   procedure info(msg : string) is begin logger.info(msg); end procedure;
   procedure warn(msg : string) is begin logger.warn(msg); end procedure;
   procedure error(msg : string) is begin logger.error(msg); end procedure;

   type t_logger is protected body

      variable cfg : t_config := config;

      procedure set_config(c : t_config) is begin cfg := c; end procedure;

      procedure log(lvl : t_level; msg : string) is
         constant MAX_TIME_LEN : positive := 32;
         variable time : string(1 to MAX_TIME_LEN);
         variable time_line : line;

         procedure trim_time(t : inout string) is
         begin
            for i in t'reverse_range loop
               if t(i) = ' ' then time(i) := nul; else return; end if;
            end loop;
         end procedure;
      begin
         if lvl < cfg.level then return; end if;

         if cfg.show_sim_time then
            write(time_line, now, left, MAX_TIME_LEN, cfg.time_unit);
            time := time_line.all;
            trim_time(time);
         end if;

         write(output, t_level'image(lvl) & cfg.separator & time & cfg.separator &  msg & LF);
      end procedure;


      procedure trace(msg : string) is begin log(TRACE, msg); end procedure;
      procedure debug(msg : string) is begin log(DEBUG, msg); end procedure;
      procedure info(msg : string) is begin log(INFO, msg); end procedure;
      procedure warn(msg : string) is begin log(WARN, msg); end procedure;
      procedure error(msg : string) is begin log(ERROR, msg); end procedure;

      procedure set_level(l : t_level) is
      begin
         cfg.level := l;
      end procedure;

   end protected body;

   procedure set_config(cfg : t_config) is begin logger.set_config(cfg); end procedure;

   function config(
      level         : t_level := INFO;
      time_unit     : time := ns;
      prefix        : string(1 to 32) := (others => nul);
      separator     : string(1 to 3) := ": " & nul;
      show_level    : boolean := true;
      show_sim_time : boolean := true
   ) return t_config is
      variable cfg : t_config;
   begin
      cfg.level := level;
      cfg.show_level := show_level;
      cfg.time_unit := time_unit;
      cfg.show_sim_time := show_sim_time;
      cfg.prefix := prefix;
      cfg.separator := separator;
      return cfg;
   end function;

end package body;
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
      variable l : log.t_logger;
   begin
      wait for 7.5 ns;

      log.set_config(log.config(log.TRACE));

      log.trace("TRACE");
      log.debug("DEBUG");
      log.info("INFO");
      log.warn("WARN");
      log.error("ERROR" & LF);

      l.set_config(log.config(log.TRACE));
      l.trace("TRACE");
      l.debug("DEBUG");
      l.info("INFO");
      l.warn("WARN");
      l.error("ERROR");

      std.env.finish;
   end process;
end architecture;
