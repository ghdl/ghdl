library std;
   use std.textio.all;

package log is

   type t_level is (TRACE, DEBUG, INFO, WARN, ERROR);

   type t_logger is protected
      procedure set_level(lvl : t_level);

      procedure trace(msg : string);
      procedure debug(msg : string);
      procedure info(msg : string);
      procedure warn(msg : string);
      procedure error(msg : string);
   end protected;

   shared variable logger : t_logger;

   procedure trace(msg : string);
   procedure debug(msg : string);
   procedure info(msg : string);
   procedure warn(msg : string);
   procedure error(msg : string);

end package;

package body log is

   procedure trace(msg : string) is begin logger.trace(msg); end procedure;
   procedure debug(msg : string) is begin logger.debug(msg); end procedure;
   procedure info(msg : string) is begin logger.info(msg); end procedure;
   procedure warn(msg : string) is begin logger.warn(msg); end procedure;
   procedure error(msg : string) is begin logger.error(msg); end procedure;

   type t_logger is protected body
      variable level      : t_level := INFO;
      variable show_level : boolean := true;

      variable time_unit     : time    := ns;
      variable show_sim_time : boolean := true;

      procedure set_level(lvl : t_level) is
      begin
         level := lvl;
      end procedure;

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
         if lvl < level then return; end if;

         if show_sim_time then
            write(time_line, now, left, MAX_TIME_LEN, time_unit);
            time := time_line.all;
            trim_time(time);
         end if;

         write(output, t_level'image(lvl) & ": " & time & ": " &  msg & LF);
      end procedure;

      procedure trace(msg : string) is begin log(TRACE, msg); end procedure;
      procedure debug(msg : string) is begin log(DEBUG, msg); end procedure;
      procedure info(msg : string) is begin log(INFO, msg); end procedure;
      procedure warn(msg : string) is begin log(WARN, msg); end procedure;
      procedure error(msg : string) is begin log(ERROR, msg); end procedure;

   end protected body;

end package body;
