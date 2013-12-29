-- module GCD where
-- 
-- 
-- $wmygcd::*Int# -> *Int# -> *Int#
-- $wmygcd ww ww1 =
--   let wild::GHC.Types.Bool = (GHC.Prim.==# ww ww1) in
--   case wild of :: *Int#
--     GHC.Types.False ->
--       let wild1::GHC.Types.Bool = (GHC.Prim.<# ww ww1) in
--       case wild1 of :: *Int#
--         GHC.Types.False -> ($wmygcd (GHC.Prim.-# ww ww1) ww1)
--         GHC.Types.True -> ($wmygcd ww (GHC.Prim.-# ww1 ww))
--     GHC.Types.True -> ww
-- 
-- mygcd::GHC.Types.Int -> GHC.Types.Int -> GHC.Types.Int
-- mygcd w w1 =
--   let w2::GHC.Types.Int = w in
--   case w2 of :: GHC.Types.Int
--     GHC.Types.I# ww::*Int# ->
--       let w3::GHC.Types.Int = w1 in
--       case w3 of :: GHC.Types.Int
--         GHC.Types.I# ww1::*Int# ->
--           let ww2::*Int# = ($wmygcd ww ww1) in
--           case ww2 of :: GHC.Types.Int DEFAULT -> (GHC.Types.I# ww2)
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.\Prim\.all;

package \GCD\ is
end \GCD\;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.\Prim\.all;
use work.\GCD\.all;

entity \$wmygcd\ is
  port (clk : in std_logic;
        s1_call : in std_logic;
        s1_ret : out std_logic;
        s1_ww : in \Int#\;
        s1_ww1 : in \Int#\;
        res : out \Int#\);
end entity;

architecture rtl of \$wmygcd\ is
  signal tail_call : std_logic;
  signal tail_ww : \Int#\;
  signal tail_ww1 : \Int#\;
  signal core_call : std_logic;
  signal core_ret : std_logic;
  signal core_ww : \Int#\;
  signal core_ww1 : \Int#\;
  signal s1_act : std_logic;
  signal s1_wait : std_logic;
  signal s1_saved_ww : \Int#\;
  signal s1_saved_ww1 : \Int#\;
begin
  process (core_call, core_ww, core_ww1)
  variable wild : \GHC.Types.Bool\;
  variable wild1 : \GHC.Types.Bool\;
  variable ww : \Int#\;
  variable ww1 : \Int#\;
  begin
    ww := core_ww;
    ww1 := core_ww1;
    wild := \GHC.Prim.==#\(ww, ww1);
    if \is_GHC.Types.False\(wild) then
      wild1 := \GHC.Prim.<#\(ww, ww1);
      if \is_GHC.Types.False\(wild1) then
        res <= \$wmygcd\(\GHC.Prim.-#\(ww, ww1), ww1);
      elsif \is_GHC.Types.True\(wild1) then
        res <= \$wmygcd\(ww, \GHC.Prim.-#\(ww1, ww));
      end if;
    elsif \is_GHC.Types.True\(wild) then res <= ww;
    end if;
  end process;
  
  process (clk)
  begin
    if rising_edge(clk) then
      core_call <= '0';
      if s1_call = '1' then
        s1_wait <= '1';
        s1_saved_ww <= s1_ww;
        s1_saved_ww1 <= s1_ww1;
      end if;
      if tail_call = '1' then
        core_call <= '1';
        core_ww <= tail_ww;
        core_ww1 <= tail_ww1;
      elsif core_ret = '1' or s1_act = '1' then
        s1_act <= '0';
        if s1_wait = '1' then
          core_call <= '1';
          s1_act <= '1';
          s1_wait <= '0';
          core_ww <= s1_saved_ww;
          core_ww1 <= s1_saved_ww1;
        elsif s1_call = '1' then
          core_call <= '1';
          s1_act <= '1';
          s1_wait <= '0';
          core_ww <= s1_ww;
          core_ww1 <= s1_ww1;
        end if;
      end if;
    end if;
  end process;
  
  s1_ret <= core_ret and s1_act;
  
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.\Prim\.all;
use work.\GCD\.all;

entity mygcd is
  port (clk : in std_logic;
        s1_call : in std_logic;
        s1_ret : out std_logic;
        s1_w : in \GHC.Types.Int\;
        s1_w1 : in \GHC.Types.Int\;
        res : out \GHC.Types.Int\);
end entity;

architecture rtl of mygcd is
  signal tail_call : std_logic;
  signal tail_w : \GHC.Types.Int\;
  signal tail_w1 : \GHC.Types.Int\;
  signal core_call : std_logic;
  signal core_ret : std_logic;
  signal core_w : \GHC.Types.Int\;
  signal core_w1 : \GHC.Types.Int\;
  signal s1_act : std_logic;
  signal s1_wait : std_logic;
  signal s1_saved_w : \GHC.Types.Int\;
  signal s1_saved_w1 : \GHC.Types.Int\;
begin
  process (core_call, core_w, core_w1)
  variable w2 : \GHC.Types.Int\;
  variable ww : \Int#\;
  variable w3 : \GHC.Types.Int\;
  variable ww1 : \Int#\;
  variable ww2 : \Int#\;
  variable w : \GHC.Types.Int\;
  variable w1 : \GHC.Types.Int\;
  begin
    w := core_w;
    w1 := core_w1;
    w2 := w;
    if \is_GHC.Types.I#\(w2) then
      \expand_GHC.Types.I#\(w2, ww);
      w3 := w1;
      if \is_GHC.Types.I#\(w3) then
        \expand_GHC.Types.I#\(w3, ww1);
        ww2 := \$wmygcd\(ww, ww1);
        res <= \GHC.Types.I#\(ww2);
      end if;
    end if;
  end process;
  
  process (clk)
  begin
    if rising_edge(clk) then
      core_call <= '0';
      if s1_call = '1' then
        s1_wait <= '1';
        s1_saved_w <= s1_w;
        s1_saved_w1 <= s1_w1;
      end if;
      if tail_call = '1' then
        core_call <= '1';
        core_w <= tail_w;
        core_w1 <= tail_w1;
      elsif core_ret = '1' or s1_act = '1' then
        s1_act <= '0';
        if s1_wait = '1' then
          core_call <= '1';
          s1_act <= '1';
          s1_wait <= '0';
          core_w <= s1_saved_w;
          core_w1 <= s1_saved_w1;
        elsif s1_call = '1' then
          core_call <= '1';
          s1_act <= '1';
          s1_wait <= '0';
          core_w <= s1_w;
          core_w1 <= s1_w1;
        end if;
      end if;
    end if;
  end process;
  
  s1_ret <= core_ret and s1_act;
  
end architecture;

