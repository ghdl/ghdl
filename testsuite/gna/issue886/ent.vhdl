library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package pong_pkg is
    subtype short is signed(16 downto 0);

    type direction is (UP, DOWN, NONE);
    type dimension is record
        w : short;
        h : short;
    end record;

    constant SCREEN_SIZE : dimension := (   w => to_signed(640, short'length), 
                                            h => to_signed(480, short'length) );

    type location is record
        r : short;
        c : short;
    end record;    
    
    type paddle is record
        loc : location;
        dir : direction;
    end record;    

    component paddle_mover is
        generic(
            paddle_size : dimension;
            screen_size : dimension;
            reset_loc   : location
        );
        port(
            clk : in std_logic;
            en  : in std_logic;
            rst : in std_logic;
            dir : in direction;
            q   : in paddle;
            d   : out paddle
        );
    end component;
end package;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.pong_pkg.all;

entity paddle_mover is
generic(
    paddle_size : dimension := (w => to_signed(1, short'length), h => to_signed(1 short'length));
    screen_size : dimension := (w => to_signed(1, short'length), h => to_signed(1 short'length));
    reset_loc   : location  := (r => to_signed(1, short'length), c => to_signed(1 short'length))
);
port(
    clk : in std_logic;
    en  : in std_logic;
    rst : in std_logic;
    dir : in direction;
    q   : in paddle;
    d   : out paddle
);
end entity;

architecture beh of paddle_mover is
        
    constant velocity   : short     := to_signed(1, short'length);

    signal next_candidate :paddle   := (dir => NONE, loc => (others => 1));
    signal next_paddle  : paddle    := (dir => NONE, loc => (others => 1));
    signal next_moves   : std_logic := '0';
    signal off_bottom   : std_logic := '0';
    signal off_top      : std_logic := '0';
begin
   
    next_candidate.dir <= q.dir;
    next_candidate.loc.c <= q.loc.c;

    next_candidate.loc.r <= q.loc.r + velocity when dir = DOWN else 
                            q.loc.r - velocity when dir = UP else 
                            q.loc.r;
    
    off_bottom <= (next_candidate.loc.r + paddle_size.h) >= screen_size.h;
    off_top    <= next_candidate.loc.r <= to_unsigned(0, short'length);

    next_moves <= off_bottom nor off_top;

    next_paddle.dir   <= next_candidate.dir;
    next_paddle.loc.c  <= next_candidate.loc.c;
    next_paddle.loc.r  <= next_candidate.loc.r when next_moves = '1' else q.loc.r;
    
    process(clk)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                d.loc <= reset_loc;
            elsif en = '1' then
                d <= next_paddle;
            end if;
        end if;
    end process;      
end architecture; 
