package key_pkg is
    type key_action is (pressed, released);
    type key_status_mat is array (natural range <>, natural range <>) of key_action;
end key_pkg;

library IEEE;
  use IEEE.std_logic_1164.all;

use work.key_pkg.all;

entity key is
    port(
        in_terminal        : in    std_logic;
        out_terminal    : out    std_logic;
        key_status        : in    key_action);
end key;

architecture key_bhv of key is
begin
    bounce_io: process(key_status, in_terminal) is
    begin
        if key_status = pressed then
            out_terminal <= in_terminal;
        else
            out_terminal <= 'Z';
        end if;
    end process bounce_io;
end architecture key_bhv;

library IEEE;
  use IEEE.std_logic_1164.all;

use work.key_pkg.all;

entity scaleable_matrix_keyboard is
    generic(
        nr_rows : positive;
        nr_cols : positive);
    port(
        col_inputs  : in  std_logic_vector(nr_cols-1 downto 0);
        row_outputs : out std_logic_vector(nr_rows-1 downto 0);
        key_status_all : in key_status_mat(nr_rows-1 downto 0, nr_cols-1 downto 0));
end scaleable_matrix_keyboard;

architecture scaleable_matrix_keyboard_bhv of scaleable_matrix_keyboard is
begin
    key_matrix : for row in row_outputs'range generate
    begin
        key_row: for col in col_inputs'range generate
        begin
            key_col: entity work.key(key_bhv)
                port map(
                    in_terminal => col_inputs(col),
                    out_terminal => row_outputs(row),
                    key_status => key_status_all(row, col));
        end generate key_row;
    end generate key_matrix;
end architecture scaleable_matrix_keyboard_bhv;


library IEEE;
  use IEEE.std_logic_1164.all;

use work.key_pkg.all;

entity mat_key_tb is
end mat_key_tb;

architecture mat_key_tb_bhv of mat_key_tb is
    signal key_0 : key_action := released;
    signal key_1 : key_action := released;
    signal key_2 : key_action := released;
    signal key_3 : key_action := released;

    signal col1 : std_logic := 'Z';
    signal col2 : std_logic := 'Z';
    
    signal row1 : std_logic;
    signal row2 : std_logic;
begin
    key_DUT: entity work.scaleable_matrix_keyboard(scaleable_matrix_keyboard_bhv)
        generic map(
            nr_rows => 2,
            nr_cols => 2
            )
        port map(
            col_inputs(0) => col1,
            col_inputs(1) => col2,
            row_outputs(0) => row1,
            row_outputs(1) => row2,
            key_status_all(0, 0) => key_0,
            key_status_all(0, 1) => key_1,
            key_status_all(1, 0) => key_1,
            key_status_all(1, 1) => key_1
            );
end architecture mat_key_tb_bhv;
