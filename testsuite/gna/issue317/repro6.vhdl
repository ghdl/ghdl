package repro6_gen_queue is
  generic (type element_type);

  procedure queue_add (el : element_type);
end repro6_gen_queue;

package body repro6_gen_queue is
  procedure queue_add (el : element_type) is
  begin
    null;
  end queue_add;
end repro6_gen_queue;

package repro6_gen_board is
  generic (type element_type);

  procedure board_add (e : element_type);
end repro6_gen_board;

package body repro6_gen_board is
  package board_queue is new work.repro6_gen_queue
    generic map (element_type => element_type);
  procedure board_add (e : element_type) is
  begin
      board_queue.queue_add(e);
  end board_add;
end repro6_gen_board;

entity repro6 is
end repro6;

architecture behav of repro6 is
  package my_board is new work.repro6_gen_board
    generic map (element_type => natural);
begin
  process
  begin
    my_board.board_add(5);
    wait;
  end process;
end behav;
