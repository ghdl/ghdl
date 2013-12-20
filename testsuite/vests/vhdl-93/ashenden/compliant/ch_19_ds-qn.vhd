
-- Copyright (C) 1996 Morgan Kaufmann Publishers, Inc

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version. 

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details. 

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA 

-- ---------------------------------------------------------------------
--
-- $Id: ch_19_ds-qn.vhd,v 1.3 2001-10-26 16:29:36 paw Exp $
-- $Revision: 1.3 $
--
-- ---------------------------------------------------------------------

library qsim;

use qsim.qsim_types.all, random.random.all;

architecture queue_net of disk_system is

  constant disk_cache_miss_rate : real := 0.2;
  constant num_disks : positive := 2;

  constant disk_cache_fork_probabilities : probability_vector(1 to num_disks)
    := ( others => disk_cache_miss_rate / real(num_disks) );

  signal info_detail_control : info_detail_type := none;
  signal new_job, cpu_queue_in, cpu_in, cpu_out,
    quantum_expired, job_done, requesting_disk,
    disk_cache_hit, request_done : arc_type;
  signal disk_cache_miss, disk_done : arc_vector(1 to num_disks);
  signal cpu_ready : boolean;

begin

  new_jobs : entity source
    generic map ( name => "new_jobs",
		  distribution => exponential,
		  mean_inter_arrival_time => 2 sec,
		  seed => sample_seeds(1),
		  time_unit => ms,
		  info_file_name => "new_jobs.dat" )
    port map ( out_arc => new_job,
	       info_detail => info_detail_control );

  cpu_join : entity join
    generic map ( name => "cpu_join",
		  time_unit => ms,
		  info_file_name => "cpu_join.dat" )
    port map ( in_arc(1) => quantum_expired,
	       in_arc(2) => new_job,
	       in_arc(3) => request_done,
	       out_arc => cpu_queue_in,
	       info_detail => info_detail_control );

  cpu_queue : entity queue
    generic map ( name => "cpu_queue",
		  time_unit => ms,
		  info_file_name => "cpu_queue.dat" )
    port map ( in_arc => cpu_queue_in,
	       out_arc => cpu_in,
	       out_ready => cpu_ready,
	       info_detail => info_detail_control );

  cpu : entity server
    generic map ( name => "cpu",
		  distribution => uniform,
		  mean_service_time => 50 ms,
		  seed => sample_seeds(2),
		  time_unit => ms,
		  info_file_name => "cpu.dat" )
    port map ( in_arc => cpu_in,
	       in_ready => cpu_ready,
	       out_arc => cpu_out,
	       info_detail => info_detail_control );

  cpu_fork : entity fork
    generic map ( name => "cpu_fork",
		  probabilities => ( 1 => 0.5, 2 => 0.45 ),
		  seed => sample_seeds(3),
		  time_unit => ms,
		  info_file_name => "cpu_fork.dat" )
    port map ( in_arc => cpu_out,
	       out_arc(1) => quantum_expired,
	       out_arc(2) => requesting_disk,
	       out_arc(3) => job_done,
	       info_detail => info_detail_control );

  job_sink : entity sink
    generic map ( name => "job_sink",
		  time_unit => ms,
		  info_file_name => "job_sink.dat" )
    port map ( in_arc => job_done,
	       info_detail => info_detail_control );

  disk_cache_fork : entity fork
    generic map ( name => "disk_cache_fork",
		  probabilities => disk_cache_fork_probabilities,
		  seed => sample_seeds(4),
		  time_unit => ms,
		  info_file_name => "disk_cache_fork.dat" )
    port map ( in_arc => requesting_disk,
	       out_arc(1 to num_disks) => disk_cache_miss,
	       out_arc(num_disks + 1) => disk_cache_hit,
	       info_detail => info_detail_control );


  disk_array : for disk_index in 1 to num_disks generate

    constant disk_index_str : string := integer'image(disk_index);

    signal disk_in : arc_type;
    signal disk_ready : boolean;

  begin

    disk_queue : entity queue
      generic map ( name => "disk_queue_" & disk_index_str,
		    time_unit => ms,
		    info_file_name => "disk_queue_" & disk_index_str & ".dat" )
      port map ( in_arc => disk_cache_miss(disk_index),
	         out_arc => disk_in,
	         out_ready => disk_ready,
	         info_detail => info_detail_control );

    disk : entity server
      generic map ( name => "disk_" & disk_index_str,
		    distribution => exponential,
		    mean_service_time => 15 ms,
		    seed => sample_seeds(4 + disk_index),
		    time_unit => ms,
		    info_file_name => "disk_" & disk_index_str & ".dat" )
      port map ( in_arc => disk_in,
	         in_ready => disk_ready,
	         out_arc => disk_done(disk_index),
	         info_detail => info_detail_control );

  end generate disk_array;


  disk_cache_join : entity join
    generic map ( name => "disk_cache_join",
		  time_unit => ms,
		  info_file_name => "disk_cache_join.dat" )
    port map ( in_arc(1 to num_disks) => disk_done,
	       in_arc(num_disks + 1) => disk_cache_hit,
	       out_arc => request_done,
	       info_detail => info_detail_control );

end architecture queue_net;
