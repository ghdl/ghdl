#include <vpi_user.h>
#include <inttypes.h>
#include <stdio.h>

//#define STOP_ITERATION 1000000000 // Initial value
#define STOP_ITERATION 10000

uint32_t iteration = 0;


PLI_INT32 start_cb(p_cb_data);
PLI_INT32 end_cb(p_cb_data);
PLI_INT32 rw_cb(p_cb_data);
PLI_INT32 ro_cb(p_cb_data);
PLI_INT32 delay_rw_cb(p_cb_data);
PLI_INT32 delay_ro_cb(p_cb_data);

void register_cb(PLI_INT32(*f)(p_cb_data),
                 PLI_INT32 reason,
                 int64_t cycles){

    s_cb_data cbData;
    s_vpi_time simuTime;
    if (cycles < 0){
        cbData.time = NULL;
    } else {
        cbData.time = &simuTime;
        simuTime.type = vpiSimTime;
        simuTime.high = (PLI_INT32) (cycles >> 32);
        simuTime.low = (PLI_INT32) (cycles & 0xFFFFFFFF);
    }

    cbData.reason = reason;
    cbData.cb_rtn = f;
    cbData.user_data = 0;
    cbData.value = 0;

    vpi_register_cb(&cbData);
}

void entry_point_cb() {
    register_cb(start_cb, cbStartOfSimulation, -1);
    register_cb(end_cb, cbEndOfSimulation, -1);
    register_cb(delay_ro_cb, cbAfterDelay, 0);
}

PLI_INT32 start_cb(p_cb_data data){
    (void) data;
    printf("Start of simulation \n");
    return 0;
}

PLI_INT32 end_cb(p_cb_data data){
    (void) data;
    printf("End of simulation %u \n", iteration);
    return 0;
}


PLI_INT32 rw_cb(p_cb_data data){
    (void) data;
    if(iteration < STOP_ITERATION) {
        register_cb(delay_ro_cb, cbAfterDelay, 1);
    } else {
        vpi_control(vpiFinish, 0);
    }

    iteration++;
    return 0;
}

PLI_INT32 ro_cb(p_cb_data data){
    (void) data;
    register_cb(delay_rw_cb, cbAfterDelay, 0);
    return 0;
}

PLI_INT32 delay_rw_cb(p_cb_data data){
    (void) data;
    register_cb(rw_cb, cbReadWriteSynch, 0);
    return 0;
}

PLI_INT32 delay_ro_cb(p_cb_data data){
    (void) data;
    register_cb(ro_cb, cbReadOnlySynch, 0);
    return 0;
}

void (*vlog_startup_routines[]) () = {
    entry_point_cb,
    0
};
