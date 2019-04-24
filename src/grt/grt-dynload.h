/* Very simple wrappers for loading dll/so files.  */
#ifndef __GRT_DYNLOAD__
#define __GRT_DYNLOAD__

void * grt_dynload_open (const char *path);
void * grt_dynload_symbol (void *handle, const char *symbol);
const char *grt_dynload_error (void);

#endif /* __GRT_DYNLOAD__ */


