/* Very simple wrappers for loading dll/so files.  */
#ifndef __GRT_DYNLOAD__
#define __GRT_DYNLOAD__

/* Extension of a shared library.  */
#if defined (WINNT)
#define DSO_EXT ".dll"
#elif defined (__APPLE__)
#define DSO_EXT ".dylib"
#else
#define DSO_EXT ".so"
#endif

extern const char *dso_ext;

void * grt_dynload_open (const char *path);
void * grt_dynload_symbol (void *handle, const char *symbol);
const char *grt_dynload_error (void);

#endif /* __GRT_DYNLOAD__ */
