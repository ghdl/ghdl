#if defined(__WIN32__)
#include <windows.h>
void *
grt_dynload_open (const char *path)
{
  return (void *)LoadLibrary (path);
}

void *
grt_dynload_symbol (void *handle, const char *symbol)
{
  return (void *)GetProcAddress ((HMODULE)handle, symbol);
}

const char *
grt_dynload_error (void)
{
  static char msg[256];

  FormatMessage
    (FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
     NULL,
     GetLastError (),
     MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
     (LPTSTR) &msg,
     sizeof (msg) - 1,
     NULL);
  return msg;
}

#else

#include <dlfcn.h>

void *
grt_dynload_open (const char *path)
{
  return dlopen (path, RTLD_LAZY);
}

void *
grt_dynload_symbol (void *handle, const char *symbol)
{
  return dlsym (handle, symbol);
}

const char *
grt_dynload_error (void)
{
  return dlerror ();
}
#endif
