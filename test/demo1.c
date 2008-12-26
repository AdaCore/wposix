
#include <sys/stat.h>
#include <stdio.h>

void main (void)
{
  struct _stat buf;

  _stat ("toto", &buf);

  if ((buf.st_mode & S_IFDIR) == 0)
    printf ("toto is not a directory\n");
  else
    printf ("toto is a directory\n");
}
