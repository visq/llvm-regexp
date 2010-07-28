#include <stdio.h>
int matcher(char* str);
int main(int argc, char ** argv)
{
  char linebuf[4096];

  linebuf[4095] = '\n';
  int r = 1;

  while(fgets(&linebuf[0],4096,stdin) != NULL)
  {
    r = r && matcher(&linebuf[0]);
    if(linebuf[4095] == '\n') {
      if(r) printf("Match !\n");
      else  printf("No match !\n");
      r = 1;      
    } else {
      linebuf[4095] = '\n';
    }
  }
  return(0);
}
