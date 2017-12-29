/***************************************************************************
*
*   fork and execute a process.  We wait until the process terminates
*   before return to caller.
***************************************************************************/
void exec_wait(char *argstr[])
{
   int   pid,mask;
   sigset_t blockset, procset;
   struct sigaction blockaction;

   sigemptyset(&blockset); // assure that no other signals are affected
   sigemptyset(&procset);  // same assurance

   sigaddset(&blockset, SIGCLD); // add SIGCLD to the set for blocking

   sigprocmask(SIG_BLOCK, &blockset, &procset)
//   mask = sigblock(sigmask(SIGCLD));
   if ((pid = fork()) == 0)
     {
       execvp(argstr[0],argstr);
       perror(argstr[0]);
       _exit(0);
     }
   else if (pid == -1)
     {
       printf("pacman - Error forking: %s\n",argstr[0]);
     }
//   else  while(pid != unixpid) sigpause(mask);
   else  while(pid != unixpid) sigsuspend(procset);
//   sigsetmask(mask);
   sigprocmask(SIG_UNBLOCK, &blockset);
}

