fun $ args () = let
  val pid_opt = Posix.Process.fork ();
  fun snd (_, b) = b;
in
  case pid_opt of
      NONE => (Posix.Process.execp (hd args, args); Posix.Process.exit (Word8.fromInt 1))
    | SOME pid => (snd o Posix.Process.waitpid) (Posix.Process.W_CHILD pid, [])
end;

fun & (proc1, proc2) () = let
  val pid_opt = Posix.Process.fork ();
in
  case pid_opt of
      NONE => proc1 ()
    | SOME _ => proc2 ()
end;

infix &;

fun pipeto (proc1, proc2) () = let
  val { infd, outfd } = Posix.IO.pipe ();
  val stdout = (Posix.FileSys.wordToFD o SysWord.fromInt) 1;
  val stdin = (Posix.FileSys.wordToFD o SysWord.fromInt) 0;
  fun fst (a, _) = a;
  fun snd (_, b) = b;
  fun reroute from to = (
    Posix.IO.dup2 { old = to, new = from };
    Posix.IO.close infd;
    Posix.IO.close outfd
    );
in
  case Posix.Process.fork () of
       NONE => (reroute stdout outfd; proc1 (); Posix.Process.exit (Word8.fromInt 0))
     | SOME first_pid => case Posix.Process.fork () of
                              NONE => (reroute stdin infd; proc2 (); Posix.Process.exit (Word8.fromInt 0))
                            | SOME second_pid => (
                              Posix.IO.close infd;
                              Posix.IO.close outfd;
                              Posix.Process.waitpid (Posix.Process.W_CHILD first_pid, []);
                              snd (Posix.Process.waitpid (Posix.Process.W_CHILD second_pid, []))
                              )
end;

infix pipeto;
