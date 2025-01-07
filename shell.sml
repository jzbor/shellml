val stdin = (Posix.FileSys.wordToFD o SysWord.fromInt) 0;
val stdout = (Posix.FileSys.wordToFD o SysWord.fromInt) 1;
val stderr = (Posix.FileSys.wordToFD o SysWord.fromInt) 2;


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

fun pipe stream (proc1, proc2) () = let
  val { infd, outfd } = Posix.IO.pipe ();
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

fun stdoutTo procs = pipe stdout procs;
fun stderrTo procs = pipe stderr procs;
fun $> procs = stdoutTo procs;

infix stdoutTo;
infix stderrTo;
infix $>;

fun consume proc1 () = let
  val { infd, outfd } = Posix.IO.pipe ();
  fun fst (a, _) = a;
  fun snd (_, b) = b;
  fun reroute from to = (
    Posix.IO.dup2 { old = to, new = from };
    Posix.IO.close infd;
    Posix.IO.close outfd
    );
  val trConfig = { fd = infd, name = "consumerStream", initBlkMode = false };
in
  case Posix.Process.fork () of
       NONE => (reroute stdout outfd; proc1 (); Posix.Process.exit (Word8.fromInt 0))
     | SOME first_pid => (
     Posix.IO.close outfd;
     let
       val reader = Posix.IO.mkTextReader trConfig;
       val vec = CharVector.fromList [];
       val (out, stream) =  (TextIO.StreamIO.inputAll o TextIO.StreamIO.mkInstream) (reader, vec)
     in
       TextIO.StreamIO.closeIn stream;
       String.implode (CharVector.foldr (op::) [] out)
     end)
end;

fun splitLines str = String.tokens (fn c => c = #"\n") str;

fun consumeLines proc = splitLines (consume proc ());
