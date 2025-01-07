val stdin = (Posix.FileSys.wordToFD o SysWord.fromInt) 0;
val stdout = (Posix.FileSys.wordToFD o SysWord.fromInt) 1;
val stderr = (Posix.FileSys.wordToFD o SysWord.fromInt) 2;

fun exit () = Posix.Process.exit (Word8.fromInt 0);


fun $ args () = let
  val pid_opt = Posix.Process.fork ();
  fun snd (_, b) = b;
in
  case pid_opt of
      NONE => (Posix.Process.execp (hd args, args); Posix.Process.exit (Word8.fromInt 1))
    | SOME pid => (snd o Posix.Process.waitpid) (Posix.Process.W_CHILD pid, [])
end;

fun $$ cmd () = (Posix.Process.fromStatus o OS.Process.system) cmd;

fun & (proc1, proc2) () = case Posix.Process.fork () of
                               NONE => (proc1 (); exit ())
                             | SOME pid => (proc2 ()) before
                              (Posix.Process.waitpid (Posix.Process.W_CHILD pid, []); ());

infix &;

fun pipe stream (proc1, proc2) () = let
  val { infd, outfd } = Posix.IO.pipe ();
  fun snd (_, b) = b;
  fun reroute from to = (
    Posix.IO.dup2 { old = to, new = from };
    Posix.IO.close infd;
    Posix.IO.close outfd
    );
in
  case Posix.Process.fork () of
       NONE => (reroute stdout outfd; proc1 (); exit ())
     | SOME first_pid => case Posix.Process.fork () of
                              NONE => (reroute stdin infd; proc2 (); exit ())
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


fun consume proc () = let
  val { infd, outfd } = Posix.IO.pipe ();
  fun snd (_, b) = b;
  fun reroute from to = (
    Posix.IO.dup2 { old = to, new = from };
    Posix.IO.close infd;
    Posix.IO.close outfd
    );
  val trConfig = { fd = infd, name = "consumerStream", initBlkMode = false };
in
  case Posix.Process.fork () of
       NONE => (reroute stdout outfd; proc (); exit ())
     | SOME pid => (
     Posix.IO.close outfd;
     let
       val reader = Posix.IO.mkTextReader trConfig;
       val vec = CharVector.fromList [];
       val (out, stream) =  (TextIO.StreamIO.inputAll o TextIO.StreamIO.mkInstream) (reader, vec)
     in
       TextIO.StreamIO.closeIn stream;
       Posix.Process.waitpid (Posix.Process.W_CHILD pid, []);
       String.implode (CharVector.foldr (op::) [] out)
     end)
end;

fun swallow () = case TextIO.inputLine TextIO.stdIn of
                               SOME line => swallow ()
                             | NONE      => ();

fun splitLines str = String.tokens (fn c => c = #"\n") str;

fun consumeLines proc = splitLines (consume proc ());

fun pipeline procs = let
  fun combine (next, proc) = proc $> next;
  fun init () = Posix.Process.fromStatus OS.Process.success;
in
  List.foldl combine init procs
end;

fun writeTo filename () = let
  val outStream = TextIO.openOut filename;
  fun loop () =
    case TextIO.inputLine TextIO.stdIn of
         SOME line => (TextIO.outputSubstr (outStream, Substring.full line); loop ())
       | NONE      => ()
in
  loop ();
  TextIO.closeOut outStream
end;

fun splitOut proc () = let
  val { infd, outfd } = Posix.IO.pipe ();
  fun snd (_, b) = b;
  val trConfig = {
    fd = outfd,
    name = "splitOutStream",
    initBlkMode = false,
    appendMode = false,
    chunkSize = 128
    };
  fun printToBoth outStream str = (
    TextIO.outputSubstr (TextIO.stdOut, Substring.full str);
    TextIO.StreamIO.outputSubstr (outStream, Substring.full str)
    )
  fun loop outStream () =
    case TextIO.inputLine TextIO.stdIn of
         SOME line => (printToBoth outStream line; loop outStream ())
       | NONE      => ()
  fun reroute from to = (
    Posix.IO.dup2 { old = to, new = from };
    Posix.IO.close infd;
    Posix.IO.close outfd
    );
in
  case Posix.Process.fork () of
       NONE => (reroute stdin infd; proc (); exit ())
     | SOME pid => (
       Posix.IO.close infd;
       let
         val writer = Posix.IO.mkTextWriter trConfig;
         val outStream = TextIO.StreamIO.mkOutstream (writer, IO.LINE_BUF)
       in
         loop outStream ();
         TextIO.StreamIO.closeOut outStream;
         Posix.Process.waitpid (Posix.Process.W_CHILD pid, []);
         ()
       end
     )
end;
