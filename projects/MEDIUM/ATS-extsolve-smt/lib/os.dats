
(* ****** ****** *)

staload "./os.sats"

(* ****** ****** *)

staload
UN = "prelude/SATS/unsafe.sats"

(* ****** ****** *)

abst@ype fd = int
abst@ype file = ptr

abst@ype pid = int

typedef process_record = @{
    process= pid,
    read=file,
    write=file
}

assume process = ref(process_record)
assume pid = int
assume file = ptr

exception ProcessFailed of ()

local

  //Experimenting with a malloc interface.
  dataview alloc(a:t@ype, int, addr) =
    | {l:agz} {n:int | n > 0} success(a, n, l) of array_v(a, l, n)
    | fail(a, 0, null) of ()
    
  extern
  fun execvp {n:nat} {l:addr} (
    buf: !array_v(string, l + sizeof string, n) | cmd: string, arr: ptr l
  ): void = "mac#"
  
  extern
  fun fork(): int = "mac#"
  
  extern
  fun wait_unistd(status: &int? >> int) : void = "mac#wait"
  
  extern
  fun waitpid_unistd(
    pid: pid, status: &int? >> int, options: int
  ): void = "mac#waitpid"
  
  extern
  fun calloc{a:t@ype} {n:nat} (
    n: int n, size: size_t 
  ) : [l: addr] (alloc(a, n, l) | ptr l) = "mac#"
  
  extern
  fun free{a:t@ype} {n:nat} {l:addr} (
    pf: array_v(a, l, n) | p: ptr l
  ) : void = "mac#"
  
  extern
  prfun bytes2t0ype {a:t@ype} {l:addr} (
    bytes_v (l, sizeof(a))
  ): a @ l

  extern
  fun pipe (
    fd: &(@[int?][2]) >> @[int][2]
  ): int = "mac#"
  
  extern
  fun close (
    file: int
  ): int = "mac#"
   
  extern
  fun dup2 (
    oldfd: int, newfd: int
  ): int = "mac#"
  
  extern
  fun fopen (
    fd: int, mode: string
  ): file
  
  extern
  fun wifexited (status :int) : bool = "mac#WIFEXITED"
  
  extern
  fun wifsignaled(status :int) : bool = "mac#WIFSIGNALED"

  #define :: list_cons
  #define nil list_nil
  
  macdef stdin_fileno = $extval(int, "STDIN_FILENO")
  macdef stdout_fileno = $extval(int, "STDOUT_FILENO")
  
in

  implement 
  process_start(cmd, args) = let 
    val child = fork()
    var send : @[int][2]
    var recv : @[int][2]
    
    val r = pipe(send)
    val () = assertloc(r != ~1)
    val r = pipe(recv)
    val () = assertloc(r != ~1)
  in
    case+ child of 
      | 0 => let
          (** Set up stdin and stdout to be connected to the parent *)
          val r = dup2(send[0], stdin_fileno)
          val () = assertloc (r != ~1)
          val r = dup2(recv[1], stdout_fileno)
          val () = assertloc (r != ~1)
          
          val () = {
            val _ = close(send[1])
            val _ = close(recv[0])
          }
          val args = cmd :: args
          val len = length(args)
          val (mem | m) = calloc{string} (len + 1, sizeof<string>)
        in
          case+ m of 
            | _ when iseqz(m) => let
               prval fail() = mem
             in
               $raise ProcessFailed () 
             end
            | _ =>> exit(1) where {
              prval success(buf) = mem
              fun copy {n:nat} {l:addr} (
                buf: !array_v(string, l, n+1)
                  | lst: list(string, n), p: ptr l
              ) : void =
                case+ lst of
                  | list_cons(x, xs) => {
                      prval (y, ys) = array_v_uncons(buf)
                      val () = !p := x
                      val () = copy(ys | xs, ptr1_succ(p))
                      prval () = buf := array_v_cons(y, ys)
                  }
                  | list_nil () => ()
              val () = copy(buf | args, m)
              prval (pf, mem) = array_v_uncons(buf)
              //
              val () = execvp(mem | !m, m)
              //
              prval () = buf := array_v_cons(pf, mem)
              val () = free(buf | m)
            }
        end
      | _ when child = ~1 => $raise ProcessFailed()
      | _ =>> let
        val () = {
          (** Close unneeded file descriptors *)
          val _ = close(send[0])
          val _ = close(recv[1])
        }
        val recordsize = sizeof<process_record>
        val (mem | m) = calloc{byte}(sz2i(recordsize), sizeof<byte>)
      in
        if iseqz(m) then let
            prval fail() = mem
        in
          $raise ProcessFailed ()
        end
        else let
          prval success(buf) = mem
          prval pf = bytes2t0ype{process_record} (buf)
          
          val readfile = fopen(recv[0], "r")
          val writefile = fopen(send[1], "w")
          
          val () = assertloc (isneqz(readfile))
          val () = assertloc (isneqz(writefile))
          
          val pr = m
          val () = begin
            pr->process := child;
            pr->read := readfile;
            pr->write := writefile;
          end
        in
          ref_make_viewptr{process_record} (pf | pr)
        end
      end
  end // end of [process_start]
end
