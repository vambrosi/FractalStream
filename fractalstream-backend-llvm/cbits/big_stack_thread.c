#include <pthread.h>
#include <stddef.h>

/* A Haskell `IO ()` action, wrapped on the Haskell side via
 * `foreign import ccall "wrapper"` into a plain `void (*)(void)` C function
 * pointer. */
typedef void (*HsThunk)(void);

struct fs_big_stack_args {
    HsThunk thunk;
};

static void *fs_big_stack_trampoline(void *arg) {
    struct fs_big_stack_args *args = (struct fs_big_stack_args *)arg;
    args->thunk();
    return NULL;
}

/* Run `thunk` on a fresh pthread with `stackSize` bytes of native stack,
 * blocking until it finishes. Used to give LLVM-JIT-compiled kernel calls
 * (whose native stack requirements can exceed GHC's default ~512KB-544KB
 * worker-thread stack on macOS) enough headroom to run without overflowing.
 *
 * This is a workaround, not a fix -- it raises the ceiling rather than
 * bounding what a kernel call actually needs. See
 * agents/big-stack-thread.md's "saddledrop native stack crash, round 2"
 * section for the real, still-open bug (unshared exp/log expansion under
 * `critical`'s double differentiation of variable-exponent powers). */
void fs_run_on_big_stack(HsThunk thunk, size_t stackSize) {
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr, stackSize);

    struct fs_big_stack_args args = { thunk };
    pthread_t t;
    pthread_create(&t, &attr, fs_big_stack_trampoline, &args);
    pthread_join(t, NULL);

    pthread_attr_destroy(&attr);
}
