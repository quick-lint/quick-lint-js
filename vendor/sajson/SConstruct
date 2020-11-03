import os
import multiprocessing
import sys

SetOption("num_jobs", multiprocessing.cpu_count())


def export(fn):
    Export({fn.__name__: fn})
    return fn


@export
def sajson(env):
    env.Append(CPPPATH=["#/include"])


@export
def unittestpp(env):
    env.Append(
        CPPPATH=["#/third-party/UnitTest++/src"],
        LIBPATH=["#/${BUILDDIR}/libraries"],
        LIBS=["unittestpp"],
    )


def gcc(env):
    env["CC"] = "gcc"
    env["CXX"] = "g++"


def clang(env):
    env["CC"] = "clang"
    env["CXX"] = "clang++"


def dbg(env):
    env.Append(CCFLAGS=["-g"])


def opt(env):
    env.Append(CCFLAGS=["-O2", "-g"], LINKFLAGS=["-O2", "-g"])


def san(env):
    static_opt = "-static-libasan" if env.subst("$CC") == "gcc" else None
    env.Append(
        CCFLAGS=[
            "-g",
            # ASAN & UBSAN
            "-fsanitize=address,undefined",
            # Requires newer gcc.
            # '-fsanitize=pointer-compare', '-fsanitize=pointer-subtract',
        ],
        LINKFLAGS=["-g", static_opt, "-fsanitize=address,undefined"],
    )


def m32(env):
    env.Append(CCFLAGS=["-m32"], LINKFLAGS=["-m32"])


def m64(env):
    env.Append(CCFLAGS=["-m64"], LINKFLAGS=["-m64"])


env = Environment(
    ENV=os.environ,
    CXXFLAGS=["-std=c++11", "-Wall", "-Werror", "-Wno-unused-private-field"],
)

if sys.platform == "darwin":
    builds = [
        ("clang-64-opt", [clang, m64, opt]),
        ("clang-64-dbg", [clang, m64, dbg]),
        # Not sure how to link with libasan on the travis-ci Mac image.
        # ('clang-64-san', [clang, m64, san]),
    ]
else:
    builds = [
        ("gcc-32-opt", [gcc, m32, opt]),
        ("gcc-32-dbg", [gcc, m32, dbg]),
        ("gcc-32-san", [gcc, m32, san]),
        ("gcc-64-opt", [gcc, m64, opt]),
        ("gcc-64-dbg", [gcc, m64, dbg]),
        ("gcc-64-san", [gcc, m64, san]),
        ("clang-32-opt", [clang, m32, opt]),
        ("clang-32-dbg", [clang, m32, dbg]),
        # clang m32 sanitizers don't work on ubuntu
        # ('clang-32-san', [clang, m32, san]),
        ("clang-64-opt", [clang, m64, opt]),
        ("clang-64-dbg", [clang, m64, dbg]),
        ("clang-64-san", [clang, m64, san]),
    ]

for name, tools in builds:
    e = env.Clone(tools=tools)
    e.Append(BUILDDIR=os.path.join("build", name))
    e.SConscript("SConscript", variant_dir="$BUILDDIR", duplicate=0, exports={"env": e})
