import os
from ctypes import *


class SeverityEnumeration:
    ERROR = 1
    WARNING = 2


class DiagnosticStructure(Structure):
    _fields_ = [
        ("message", c_char_p),
        ("code", c_char_p),
        ("severity", c_int),
        ("begin_offset", c_int),
        ("end_offset", c_int),
    ]


def iterdiags(diags):
    count = 0
    while diags[count].message is not None:
        yield diags[count]
        count += 1


DiagnosticStructurePointer = POINTER(DiagnosticStructure)


class ParserStructure(Structure):
    _fields_ = []


ParserStructurePointer = POINTER(ParserStructure)


def get_script_path():
    return os.path.dirname(os.path.realpath(__file__))


def set_argtypes_and_restype(func, argtypes=[], restype=None):
    func.argtypes = argtypes
    func.restype = restype


def create_library():
    lib_path = get_script_path() + "/c-api.so"
    lib = CDLL(lib_path)
    set_argtypes_and_restype(
        lib.qljs_web_demo_create_parser, restype=ParserStructurePointer
    )
    set_argtypes_and_restype(
        lib.qljs_web_demo_destroy_parser, argtypes=[ParserStructurePointer]
    )
    set_argtypes_and_restype(
        lib.qljs_web_demo_set_text,
        argtypes=[ParserStructurePointer, c_void_p, c_size_t],
    )
    set_argtypes_and_restype(
        lib.qljs_web_demo_lint,
        argtypes=[ParserStructurePointer],
        restype=DiagnosticStructurePointer,
    )
    return lib


lib = create_library()


class Parser:
    def init(self):
        self._c_parser = lib.qljs_web_demo_create_parser()
        if self._c_parser is None:
            raise MemoryError()

    def dealloc(self):
        if self._c_parser is not None:
            lib.qljs_web_demo_destroy_parser(self._c_parser)

    def set_text(self, text, count):
        lib.qljs_web_demo_set_text(self._c_parser, text, count)

    def lint(self):
        return lib.qljs_web_demo_lint(self._c_parser)

    def set_text_and_lint(self, text, count):
        self.set_text(text, count)
        return self.lint()
